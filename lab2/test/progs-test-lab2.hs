{-# OPTIONS_GHC -cpp #-}

-- GHC needs -threaded

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.IO.Unsafe

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

debug :: String -> IO ()
debug s = do d <- readIORef doDebug
             if d then putStrLn s else return ()


listGoodProgs = listCCFiles "good"

listBadProgs = listCCFiles "bad"

listCCFiles dir = 
    liftM (map (\f -> joinPath [dir,f]) . sort . filter ((=="cc") . getExt)) $ getDirectoryContents dir


welcome :: IO ()
welcome = do putStrLn $ "This is the test program for Programming Languages Lab 2"


runMake :: FilePath -> IO ()
runMake dir = do checkDirectoryExists dir
                 runCommandNoFail_ ("make -C " ++ quote dir) ""

runTests :: FilePath -> IO ([Bool],[Bool])
runTests dir = 
#ifdef mingw32_HOST_OS  
    do let prog = joinPath [dir,"lab2.exe"]
#else
    do let prog = joinPath [dir,"lab2"]
#endif    
       checkFileExists prog
       goodProgs <- listGoodProgs
       badProgs  <- listBadProgs
       good <- mapM (testFrontendProg prog True)  goodProgs
       bad  <- mapM (testFrontendProg prog False) badProgs
       return (good,bad)


testFrontendProg :: FilePath -> Bool -> FilePath -> IO Bool
testFrontendProg prog good f =
    do let bad = not good
           c = prog ++ " " ++ f
       putStrLn $ "Checking " ++ f ++ "..."
       (out,err,s) <- runCommandStrWait c ""
       debug $ "Exit code: " ++ show s
       case lines out of
           "OK":_ | good         -> do return True
                  | bad          -> do reportError c "passed BAD program" f "" out err
                                       return False
           "TYPE ERROR":_ 
                  | good      -> do reportError c "" f "" out err
                                    return False
                  | bad       -> do --reportErrorColor green c "failed bad program" f "" out err
                                       return True
           _                     -> do reportError c "invalid output" f "" out err
                                       return False

--
-- * Main
--

parseArgs :: [String] -> IO String
parseArgs ["-debug",cfFile] = 
    do writeIORef doDebug True
       return cfFile
parseArgs [cfFile] = return cfFile
parseArgs _ = do hPutStrLn stderr "Usage: progs-test-lab2 <type checker code directory>"
                 exitFailure

mainOpts :: FilePath -> IO ()
mainOpts dir = 
    do welcome
       runMake dir
       (good,bad) <- runTests dir
       putStrLn ""
       putStrLn "------------------------------------------------------------"
       report "Good programs: " good
       report "Bad programs:  " bad

main :: IO ()
main = getArgs >>= parseArgs >>= mainOpts

--
-- * List utilities
--

grep :: String -> String -> [String]
grep x = filter (x `isSubStringOf`) . lines
  where isSubStringOf x = any (x `isPrefixOf`) . tails

--
-- * Path name utilities
--

getExt :: FilePath -> String
getExt = reverse . takeWhile (/='.') . reverse

stripExt :: FilePath -> String
stripExt p = if '.' `elem` p then p' else p
  where p' = reverse $ drop 1 $ dropWhile (/='.') $ reverse p

basename :: FilePath -> FilePath
basename = reverse . takeWhile (not . isPathSep) . reverse

isPathSep :: Char -> Bool
isPathSep c = c == pathSep

joinPath :: [String] -> FilePath
joinPath = concat . intersperse [pathSep]

pathSep :: Char
#if defined(mingw32_HOST_OS)
pathSep = '\\'
#else
pathSep = '/'
#endif

quote :: FilePath -> FilePath
quote p = "\"" ++ concatMap f p ++ "\""
  where 
    f '\'' = "\\'"
    f c = [c]

--
-- * Either utilities
--

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

fromLeft :: Either a b -> a
fromLeft =  either id (error "fromLeft: Right")

catLefts :: [Either a b] -> [a]
catLefts xs = [x | Left x <- xs]

--
-- * Terminal output colors
--

type Color = Int

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

highlight = "\ESC[7m"
bold      = "\ESC[1m"
underline = "\ESC[4m"
normal    = "\ESC[0m"
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"
bgcol col = "\ESC[0" ++ show (40+col) ++ "m"

red, green, blue, black :: Color
black = 0
red = 1
green = 2
blue = 4

--
-- * Various versions of runCommand
--

runCommandStr :: String -- ^ command
        -> String -- ^ stdin data
        -> IO (String,String,ProcessHandle) -- ^ stdout, stderr, process
runCommandStr c inStr = 
    do
    outVar <- newEmptyMVar
    errVar <- newEmptyMVar
    (pin,pout,perr,p) <- runInteractiveCommand c
    forkIO $ do debug "Writing input..."
                hPutStr pin inStr
                hClose pin
                debug "Wrote input."
    forkIO $ do debug "Reading output..."
                s <- hGetContents pout
                putMVar outVar s
                debug "Read output."
    forkIO $ do debug "Reading error..."
                s <- hGetContents perr
                putMVar errVar s
                debug "Read error."
    out <- takeMVar outVar
    err <- takeMVar errVar
    return (out,err,p)

runCommandStrWait :: String -- ^ command
		  -> String -- ^ stdin data
		  -> IO (String,String,ExitCode) -- ^ stdout, stderr, process exit status
runCommandStrWait c inStr =
    do
    debug $ "Running " ++ c
    (out,err,p) <- runCommandStr c inStr
    s <- waitForProcess p
    debug $ "Standard output:\n" ++ out
    debug $ "Standard error:\n" ++ err
    return (out,err,s)

runCommandNoFail_ :: String -- ^ Command
                  -> FilePath -- ^ Input file
                  -> IO ()
runCommandNoFail_ c f = runCommandNoFail c f >> return ()

runCommandNoFail :: String -- ^ Command
                 -> FilePath -- ^ Input file
                 -> IO (String,String) -- ^ stdout and stderr
runCommandNoFail e f = 
    do
    let c = e ++ " " ++ f
    hPutStrLn stderr $ "Running " ++ c ++ "..."
    (out,err,s) <- runCommandStrWait c ""
    case s of
	   ExitFailure x -> do
			    reportError e ("with status " ++ show x) f "" out err
			    exitFailure
	   ExitSuccess -> return (out,err)

--
-- * Checking files and directories
--

checkFileExists :: FilePath -> IO ()
checkFileExists f =
    do e <- doesFileExist f
       when (not e) $ do putStrLn $ color red $ quote f ++ " is not an existing file."
		         exitFailure

checkDirectoryExists :: FilePath -> IO ()
checkDirectoryExists f =
    do e <- doesDirectoryExist f
       when (not e) $ do putStrLn $ color red $ quote f ++ " is not an existing directory."
		         exitFailure

--
-- * Error reporting and output checking
--

reportErrorColor :: Color 
                 -> String -- ^ command that failed
	         -> String -- ^ how it failed
	         -> FilePath -- ^ source file
	         -> String -- ^ given input
	         -> String -- ^ stdout output
	         -> String -- ^ stderr output
	         -> IO ()
reportErrorColor col c m f i o e =
    do
    putStrLn $ color col $ c ++ " failed: " ++ m
    when (not (null f)) $ prFile f
    when (not (null i)) $ do
			  putStrLn "Given this input:"
			  putStrLn $ color blue $ i
    when (not (null o)) $ do
			  putStrLn "It printed this to standard output:"
			  putStrLn $ color blue $ o
    when (not (null e)) $ do
			  putStrLn "It printed this to standard error:"
			  putStrLn $ color blue $ e

reportError :: String -- ^ command that failed
	    -> String -- ^ how it failed
	    -> FilePath -- ^ source file
	    -> String -- ^ given input
	    -> String -- ^ stdout output
	    -> String -- ^ stderr output
	    -> IO ()
reportError = reportErrorColor red

prFile :: FilePath -> IO ()
prFile f = do
           e <- doesFileExist f           
           when e $ do putStrLn $ "For input file " ++ f ++ ":"
	               putStrLn $ "---------------- begin " ++ f ++ " ------------------" 
	               s <- readFile f
	               putStrLn $ color green s
	               putStrLn $ "----------------- end " ++ f ++ " -------------------" 


-- | Report how many tests passed.
report :: String -> [Bool] -> IO ()
report n rs = 
  do let (p,t) = (length (filter id rs), length rs)
         c = if p == t then green else red
     putStrLn $ color c $ 
              n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
