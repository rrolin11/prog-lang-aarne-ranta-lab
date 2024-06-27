void main () {
	int a = 0 ; 
	while( a < 6 ){
		printInt(fib(a)) ;
		a++ ;
	}
}

int fib ( int n ) {
	if (n < 2) {
		return n ;
	} else {
		return fib( n - 1 ) + fib( n - 2 ) ;
	}
}