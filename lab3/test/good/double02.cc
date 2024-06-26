void main () {
	// Doubles como variables
	double d ;
	d = readDouble() ;
	double d2 = readDouble() ;
	
	// Comparación de doubles
	if (d >= d2) 
		printDouble(d) ;
	else 
		printDouble(d2) ;

	// Aritmética de doubles
	printDouble(d - d2) ;
	printDouble(d + d2) ;
	printDouble(d2 - d) ;
	printDouble(d * d) ;
	printDouble(d2 / d) ; 
	printDouble(d * (++d2 )) ;
	printDouble(d2) ;

	// Post ++ y post -- de doubles
	if (d == d++) 
		printDouble(d--) ; 
	else 
		printDouble(99.9) ;
	printDouble(d) ;
}