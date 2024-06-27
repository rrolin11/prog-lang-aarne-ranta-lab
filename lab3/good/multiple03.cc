void main () {
	int a, b ;
	a = readInt() ;
	b = readInt() ;
	printString("El maximo entre " + i2Str(a) + " y " + i2Str(b) + " es " + i2Str(max(a, b))) ;
}

int max(int x, int y) {
	return y ; // To test if order of variables is correct (x should be 1 and y should be 2)
	// if (x > y) {
	// 	return x ; 
	// } else {
	// 	return y ; 
	// }
}