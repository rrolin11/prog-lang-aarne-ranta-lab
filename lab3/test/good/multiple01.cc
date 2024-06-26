void main () {
	int i ;
	i = fun();
	printInt(i) ;
	printInt(fun()) ;
}

int fun() {
	return 1 ;
}
