#include "fptrtest.h"

int addArgs (int (*functionPtr)(int, int), int a, int b)
{
	int c = a*b;
	return (*functionPtr)(a,b) + c;;
}

