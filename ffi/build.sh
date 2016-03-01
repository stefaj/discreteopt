lib=/opt/ibm/ILOG/CPLEX_Studio_Community1263/cplex/lib/x86-64_linux/static_pic/lib
include=/opt/ibm/ILOG/CPLEX_Studio_Community1263/cplex/include
sources = test.c
executable = test

all: test

test:	
	export LIBRARY_PATH=$lib
	export INCLUDE=$include
	export CPATH=$include
	echo "Building..."
	gcc -DIL_STD -L($include) -I($lib) -lcplex -lm -lpthread $sources $executable
	echo "Running..."
	./$executable
