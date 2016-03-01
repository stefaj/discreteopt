lib=/opt/ibm/ILOG/CPLEX_Studio_Community1263/cplex/lib/x86-64_linux/static_pic/lib/
include=/opt/ibm/ILOG/CPLEX_Studio_Community1263/cplex/include/
sources = test.c
executable = test

all: test

test:	
	export LIBRARY_PATH=$(lib); export INCLUDE=$(include); export CPATH=$(include);	gcc -DIL_STD -L$(include) -I$(lib) -lm -lpthread $(sources) -o $(executable)
	echo "Running..."
	./$(executable)
