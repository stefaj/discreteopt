CPLEX_DIR = /opt/ibm/ILOG/CPLEX_Studio_Community1263/cplex
lib=$(CPLEX_DIR)/lib/x86-64_linux/static_pic/
CPLEX_INC = $(CPLEX_DIR)/include
sources = test.c
executable = test


COPT = -m64 -fPIC -fno-strict-aliasing
CLNFLAGS = -lcplex -lm -lpthread

CFLAGS = $(COPT)  -I$(CPLEX_INC) -L$(lib)


# export LIBRARY_PATH=$(lib) \
# export INCLUDE=$(include) \
# export CPATH=$(include)

CC = gcc -O0


all: 	clean
	test

test:
	@echo Building...
	$(CC) -DIL_STD  $(CFLAGS) $(sources) -o $(executable) $(CLNFLAGS)
	@echo Running...
	./$(executable)

clean:
	rm $(executable)
