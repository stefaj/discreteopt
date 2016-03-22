CPLEX_DIR = /opt/ibm/ILOG/CPLEX_Studio_Community1263/cplex
lib=$(CPLEX_DIR)/lib/x86-64_linux/static_pic/
CPLEX_INC = $(CPLEX_DIR)/include

sources1 = test.c
executable1 = test

sources2 = test2.c
executable2 = test2

sources2 = test4.c
executable2 = test4

sources3 = test3.c
executable3 = test3

sources4 = test4.c
executable4 = test4

sources5 = test5.c
executable5 = test5


sources6 = test6.c
executable6 = test6

COPT = -m64 -fPIC -fno-strict-aliasing
CLNFLAGS = -lcplex -lm -lpthread

CFLAGS = $(COPT)  -I$(CPLEX_INC) -L$(lib)


# export LIBRARY_PATH=$(lib) \
# export INCLUDE=$(include) \
# export CPATH=$(include)

CC = gcc -O0


all: 	clean
	test


test:   $(sources1) 
	@echo Building...
	$(CC) -DIL_STD  $(CFLAGS) $(sources1) -o $(executable1) $(CLNFLAGS)
	@echo Running...
	./$(executable1)



test2:  $(sources2) 
	@echo Building...
	$(CC) -DIL_STD  $(CFLAGS) $(sources2) -o $(executable2) $(CLNFLAGS)
	@echo Running...
	./$(executable2)


test3: $(sources3) 
	@echo Building...
	$(CC) -DIL_STD  $(CFLAGS) $(sources3) -o $(executable3) $(CLNFLAGS)
	@echo Running...
	./$(executable3)

test4: $(sources4)
	@echo Building...
	$(CC) -DIL_STD  $(CFLAGS) $(sources4) -o $(executable4) $(CLNFLAGS)
	@echo Running...
	./$(executable4)

test5: $(sources5)
	@echo Building...
	$(CC) -DIL_STD  $(CFLAGS) $(sources5) -o $(executable5) $(CLNFLAGS)
	@echo Running...
	./$(executable5)



test6: $(sources6)
	@echo Building...
	$(CC) -DIL_STD  $(CFLAGS) $(sources6) -o $(executable6) $(CLNFLAGS)
	@echo Running...
	./$(executable6)

clean:
	rm $(executable1)
	rm $(executable2)
	rm $(executable3)
	rm $(executable4)
