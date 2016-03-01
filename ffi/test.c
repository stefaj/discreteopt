#include <ilcplex/cplex.h>
#include <math.h>
#include <stdio.h>

#define NUMROWS 2
#define NUMCOLS 3
#define NUMNZ 6

 int main(int argc, char **argv)
 {
    int status = 0;
    CPXENVptr env = NULL;
    CPXLPptr lp = NULL;

    double obj[NUMCOLS];
    double lb[NUMCOLS];
    double ub[NUMCOLS];
    double x[NUMCOLS];
    int rmatbeg[NUMROWS];
    int rmatind[NUMNZ];
    double rmatval[NUMNZ];
    double rhs[NUMROWS];
    char sense[NUMROWS];

    int solstat;
    double objval;

    env = CPXopenCPLEX (&status);
    if(env==NULL)
    {
        //ERROR
    }
    lp = CPXcreateprob(env, &status, "lpex1");
    if(lp == NULL)
    {
        // ERROR
    }

    CPXchgobjsen (env, lp, CPX_MAX);

    lb[0] = 0.0;
    ub[0] = 40.0;

    lb[1] = 0.0;
    ub[1] = CPX_INFBOUND;

    lb[2] = 0.0;
    ub[2] = CPX_INFBOUND;

    obj[0] = 1.0;
    obj[1] = 2.0;
    obj[2] = 3.0;

    status = CPXnewcols (env, lp, NUMCOLS, obj, lb, ub, NULL, NULL);

    rmatbeg[0] = 0;
    rmatind[0] = 0;     rmatind[1] = 1;     rmatind[2] = 2;
    rmatval[0] = -1.0;  rmatval[1] = 1.0;   rmatval[2] = 1.0;   sense[0] = 'L';     rhs[0] = 20.0;

    rmatbeg[1] = 3;
    rmatind[3] = 0;     rmatind[4] = 1;     rmatind[5] = 2;
    rmatval[3] = 1.0;   rmatval[4] = -3.0;  rmatval[5] = 1.0;   sense[1] = 'L';     rhs[1] = 30.0;

    status = CPXaddrows (env, lp, 0, NUMROWS, NUMNZ, rhs, sense, rmatbeg, rmatind, rmatval, NULL, NULL);
    status = CPXlpopt (env, lp);

    status = CPXsolution (env, lp, &solstat, &objval, x, NULL, NULL, NULL);

    printf ("\nSolution status = %d\n", solstat);
    printf ("Solution value = %f\n", objval);
    printf ("Solution= [%f, %f, %f]\n\n", x[0], x[1], x[2]);

    printf("This is great stuff\n");
    return 0;
 }
