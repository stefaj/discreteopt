#include <ilcplex/cplex.h>
#include <math.h>
#include <stdio.h>

#define NUMROWS 6
#define NUMCOLS 8
#define NUMNZ 13


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
    char ctype[NUMCOLS];

    int solstat;
    double objval;

    env = CPXopenCPLEX (&status);


    //CPXsetintparam(env, CPX_PARAM_MIPCBREDLP, CPX_OFF);
    //CPXsetintparam(env, CPX_PARAM_PRELINEAR, CPX_OFF);
    /* Turn on traditional search for use with control callbacks */

    // status = CPXsetintparam (env, CPXPARAM_MIP_Strategy_Search,
    //                        CPX_MIPSEARCH_TRADITIONAL);


    lp = CPXcreateprob(env, &status, "lpex1");
    CPXchgprobtype(env, lp, CPXPROB_MILP);

    CPXchgobjsen (env, lp, CPX_MIN);

    for(int i = 0; i < NUMCOLS; i++)
    {
      lb[i] = 0.0;
      ub[i] = CPX_INFBOUND;
    }

    obj[0] = 1.0;
    obj[1] = 3.0;
    obj[2] = 1.0;
    obj[3] = 1.0;
    obj[4] = 1.0;
    obj[5] = 0.0;
    obj[6] = 0.0;
    obj[7] = 0.0;
    status = CPXnewcols (env, lp, NUMCOLS, obj, lb, ub, NULL, NULL);

    rmatbeg[0] = 0;
    rmatind[0] = 5;     rmatind[1] = 6;     rmatind[2] = 7;
    rmatval[0] = 1.0;  rmatval[1] = 1.0;   rmatval[2] = 1.0;   sense[0] = 'E';     rhs[0] = 4.0;

    rmatbeg[1] = 3;
    rmatind[3] = 0;     rmatind[4] = 5;    
    rmatval[3] = -1.0;   rmatval[4] = 1.0;   sense[1] = 'L';     rhs[1] = 0.0;
    
    rmatbeg[2] = 5;
    rmatind[5] = 1;     rmatind[6] = 6;    
    rmatval[5] = -1.0;   rmatval[6] = 1.0;   sense[2] = 'L';     rhs[2] = 0.0;
  
    rmatbeg[3] = 7;
    rmatind[7] = 2;     rmatind[8] = 5;    
    rmatval[7] = -1.0;   rmatval[8] = 1.0;   sense[3] = 'L';     rhs[3] = 0.0;
  
    rmatbeg[4] = 9;
    rmatind[9] = 3;     rmatind[10] = 7;    
    rmatval[9] = -1.0;   rmatval[10] = 1.0;   sense[4] = 'L';     rhs[4] = 0.0;
  
    rmatbeg[5] = 11;
    rmatind[11] = 4;     rmatind[12] = 7;    
    rmatval[11] = -1.0;   rmatval[12] = 1.0;   sense[5] = 'L';     rhs[5] = 0.0;


    ctype[0] = 'I'; ctype[1] = 'I'; ctype[2] = 'I';
    ctype[3] = 'I'; ctype[4] = 'I'; ctype[5] = 'C';
    ctype[6] = 'C'; ctype[7] = 'C'; 

    status = CPXaddrows (env, lp, 0, NUMROWS, NUMNZ, rhs, sense, rmatbeg, rmatind, rmatval, NULL, NULL);
    status = CPXcopyctype (env, lp, ctype);

    status = CPXmipopt (env, lp);



   /* Write the output to the screen. */
    // solstat = CPXgetstat (env, lp);
    // printf ("\nSolution status = %d\n", solstat);
    // status = CPXgetobjval (env, lp, &objval);
    // printf ("Solution value  = %f\n\n", objval);

    status = CPXsolution (env, lp, &solstat, &objval, x, NULL, NULL, NULL);
    printf ("\nSolution status = %d\n", solstat);
    printf ("Solution value = %f\n", objval);
    printf ("Solution= [%f, %f, %f, %f, %f, %f, %f, %f]\n\n",x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7]);

    printf("This is great stuff\n");


    return 0;
 }
