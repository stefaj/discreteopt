#include <ilcplex/cplex.h>
#include <math.h>
#include <stdio.h>

#define NUMROWS 2
#define NUMCOLS 3
#define NUMNZ 6


/*The incumbent callback is used to reject integer feasible solutions that do not meet additional restrictions the user may wish to impose.
The user-written callback will be called each time a new incumbent solution has been found, including when solutions are provided by the user’s heuristic callback routine.
The user callback routine is called with the new solution as input.
Depending on the API, the callback function changes an argument or invokes a method to indicate whether or not the new solution should replace the incumbent solution.*/

/*int callback (CPXCENVptr env,
              void *cbdata,
              int wherefrom,
              void *cbhandle,
              double objval,
              double *x,
              int *isfeas_p,
              int *useraction_p)*/
// lazy constaint callback 
int callback (CPXCENVptr env,
            void *cbdata,
            int wherefrom,
            void *cbhandle,
            int *useraction_p)
    {
        int status;
        CPXLPptr lp = malloc(0);
        status = CPXgetcallbacknodelp (env, cbdata, wherefrom, &lp);


      fprintf(stdout,"lazy: ");

        // char s[50];
        // sprintf(s,"%f", objval);
        // printf("%s\n", s);

        double x[3];
        CPXgetcallbacknodex (env, cbdata, wherefrom, x, 0, 3-1);

        printf("\n");
        for(int i = 0; i < 3; i++)
        {
            char xs[10];

            sprintf(xs,"%f, ", x[i]);
            printf("%s", xs);
        }
        printf("\n");

        // if(x[0] > 20)
        // {
        //     x[0]--;
        //     *isfeas_p = 0;
        // }


        int cmatbeg[1] = {0};
        int cmatind[3] = {0,1,2};
        double cmatval[3] = {0,1,0};
        char csense[1] = {'L'};
        double crhs[1] = {20};
        // CPXaddusercuts doesnt work for some fucking reason

        if(x[2] > 20)
        {
            //*isfeas_p = 0;
            //status = CPXaddlazyconstraints(env, lp, 1, 3, crhs, csense, cmatbeg, cmatind, cmatval, NULL );
            status = CPXcutcallbackadd(env, cbdata, wherefrom, 3, 20, 'L', cmatind, cmatval, CPX_USECUT_FORCE);
        }
        //
        if ( status ) {
          fprintf (stderr, "Some fucking error, status = %d.\n", status);
        }

        //*useraction_p = CPX_CALLBACK_SET;

        return 0;
    }

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


    CPXsetintparam(env, CPX_PARAM_MIPCBREDLP, CPX_OFF);
    CPXsetintparam(env, CPX_PARAM_PRELINEAR, CPX_OFF);
    /* Turn on traditional search for use with control callbacks */

    // status = CPXsetintparam (env, CPXPARAM_MIP_Strategy_Search,
    //                        CPX_MIPSEARCH_TRADITIONAL);


    lp = CPXcreateprob(env, &status, "lpex1");
    //CPXchgprobtype(env, lp, CPXPROB_MILP);

    CPXchgobjsen (env, lp, CPX_MAX);

    status = CPXsetlazyconstraintcallbackfunc (env, callback,
                                      NULL);


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

    ctype[0] = 'I'; ctype[1] = 'C'; ctype[2] = 'I';

    // status = CPXaddusercuts (env, lp, cutcnt, cutnzcnt, cutrhs,
    //                      cutsense, cutbeg, cutind, cutval, NULL);
    //status = CPXaddusercuts(env, lp, NUMROWS, NUMNZ, rhs, sense, rmatbeg, rmatind, rmatval, NULL );

    status = CPXaddrows (env, lp, 0, NUMROWS, NUMNZ, rhs, sense, rmatbeg, rmatind, rmatval, NULL, NULL);
    status = CPXcopyctype (env, lp, ctype);

    // cuts
    int cmatbeg[1] = {0};
    int cmatind[3] = {0,1,2};
    double cmatval[3] = {1,0,0};
    char csense[1] = {'L'};
    double crhs[1] = {20};

    //CPXaddusercuts doesnt work for some fucking reason

    //status = CPXaddlazyconstraints(env, lp, 1, 3, crhs, csense, cmatbeg, cmatind, cmatval, NULL );
    if ( status ) {
      fprintf (stderr, "Some fucking error, status = %d.\n", status);
   }



    status = CPXmipopt (env, lp);



   /* Write the output to the screen. */
    // solstat = CPXgetstat (env, lp);
    // printf ("\nSolution status = %d\n", solstat);
    // status = CPXgetobjval (env, lp, &objval);
    // printf ("Solution value  = %f\n\n", objval);

    status = CPXsolution (env, lp, &solstat, &objval, x, NULL, NULL, NULL);
    printf ("\nSolution status = %d\n", solstat);
    printf ("Solution value = %f\n", objval);
    printf ("Solution= [%f, %f, %f]\n\n", x[0], x[1], x[2]);

    printf("This is great stuff\n");


    return 0;
 }
