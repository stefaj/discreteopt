#include <ilcplex/cplex.h>
#include <math.h>
#include <stdio.h>


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


        double x[3];
        CPXgetcallbacknodex (env, cbdata, wherefrom, x, 0, 3-1);

        printf("\n");
        for(int i = 0; i < 8; i++)
        {
            char xs[10];

            sprintf(xs,"%f, ", x[i]);
            printf("%s", xs);
        }
        printf("\n");

        int cmatbeg[1] = {0};
        int cmatind[3] = {0,1,2};
        double cmatval[3] = {0,1,0};
        char csense[1] = {'L'};
        double crhs[1] = {20};

        if(x[2] > 20)
        {
            //status = CPXcutcallbackadd(env, cbdata, wherefrom, 3, 20, 'L', cmatind, cmatval, CPX_USECUT_FORCE);
        }
        if ( status ) {
          fprintf (stderr, "Some fucking error, status = %d.\n", status);
        }

        return 0;
    }



 int main(int argc, char **argv)
 {
    int status = 0;
    CPXENVptr env = NULL;
    CPXLPptr lp = NULL;


    int solstat;
    double objval;

    env = CPXopenCPLEX (&status);

    lp = CPXcreateprob(env, &status, "lpex1");
    CPXchgobjsen (env, lp, CPX_MIN);

    // status = CPXsetintparam(env, CPXPARAM_ScreenOutput, CPX_ON);
    // if(status)
    // {
    //   fprintf(stderr, "Failure to turn on indicator, %f",status);
    // }

    // status = CPXsetintparam(env, CPXPARAM_Read_DataCheck, CPX_ON);
    // if(status)
    // {
    //   fprintf(stderr, "Failure to turn on indicator, %f",status);
    // }


    status = CPXsetlazyconstraintcallbackfunc (env, callback,
                                      NULL);



    double obj[] = {1,3,1,1,1,0,0,0};
    double rhs[] = {4.0,0.0,0.0,0.0,0.0,0.0};
    char sense[] = {'E','L','L','L','L','L'};
    int matbeg[] = {0,1,2,3,4,5,8,10};
    int matcnt[] = {1,1,1,1,1,3,2,3};
    int matind[] = {1,2,3,4,5,3,1,0,2,0,5,4,0};
    double matval[] = {-1.0,-1.0,-1.0,-1.0,-1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0};
    double lb[] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
    double ub[] = {1.1e20,1.1e20,1.1e20,1.1e20,1.1e20,1.1e20,1.1e20,1.1e20};
    double rngval[] = {0.0,0.0,0.0,0.0,0.0,0.0};


 for(int i = 0; i < 8; i++)
    {
//     lb[i] = 0.0;
//     ub[i] = CPX_INFBOUND;
    }


    status = CPXcopylp (env, lp, 8, 6, CPX_MIN, obj,
                    rhs, sense, matbeg,matcnt, matind,
                    matval, lb, ub, rngval);

    
    char ctype[] = {'I','I','I','I','I','C','C','C'};
    status = CPXcopyctype (env, lp, ctype);

    status = CPXmipopt (env, lp);
   
    double x[8];
    status = CPXsolution (env, lp, &solstat, &objval, x, NULL, NULL, NULL);
    printf("Solution status %f\n",status);
    printf ("\nSolution status = %d\n", solstat);
    printf ("Solution value = %f\n", objval);
    printf ("Solution= [%f, %f, %f, %f, %f, %f, %f, %f]\n\n",x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7]);

    printf("This is great stuff\n");


    return 0;
 }
