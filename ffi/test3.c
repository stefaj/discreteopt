#include <ilcplex/cplex.h>
#include <math.h>
#include <stdio.h>


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

    status = CPXsetintparam(env, CPXPARAM_ScreenOutput, CPX_ON);
    if(status)
    {
      fprintf(stderr, "Failure to turn on indicator, %f",status);
    }

    status = CPXsetintparam(env, CPXPARAM_Read_DataCheck, CPX_ON);
    if(status)
    {
      fprintf(stderr, "Failure to turn on indicator, %f",status);
    }

//    double obj[] = {1,3,1,1,1,0,0,0};
//    double rhs[] = {4.0,0.0,0.0,0.0,0.0,0.0};
//    char sense[] = {'E','L','L','L','L','L'};
//    int matbeg[] = {0,3,5,7,9,11};
//    int matcnt[] = {0,0,0,0,0,0,0,0};
//    int matind[] = {5,6,7,0,5,1,6,2,5,3,7,4,7};
//    double matval[] = {1,1,1,-1,1,-1,1,-1,1,-1,1,-1,1};
//    double lb[] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
//    double ub[] = {CPX_INFBOUND,CPX_INFBOUND,CPX_INFBOUND,CPX_INFBOUND,CPX_INFBOUND,
//                  CPX_INFBOUND,CPX_INFBOUND,CPX_INFBOUND};
//    double rngval[] = {0.0,0.0,0.0,0.0,0.0,0.0};

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

//[1.0,3.0,1.0,1.0,1.0]
//[4.0,0.0,0.0,0.0,0.0,0.0]
//[69,76,76,76,76,76]
//[0,1,2,3,4,5,8,10]
//[1,1,1,1,1,3,2,3]
//[1,2,3,4,5,3,1,0,2,0,5,4,0]
//[-1.0,-1.0,-1.0,-1.0,-1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
//[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
//[1.1e20,1.1e20,1.1e20,1.1e20,1.1e20,1.1e20,1.1e20,1.1e20]
//[0.0,0.0,0.0,0.0,0.0,0.0]



 for(int i = 0; i < 8; i++)
    {
//     lb[i] = 0.0;
//     ub[i] = CPX_INFBOUND;
    }

   // obj[0] = 1.0;
   // obj[1] = 3.0;
   // obj[2] = 1.0;
   // obj[3] = 1.0;
   // obj[4] = 1.0;
   // obj[5] = 0.0;
   // obj[6] = 0.0;
   // obj[7] = 0.0;
  //  matbeg[0] = 0;
//    matind[0] = 5;    matind[1] = 6;     matind[2] = 7;
//    matval[0] = 1.0;  matval[1] = 1.0;   matval[2] = 1.0;   sense[0] = 'E';     rhs[0] = 4.0;
   // matbeg[1] = 3;
 //   matind[3] = 0;     matind[4] = 5;    
//    matval[3] = -1.0;  matval[4] = 1.0;   sense[1] = 'L';     rhs[1] = 0.0;
  //  matbeg[2] = 5;
//    matind[5] = 1;     matind[6] = 6;    
//    matval[5] = -1.0;  matval[6] = 1.0;   sense[2] = 'L';     rhs[2] = 0.0;
   // matbeg[3] = 7;
//    matind[7] = 2;     matind[8] = 5;    
//    matval[7] = -1.0;  matval[8] = 1.0;   sense[3] = 'L';     rhs[3] = 0.0;
 //   matbeg[4] = 9;
//    matind[9] = 3;     matind[10] = 7;    
//    matval[9] = -1.0;   matval[10] = 1.0;   sense[4] = 'L';     rhs[4] = 0.0;
  //  matbeg[5] = 11;
//    matind[11] = 4;     matind[12] = 7;    
//    matval[11] = -1.0;   matval[12] = 1.0;   sense[5] = 'L';     rhs[5] = 0.0;











   // status = CPXnewcols (env, lp, 8, obj, lb, ub, NULL, NULL);

    //status = CPXaddrows (env, lp, 0, 6, 13, rhs, sense, matbeg, matind, matval, NULL, NULL);

    status = CPXcopylp (env, lp, 8, 6, CPX_MIN, obj,
                    rhs, sense, matbeg,matcnt, matind,
                    matval, lb, ub, rngval);

    printf("%f\n", status);
    status = CPXlpopt (env, lp);
   
    double x[8];
    status = CPXsolution (env, lp, &solstat, &objval, x, NULL, NULL, NULL);
    printf("Solution status %f\n",status);
    printf ("\nSolution status = %d\n", solstat);
    printf ("Solution value = %f\n", objval);
    printf ("Solution= [%f, %f, %f, %f, %f, %f, %f, %f]\n\n",x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7]);

    printf("This is great stuff\n");


    return 0;
 }
