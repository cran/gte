#include <R.h>
#include <Rmath.h>

/*################################################################## 
  # This  code is written  in C  software to  calculate the  indicator 
  #          matrix  which is recalled  by the  main program. 
  ################################################################## */
	
  void  GetAlpha (double  *lr, double  *l, double  *r, int  *m, int *n, 
                  double  *alpha)
  { 
    int  i,j; 
    for(i=0;i<*n;i++){ 
      for(j=0;j<*m;j++){ 
        if((l[i]<=lr[j])  &&  (r[i]>=lr[j+1])){ 
          alpha[i*(*m)+j]=1; 
        } else{ 
          alpha[i*(*m)+j]=0; 
        } 
      } 
    } 
  } 

/*################################################################## 
  # This  code is written  in C software  to calculate  the  GT estimator 
  #             which  is recalled  by the main  program. 
  ################################################################## */

  void St2  (double *Alpha1,  double  *w, int *n,  int *m, int  *itermax, 
             double  *tol, double  *p, double  *eps)
	{ 
    int  i,j,k; 
    int  iter=0; 
    double  sum=0; 
    double  p0; 
    double  p1[*m]; 
    double  maxi=0; 
    double  diffe; 

    while((iter<*itermax)&&(*eps>*tol)){ 
			maxi=0; 
      for(j=0;j<*m;j++){ 
				p1[j] = p[j]; 
      } 
      for(j=0;j<*m;j++){ 
				p0 =  0; 
        for(i=0;i<*n;i++){ 
          sum  = 0; 
          for(k=0;k<*m;k++){ 
             sum  = sum + p1[k]*Alpha1[i*(*m)+k]; 
          } 
          if(sum >0)  { p0 = p0  + w[i]*Alpha1[i*(*m)+j]*p1[j]/sum;} 
        } 
        p[j] = p0; 
        diffe = fabs(p[j]-p1[j]); 

        if(diffe>maxi){ 
          maxi = diffe; 
          *eps =  diffe; 
        } 
      } 
      iter = iter + 1; 
     } 
  } 
