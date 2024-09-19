// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // time / year
  matrix[y,i] St; //Response size at t
  matrix[y,i] Stmin; // size at t-1 // size at t-1
}

// The parameters accepted by the model. Our model
parameters {
  real beta0[y];
  real beta1[y];
  real<lower = 0> sigma;
  real beta0mu;
  real beta1mu;
  real<lower = 0> tausq0;
  real<lower = 0> tausq1;
  
}

model {
  
  //for(t in 1:y) {
    
   // log(St[y,]) ~ normal(beta0[y] + beta1[y]*log(Stmin[y,]), sigma);
   
 // } 
 
    for(t in 1:y) {
      
      for(j in 1:i) {
        
        if(St[t,j]!=999 && Stmin[t,j]!=999) { // in order to skip over NA's
        
          log(St[t,j]) ~ normal(beta0[t] + beta1[t]*log(Stmin[t,j]), sigma);
        }
      }
    }
    
  //Prior
  beta0~normal(beta0mu,tausq0); 
  beta1~normal(beta1mu,tausq1); 
  sigma ~ inv_gamma(1,1);
  
  // hyper priors
  beta0mu ~ normal(0,10);
  beta1mu ~ normal(1,10);
  tausq0 ~ inv_gamma(1,1);
  tausq1 ~ inv_gamma(1,1);
  }