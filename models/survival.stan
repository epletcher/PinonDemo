// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // year
  int Surv[y,i]; //Response (survival)
  matrix[y,i] Stmin; // size at t-1
}

// parameters (betas)
parameters {
  real beta0[y];
  real beta1[y];
  
  // hyper parameters
  real<lower = 0> tausq0; 
  real<lower = 0> tausq1;
  real beta0mu;
  real beta1mu;
}

model {
  
  for(t in 1:y) {
      
      for(j in 1:i) {
        
        if(Surv[t,j]!=999 && Stmin[t,j]!=999) { // in order to skip over NA's
    
          Surv[t,j] ~ bernoulli(inv_logit(beta0[t] + (beta1[t]*log(Stmin[t,j]))));
          
        }
        
      }
    
  }
    
  //Prior
  beta0~normal(beta0mu,tausq0); 
  beta1~normal(beta1mu, tausq1);
  
  // hyper priors
  beta0mu ~ normal(0,10);
  beta1mu ~ normal(0,10);
  tausq0 ~ inv_gamma(1,1);
  tausq1 ~ inv_gamma(1,1);
  }