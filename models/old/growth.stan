// The input data
data {
  int<lower=0> i; // individual
  vector[i] St2; //Response; size at last time step
  vector[i] St1; // size at first timestep
  vector[i] G1; // growth from size at t-1 to size at t
}

// The parameters accepted by the model. Our model
parameters {
  real beta0;
  real beta1;
  real<lower = 0> sigma;
  real beta0mu;
  real beta1mu;
  real<lower = 0> tausq0;
  real<lower = 0> tausq1;
  
}

model {
      
      for(j in 1:i) {
        
        if(St2[j]!=999 && St1[j]!=999) { // in order to skip over NA's
        
          log(G1[j]) ~ normal(beta0 + beta1*log(St1[j]), sigma);
          
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