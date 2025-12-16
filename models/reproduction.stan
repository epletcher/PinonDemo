
// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // time / year
  int<lower=0> k; // growth state space model iteration
  int cp[y,i]; //Response; cone production 
  matrix[i,k] Sz[y]; //array of heights across years (y) by individual (i) and process/param uncertainty carried over from growth model (k)
  
}

// The parameters accepted by the model. Our model
parameters {
  real beta0[y];
  real beta1[y];
  
  real<lower = 0> sigs[y]; // by year, because variances it grows back in time
  
  matrix[y,i] tsz; //latent true size at t for individual i
  
}

model {
      
        for(j in 1:i){ // individuals
          
          for(t in 1:y) { // years
            
            if(cp[t,j]!=999) { // in order to skip over NA's
          
    Sz[t,j,] ~ normal(tsz[t,j], sigs[t])T[0,];    
    
    cp[t,j] ~ poisson(exp(beta0[t] + beta1[t]*tsz[t,j])); 
    
          }
            
       }
    
    }
  
  //priors
  beta0 ~ normal(0,10); 
  beta1 ~ normal(0,10);
  sigs ~ normal(0,10)T[0,]; 
  }
