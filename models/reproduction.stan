// ** note this is model is not set up to be run yet **

// The input data
data {
  int<lower=0> i; // individual
  int cp[i]; //Response; cone production 
  vector[i] Sz; // height 2024
}

// The parameters accepted by the model. Our model
parameters {
  real beta0;
  real beta1;
  
}

model {
      // ** need add iteration across years too **
        for(j in 1:i){
          
          if(Sz[j]!=999 && cp[j]!=999) { // in order to skip over NA's
          
    cp[j] ~ poisson(exp(beta0 + beta1*Sz[j])); 
    
          }
    
    }
  
  //priors
  beta0 ~ normal(0,1); 
  beta1 ~ normal(0,1);
  
  }
