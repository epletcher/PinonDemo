
// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // time / year
  int<lower=0> k; // growth state space model iteration OR growth scenario (low, average, high)
  // int yr[y] ; //ID for year transition in number form// year for fixed effect
  int cp[y,i]; //Response; cone production 
  matrix[i,k] Sz[y]; //array of heights across years (y) by individual (i) and process/param uncertainty carried over from growth model (k)
  
}


// MODEL 2: No state space model, modeling reproduction under 3 growth scenarios (low, average, high)

// The parameters accepted by the model. Our model
parameters {
  
  real alpha1[y];
  real alpha2[y];
  real alpha3[y];
  
  real beta1[y];
  real beta2[y];
  real beta3[y];
  
  real beta1_2[y];
  real beta2_2[y];
  real beta3_2[y];
  
  real alphamu;
  real betamu;
  real beta2mu;
  
  real<lower = 0> tausq0;
  real<lower = 0> tausq1;
  real<lower = 0> tausq2;
  
}

model {
      
        for(j in 1:i){ // individuals
          
          for(t in 1:y) { // years
            
            if(cp[t,j]!=999) { // in order to skip over NA's
    
    cp[t,j] ~ poisson(exp(alpha1[t] + beta1[t]*Sz[t,j,1] + beta1_2[t]*Sz[t,j,1]^2)); // unlogged size, under low growth conditions
    
    cp[t,j] ~ poisson(exp(alpha2[t] + beta2[t]*Sz[t,j,2] + beta2_2[t]*Sz[t,j,2]^2)); // unlogged size, under avg growth conditions
  
    cp[t,j] ~ poisson(exp(alpha3[t] + beta3[t]*Sz[t,j,3] + beta3_2[t]*Sz[t,j,3]^2)); // unlogged size, under high growth conditions
    
          }
            
       }
    
    }
  
  //priors
  alpha1 ~ normal(alphamu,tausq0);
  alpha2 ~ normal(alphamu,tausq0);
  alpha3 ~ normal(alphamu,tausq0);
  
  alphamu ~ normal(0,10);
  tausq0 ~ normal(0,10)T[0,];
  
  beta1 ~ normal(betamu,tausq1);
  beta2 ~ normal(betamu,tausq1);
  beta3 ~ normal(betamu,tausq1);
  
  betamu ~ normal(1,10);
  tausq1 ~ normal(0,10)T[0,];
  
  beta1_2 ~ normal(beta2mu,tausq2);
  beta2_2 ~ normal(beta2mu,tausq2);
  beta3_2 ~ normal(beta2mu,tausq2);
  
  beta2mu ~ normal(1,10);
  tausq2 ~ normal(0,10)T[0,];
  
  }
