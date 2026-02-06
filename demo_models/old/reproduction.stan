
// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // time / year
  int<lower=0> k; // growth state space model iteration OR growth scenario (low, average, high)
  // int yr[y] ; //ID for year transition in number form// year for fixed effect
  int cp[y,i]; //Response; cone production 
  matrix[i,k] Sz[y]; //array of heights across years (y) by individual (i) and process/param uncertainty carried over from growth model (k)
  
}

// MODEL 1: state space model using parameter uncertainty from the growth model

// // The parameters accepted by the model. Our model
// parameters {
//   real beta0[y];
//   real beta1[y];
//   real<lower = 0> sigs[y]; // by year, because variances it grows back in time
//   
//   real beta0mu;
//   real beta1mu;
//   real sigmu;
//   real<lower = 0> tausq0;
//   real<lower = 0> tausq1;
//   real<lower = 0> tausqs;
//   
//   matrix[y,i] tsz; //latent true size at t for individual i
//   
// }
// 
// model {
//       
//         for(j in 1:i){ // individuals
//           
//           for(t in 1:y) { // years
//             
//             if(cp[t,j]!=999) { // in order to skip over NA's
//           
//     //Sz[t,j,] ~ normal(tsz[t,j], sigs[t])T[0,]; 
//     Sz[t,j,] ~ normal(tsz[t,j], sigs[t]); // is the issue that we are forcing this to be negative when logged?? trying w/o truncation
//     
//     cp[t,j] ~ poisson(exp(beta0[t] + beta1[t]*exp(tsz[t,j]))); // exponentiate logged size
//     
//     //cp[t,j] ~ poisson(exp(beta0[t] + beta1[t]*tsz[t,j])); // unlogged size
//           }
//             
//        }
//     
//     }
//   
//   //priors
//   beta0 ~ normal(beta0mu,tausq0); 
//   beta0mu ~ normal(0,10);
//   tausq0 ~ normal(0,10)T[0,];
//   
//   beta1 ~ normal(beta1mu,tausq1);
//   beta1mu ~ normal(1,10);
//   tausq1 ~ normal(0,10)T[0,];
//   
//   // sigs ~ inv_gamma(1,1); // try sigs as fixed effect
//   sigs ~ normal(sigmu,tausqs)T[0,];
//   sigmu ~ normal(0,0.05); // (0,0.5) = no log; (0,0.05) = logged version , still issues w 0.05, try 0.01
//   tausqs ~ normal(0,1)T[0,]; // also trying 1 instead of 10 here
//   
//   }
//   

// MODEL 2: No state space model, modeling reproduction under 3 growth scenarios (low, average, high)

// The parameters accepted by the model. Our model
parameters {
  
  real alpha1[y];
  real alpha2[y];
  real alpha3[y];
  
  real beta1[y];
  real beta2[y];
  real beta3[y];
  
  real alphamu;
  real betamu;
  
  real<lower = 0> tausq0;
  real<lower = 0> tausq1;
  
}

model {
      
        for(j in 1:i){ // individuals
          
          for(t in 1:y) { // years
            
            if(cp[t,j]!=999) { // in order to skip over NA's
    
    cp[t,j] ~ poisson(exp(alpha1[t] + beta1[t]*Sz[t,j,1])); // unlogged size, under low growth conditions
    
    cp[t,j] ~ poisson(exp(alpha2[t] + beta2[t]*Sz[t,j,2])); // unlogged size, under avg growth conditions
  
    cp[t,j] ~ poisson(exp(alpha3[t] + beta3[t]*Sz[t,j,3])); // unlogged size, under high growth conditions
    
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
  
  }
