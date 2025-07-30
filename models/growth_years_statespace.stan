// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // time / year
  int<lower=0> c; // length = 2, contains census start and end year
  matrix[y,i] Sz; //size at t
  int tcy[i,c]; // census begin/end points indexing 
}

// The parameters accepted by the model. Our model
parameters {
  // real beta0[y]; // year effect
  // real beta1[y]; // year effect
  real beta0; // w/o year effect
  real beta1; // w/o year effect
  real<lower = 0> sigp;
  real<lower = 0> sigo;
  // real beta0mu;
  // real beta1mu;
  // real<lower = 0> tausq0;
  // real<lower = 0> tausq1;
  
  matrix[y,i] Szl; //latent size at t
  
}

// model current size
model {

for(j in 1:i) {

//start year that the tree first shows up (enters the census), end the year the tree dies (leaves the census for good), fill blank matrix for first year's value
    for(t in (tcy[j,1]+1):tcy[j,2]) { 

        Szl[t,j] ~ normal(beta0 + beta1*Szl[t-1,j], sigp); // w/o year effect; NA/-99 values will be kind of crazy b/c we arent removing them here //remove log here
        
        // Szl[t,j] ~ normal(beta0[t] + beta1[t]*Szl[t-1,j], sigp); // w/ year effect; NA/-99 values will be kind of crazy b/c we arent removing them here //remove log here
        
        if(Sz[t,j]!=-99) { // in order to skip over NA's, only need to do this for the data model
        
        Sz[t,j] ~ normal(exp(Szl[t,j]), sigo); //exponentiate here

        }
      }
    }

 
  //Prior
  beta0 ~ normal(0,10); 
  beta1 ~ normal(1,10); 
  sigp ~ normal(0.05,.001) T[0,]; //  informative prior based on biologically reasonable annual growth (need to try constraining more, tried 1, then 0.1, now 0.01, now 0.001)
  sigo ~ inv_gamma(1,1); 
  
  // hyper priors
  // beta0mu ~ normal(0,10);
  // beta1mu ~ normal(1,10);
  // tausq0 ~ inv_gamma(1,1);
  // tausq1 ~ inv_gamma(1,1);
  }
  