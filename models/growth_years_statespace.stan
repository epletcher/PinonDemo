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
  real beta0[y];
  real beta1[y];
  real<lower = 0> sigp;
  real<lower = 0> sigo;
  real beta0mu;
  real beta1mu;
  real<lower = 0> tausq0;
  real<lower = 0> tausq1;
  
  matrix[y,i] Szl; //latent size at t
  
}

// model current size
model {

for(j in 1:i) {

//start year that the tree first shows up (enters the census), end the year the tree dies (leaves the census for good), fill blank matrix for first year's value
    for(t in (tcy[j,1]+1):tcy[j,2]) { 

        log(Szl[t,j]) ~ normal(beta0[t] + beta1[t]*log(Szl[t-1,j]), sigp); // NA/-99 values will be kind of crazy b/c we arent removing them here
        
        if(Sz[t,j]!=-99) { // in order to skip over NA's, only need to do this for the data model
        
        Sz[t,j] ~ normal(Szl[t,j], sigo);

        }
      }
    }

 
  //Prior
  beta0 ~ normal(beta0mu,tausq0); 
  beta1 ~ normal(beta1mu,tausq1); 
  sigp ~ normal(0.102,0.01) T[0,]; // how do i put this on the log-scale??? do i just log the mean and variance for sig p that we calculated using the raw data? informative prior based on biologically reasonable annual growth
  sigo ~ inv_gamma(1,1); 
  
  // hyper priors
  beta0mu ~ normal(0,10);
  beta1mu ~ normal(1,10);
  tausq0 ~ inv_gamma(1,1);
  tausq1 ~ inv_gamma(1,1);
  }