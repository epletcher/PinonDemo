// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // time / year
  matrix[y,i] Sz; //size at t
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
  
  
}

// model current size
model {

//declare objects
matrix[y,i] Szl; //latent size at t


    for(t in 2:y) { //start year 2, fill blank matrix for first year's value

      for(j in 1:i) {

        if(Sz[t,j]!=999 && Sz[t-1,j]!=999) { // in order to skip over NA's, I'm a little confesued by 

        log(Szl[t,j]) ~ normal(beta0[t] + beta1[t]*log(Szl[t-1,j]), sigp);
        
        Sz[t,j] ~ normal(Szl[t,j], sigo);

        }
      }
    }

 
  //Prior
  beta0~normal(beta0mu,tausq0); 
  beta1~normal(beta1mu,tausq1); 
  sigp ~ inv_gamma(1,1);
  sigo ~ inv_gamma(1,1); // we will want to give this an informative prior based on biologically reasonable annual change is size, as a percent of the size
  
  // hyper priors
  beta0mu ~ normal(0,10);
  beta1mu ~ normal(1,10);
  tausq0 ~ inv_gamma(1,1);
  tausq1 ~ inv_gamma(1,1);
  }