// The input data
data {
  int<lower=0> i; // individual
  int<lower=0> y; // time / year
  matrix[y,i] St; //Response size at t
  matrix[y,i] Stmin; // size at t-1
  matrix[y,i] G; // growth from size at t-1 to size at t
}

// The parameters accepted by the model. Our model
parameters {
  //real nu; // DF parameter for student t's distribution that allows for fatter tails
  real beta0[y];
  real beta1[y];
  real<lower = 0> sigma;
  real beta0mu;
  real beta1mu;
  real<lower = 0> tausq0;
  real<lower = 0> tausq1;
  
}

// //model current size
model {

    for(t in 1:y) {

      for(j in 1:i) {

        if(St[t,j]!=999 && Stmin[t,j]!=999) { // in order to skip over NA's

        log(St[t,j]) ~ normal(beta0[t] + beta1[t]*log(Stmin[t,j]), sigma); // normal

         // log(St[t,j]) ~ student_t(nu, beta0[t] + beta1[t]*log(Stmin[t,j]), sigma); // student's t distribution

        }
      }
    }

// // model actual growth
// 
// model {
// 
//     for(t in 1:y) {
//       
//       for(j in 1:i) {
//         
//         if(G[t,j]!=999 && Stmin[t,j]!=999) { // in order to skip over NA's
//         
//           // log(G[t,j]) ~ normal(beta0[t] + beta1[t]*log(Stmin[t,j]), sigma); // log normal
//           
//           log(G[t,j]) ~ student_t(nu, beta0[t] + beta1[t]*log(Stmin[t,j]), sigma); // student's t distribution
//           
//         }
//       }
//     }

  //Prior
  //nu~normal(0,10); // student's t
  beta0~normal(beta0mu,tausq0); 
  beta1~normal(beta1mu,tausq1); 
  sigma ~ inv_gamma(1,1);
  
  
  // hyper priors
  beta0mu ~ normal(0,10);
  beta1mu ~ normal(1,10);
  tausq0 ~ inv_gamma(1,1);
  tausq1 ~ inv_gamma(1,1);
  }