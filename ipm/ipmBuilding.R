# A script to set up integral projection model

# ---- set working directory to PinonDemo ----

# --- Load packages

library('tidyverse')
library('boot')

# ---- load observed data ----
## load circle plot height data, plot distribution of observed heights
demo.data <- read.csv("cleaned_demo_data.csv")

# observed circle plot heights
# hist(demo.data$Ht)

## load estimated masting tree heights, under all 3 growth scenarios (low growth, mean growth, high growth)
Sz.est.p3 <- readRDS("demo_models/posterior_estimates/estimated_mastingtree_sizes_loci_mean_upci.rds")

# observed masting heights
# hist(Sz.est.p3)

# ---- year mapping ----

# years for survival model, here 'year' refers to the year that survival is predicted for based on size in the previous year
surv.years <- data.frame(cal.year = c(2013:2019,2022), surv.mod.yr = c(1:8))

# years for reproduction model, here 'year' 
repro.years <- data.frame(cal.year = c(1999:2024), repro.mod.yr = c(1:26))

# matching years across demo models
demo.years <- repro.years %>% full_join(., surv.years)

# ---- model parameters ----

## load parameters from survival, growth, and reproduction models

# survival
# predicts survival in the current year based on height in the previous year
# survival[t,j] ~ bernoulli(inv_logit(beta0[t] + (beta1[t]*log(previousyearheight[t,j]))))
##Thinning chains to reduce computation
thin<-sample(1:4500,500)
survival.paramsUT <- readRDS("demo_models/posterior_estimates/survival_params.rds")
survival.params<-survival.paramsUT[thin,]
# growth
# predicts size/hieght in the current year based on height in the previous year
# latent_size[t,j] ~ normal(beta0 + beta1*latent_size[t-1,j], sigp)
# observed_size[t,j] ~ normal(exp(latent_size[t,j]), sigo)
growth.paramsUT <- readRDS("demo_models/posterior_estimates/growth_params.rds")
#latent.size <- readRDS("demo_models/posterior_estimates/latent_size_growth_w_nas.rds")
growth.params<-growth.paramsUT[thin,]
# reproductons
# predicts reproduction in the current year based on 'estimated' height in the *current year* (under 3 different growth scenarios)
# cone_count[t,j,growth_scenario] ~ nbinom(exp(alphax + betax + currentyearheight(estimated)[t,j,x], phix))
repro.paramsUT <- readRDS("demo_models/posterior_estimates/repro_params.rds")
repro.params<-repro.paramsUT[thin,]

###function to create IPM
###Repro is the growth scenario use to estimate tree sizes in the past for repro model can be "mean"= mean estimated size, "lower"= lower CI estimated size, or "upper"=upper CI estimated size
#cone2seed is the average # of seeds per cone
#seed2seedling is the transition rate (germination and survival) from seed to smallest size class
MakeIPM<-function(repro,cone2seed, seed2seedling) {
  
  Sz<-exp(seq(-1.39,2,by=.01)) ##Size class cutoffs, log scale b/c models were built on log scale, but then exponetiate
  
  NSz<-length(Sz)
  
  MSz<-(Sz[-NSz]+Sz[-1])/2 ##Midpoint of classes
  #Sz[1]<-.01
  Nyears=length(c((14:20),23))# of year
  NIter=length(thin)
  ###storage for lambda of each year, iteration
  eigenout<-matrix(NA,Nyears,NIter)
  
  
  
  for (i in (1:NIter)) {
   
    #storage to create growth matrix
    growth<-matrix(NA,length(MSz),length(MSz))
    #Growth loop fill in differencing cumulative norm (a la Doak)
    for (s in (2:NSz)){
      Gupper<-pnorm(log(Sz[-1]),mean=growth.params$beta0[i]+growth.params$beta1[i]*log(MSz[s-1]), sd=growth.params$sigp[i])
      Glower<-pnorm(log(Sz[-NSz]),mean=growth.params$beta0[i]+growth.params$beta1[i]*log(MSz[s-1]), sd=growth.params$sigp[i])
      growth[,s-1]<-(Gupper-Glower)/sum(Gupper-Glower) #Normalize to prevent evictions at largest sizes
      
    } 
    #loop over years for surival and repro
    for (t in c((14:20),23)) { 
      #match with correct year in dataset 
      st<-demo.years$surv.mod.yr[t+1]
      # rt<-demo.years$repro.mod.yr[t]
      
      SurvVec<-inv.logit(survival.params[i,st]+survival.params[i,8+st]*log(MSz)) # probability of survival, plus 8 here is just to index past the beta0s
      
      if (repro=="mean") {t1=(26*1);t2=(26*4)}
      if (repro=="upper"){t1=(26*2);t2=(26*5)}
      if (repro=="lower") {t1=0;t2=(26*3)}
      
      ReproVec<-exp(repro.params[i,t1+t]+repro.params[i,t2+t]*MSz) 
      
      #Combine growth and survival
      A<-t(t(growth)*SurvVec) 
      #add in reproductive transitions
      A[1,]<-A[1,]+(ReproVec*cone2seed*seed2seedling) 
      
      #calculate lambda
      eigenout[st,i]<-round(as.numeric(eigen(A)$values[1]),digits=4)
    }
    print(i)
  }
  #calculate stochastic lambda
  return(exp(apply(log(eigenout),2,mean,na.rm=T)))
  
}

## test some values
test <- MakeIPM(repro="mean",cone2seed=10, seed2seedling=0.01)
test2 <- MakeIPM(repro="mean",cone2seed=5, seed2seedling=0.01)

hist(test)
hist(test2, xlab=expression(lambda), main = 'stochastic lambda (w/ uncertainty)')


