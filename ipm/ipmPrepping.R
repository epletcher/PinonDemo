# A script to set up integral projection model

# ---- set working directory to PinonDemo ----

# ---- observed data ----
## load circle plot height data, plot distribution of observed heights
demo.data <- read.csv("cleaned_demo_data.csv")

# observed circle plot heights
hist(demo.data$Ht)

## load estimated masting tree heights, under all 3 growth scenarios (low growth, mean growth, high growth)
Sz.est.p3 <- readRDS("demo_models/posterior_estimates/estimated_mastingtree_sizes_loci_mean_upci.rds")

# observed masting heights
hist(Sz.est.p3)

# ---- model parameters ----

## load parameters from survival, growth, and reproduction models

# survival
# predicts survival in the current year based on height in the previous year
# survival[t,j] ~ bernoulli(inv_logit(beta0[t] + (beta1[t]*log(previousyearheight[t,j]))))
survival.params <- readRDS("demo_models/posterior_estimates/survival_params.rds")

# growth
# predicts size/hieght in the current year based on height in the previous year
# latent_size[t,j] ~ normal(beta0 + beta1*latent_size[t-1,j], sigp)
# observed_size[t,j] ~ normal(exp(latent_size[t,j]), sigo)
growth.params <- readRDS("demo_models/posterior_estimates/survival_params.rds")
latent.size <- readRDS("demo_models/posterior_estimates/latent_size_growth_w_nas.rds")

# reproductons
# predicts reproduction in the current year based on 'estimated' height in the *current year* (under 3 different growth scenarios)
# cone_count[t,j,growth_scenario] ~ nbinom(exp(alphax + betax + currentyearheight(estimated)[t,j,x], phix))
repro.params <- readRDS(repro.params, "demo_models/posterior_estimates/repro_params.rds")
