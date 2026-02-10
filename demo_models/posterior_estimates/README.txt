## This folder contains csv with posterior estimates for all 3 demographic models (survival, growth, and reproduction). These parameter estimates are extracted and saved to this folder in each respective 'ModelPredictions*.R' script

## survival - a model of survival (dead or alive) in the current year based on size (height) in the previous year

# survival_params.rds - table of posterior estimates

growth - a model of size in the current year (height) as a function of size in the previous year (height). this model is a state space model, so we also estimated latent, unmeasured sizes.

# growth_params.rds - table of posterior estimates

# latent_size_growth_w_nas.rds - estimated latent sizes, filtered so it DOES NOT contain estimated sizes for trees outside of the census endpoints

reproduction - fits a model of reproductive output based on estimated height (estimated using the fit growth model).

# repro_params.rds - table of posterior estimates

# estimated_mastingtreesizes_loci_mean_upci.rds - backcast estimated masting tree heights (starting 2024, back to 1999), under 3 growth scenarios lower growth, mean growth, and high growth