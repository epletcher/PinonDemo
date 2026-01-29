# This folder contains csv with posterior estimates for all 3 demographic models (survival, growth, and reproduction). These parameter estimates are extracted and saved to this folder in each respective 'ModelPredictions.R' script

survival - a model of survival (dead or alive) in the current year based on size (height) in the previous year

growth - a model of size in the current year (height) as a function of size in the previous year (height)

reproduction - fits a model of reproductive output based on estimated height (estimated using the fit growth model).