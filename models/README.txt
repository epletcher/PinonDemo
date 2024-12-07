# There are 3 STAN files in this folder:

reproduction - fits a model of reproductive output based on size (height).

survival - fits a model of survival (dead or alive) in the current year based on size (height) in the previous year

growth - fits a model of size in the current year (height) as a function of size in the previous year

# There are 3 R scripts:

ModelPrepping and ModelPreppingReproduction - these scritps take cleaned data and run the stan models listed above.

ModelPredicitons - this script takes the outputs of the model runs and extracts parameter estimates and plots the fitted models.