# A script to set up integral projection model

## load circle plot height data, plot distribution of observed heights

## load parameters from survival, growth, and reproduction models

# survival
# predicts survival in the current year based on height in the previous year
# survival[t,j] ~ bernoulli(inv_logit(beta0[t] + (beta1[t]*log(previousyearheight[t,j]))))

# growth
# predicts size/hieght in the current year based on height in the previous year
# 