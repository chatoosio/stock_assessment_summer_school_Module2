#
# Day 2 Author Chato Osio

#  Surplus Production in Continuous-Time (SPICT)
library(spict)

# Data Structure


# Build spict stock

# Let's work with a good time series for Anchovy in GSA 6.
# What is available: 
  # catch since 1945
  # biomass index since 2002
  # effort index for Purse seine in kw*Days and fishing days


# Load the .csv file

ane <- read.csv("~/GitHub/stock_assessment_summer_school_Module2/Material/stocks_for_course/ANE_6_notAge/ANE_06_nonAge.csv")

# Assemble stock

ane6 <- vector("list")

# Import Catch data (Landings plus Discards)

ane6$obsC  <- ane$catch  #catch observations
ane6$timeC <- ane$year   # time of catch observations

ane6$obsC <- c(ane6$obsC, 17830.4)
ane6$timeC <- c(ane6$timeC, 2016)


# TUNING INDEXES
# Pick surveys index, in this case an acoustic biomass index

ane6$timeI <- ane$year[59:71] # Index 1
# Index 2,
# Index 3,
# etc )

ane6$obsI <- ane$index[59:71]

ane6$obsI <- c(ane6$obsI, 67910.3)
ane6$timeI <- c(ane6$timeI, 2016)

#ne_notime <- ane6

# Check the timing of when your survey was performed. 
# In this case the early part of the survey was in month 11-12, the last years in 6-7
# Since the model needs to account for growth, the weight of the fish in the survey needs to be acounted for
# Add to the year the fraction of the month e.g. 11.5/12 = 0.95 and 6.5/12 = 0.54

ane6$timeI[1:5] <- ane6$timeI[1:5] + 0.95
ane6$timeI[6:14] <- ane6$timeI[6:14] + 0.54

# Inspect the file
ane6

# Plot your data

x11()
plotspict.data(ane6) # Notice color coding of the month! 

# Plot inital guesses on the model initial values
plotspict.ci(ane6)

# Priors
# to generally stabilise estimation default semi-informative priors are imposed on these quantities that inhibit them
# from taking extreme and unrealistic values. If informative data are available these priors should have limited
# effect on results, if informative data are not available estimates will reduce to the priors (Source SPICT Vignette)

ane6$priors$logn      <- c(1, 1, 0)
ane6$priors$logalpha  <- c(1, 1, 0)
ane6$priors$logbeta   <- c(1, 1, 0)

# Fixing prior on initial values
ane6$priors$logK <- c(log(30000), 2, 1)

# Fit base model
ane6fit <- fit.spict(ane6)


# Explore results
capture.output(summary(ane6fit))

Check list:
  - did it converge?
  - how is it fitting?

# Let's have a look at the Diagnostics First


ane6fit_diagn       <- calc.osa.resid(ane6fit)

plotspict.diagnostic(ane6fit_diagn)




# Fit Summary
summary(ane6fit)

x11()
plot(ane6fit)


par(mfrow=c(3, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(ane6fit)
plotspict.ffmsy(ane6fit, qlegend=FALSE)
plotspict.catch(ane6fit, qlegend=FALSE)
plotspict.fb(ane6fit, man.legend=FALSE)
plotspict.tc(ane6fit)




ane6fit <- manage(ane6fit)
head(mansummary(ane6fit))

# Diagnostics
ane6fit_nopriors <- retro(ane6fit, nretroyear = 4)

plotspict.retro(ane6fit_nopriors)






############################################################################################################################
# Exercize
# Run the model with also effort
ane6effort <- ane6

ane6effort$obsE <- anex$nominal_effort[59:71]
ane6effort$timeE <- anex$year[59:71]

ane6effortfit <- fit.spict(ane6effort)
capture.output(summary(ane6effortfit))

plotspict.fit(ane6fit_diagn)

ane6effortt_diagn       <- calc.osa.resid(ane6effortfit)

plotspict.diagnostic(ane6effortt_diagn)

plotspict.diagnostic(ane6effortfit)




# Robust estimation for effort (or catch), helps reduce influence of idividual data points on model fit or CIs.
ane6effort$robflage <- 1 # here we turn on robust estimation on effort


# Is this a better model?
# What would you do to improve this model?
############################################################################################################################

# Short term

ane6$manstart <- 2016
ane6$timepredc <- 2016
ane6$dtpredc <- 2
ane6$timepredi <- 2020

anespict_priors <- fit.spict(ane6)
summary(anespict_priors)
plot(anespict_priors)


sumspict.predictions(anespict_nopriors)
par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(anespict_nopriors)
plotspict.ffmsy(anespict_nopriors, qlegend=FALSE)
plotspict.catch(anespict_nopriors, qlegend=FALSE)
plotspict.fb(anespict_nopriors, man.legend=FALSE)




# Run with not time differentiation in survey
ane_notime$priors$logn      <- c(1, 1, 0)
ane_notime$priors$logalpha  <- c(1, 1, 0)
ane_notime$priors$logbeta   <- c(1, 1, 0)

ane_notimefit <- fit.spict(ane_notime)
capture.output(summary(ane_notimefit))

