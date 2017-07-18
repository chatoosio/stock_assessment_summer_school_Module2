#
# Day 2 Author Chato Osio

#  Surplus Production in Continuous-Time (SPICT)
library(spict)

# Data Structure


# Build spict stock

# Let's work with a good time series for hovy in GSA Anc6.
# What is available: 
  # catch since 1945
  # biomass index since 2002
  # effort index for Purse seine in kw*Days and fishing days


# Load the .csv file

ane <- read.csv("ANE_06_nonAge.csv")

# Assemble stock

ane6 <- vector("list")

# Import Catch data (Landings plus Discards)

ane6$obsC  <- ane$catch  #catch observations
ane6$timeC <- ane$year   # time of catch observations

ane6$obsC <- c(ane6$obsC, 17830.4) # adding an extra value to update the stock
ane6$timeC <- c(ane6$timeC, 2016)  # adding an extra value to update the year


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


# Inspect the file
ane6

# Plot your data

x11()
plotspict.data(ane6) # Notice color coding of the month! 



#Plot inital guesses on the model initial values

plotspict.ci(ane6)

#The two top plots come from plotspict.data, with the dashed horizontal line representing a guess of MSY.
#This guess comes from a linear regression between the index and the catch divided by the index (middle row,left). This regression is expected to have a negative slope. A similar plot can be made showing catch versus catch/index (middle row, right) to approximately find the optimal effort (or effort proxy). The proportional increase in the index as a function of catch (bottom row, right) should show primarily positive increasesin index at low catches and vice versa. Positive increases in index at large catches could indicate model violations (Source SPICT Vignette)



# Fit base model
ane6fit <- fit.spict(ane6)


# Explore convergence
capture.output(summary(ane6fit))[1:4]

#Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First


ane6fit_diagn <- calc.osa.resid(ane6fit)

plotspict.diagnostic(ane6fit_diagn)


# Diagnostics, run it by taking away the last 4 years, one at a time
ane6fit_retro <- retro(ane6fit, nretroyear = 4)

plotspict.retro(ane6fit_retro)


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




#ane6fit <- manage(ane6fit)
#head(mansummary(ane6fit))


############################################################################################################################

# re-run the assessment by assigning the correct timing of the surveys.

# Check the timing of when your survey was performed. 
# In this case the early part of the survey was in month 11-12, the last years in 6-7
# Since the model needs to account for growth, the weight of the fish in the survey needs to be acounted for
# Add to the year the fraction of the month e.g. 11.5/12 = 0.95 and 6.5/12 = 0.54

ane6$timeI[1:5] <- ane6$timeI[1:5] + 0.95
ane6$timeI[6:14] <- ane6$timeI[6:14] + 0.54

ane6$eulertype = "soft"

ne6fitTA <- fit.spict(ane6)


# Explore convergence
capture.output(summary(ne6fitTA))[1:4]

#Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

ane6fitTA_diagn <- calc.osa.resid(ne6fitTA)

plotspict.diagnostic(ane6fitTA_diagn)


# Diagnostics, run it by taking away the last 4 years, one at a time
ane6fitTA_retro <- retro(ne6fitTA, nretroyear = 4)

plotspict.retro(ane6fitTA_retro)


# Fit Summary
summary(ne6fitTA)

x11()
plot(ne6fitTA)

################################################################################

# Priors
# to generally stabilise estimation default semi-informative priors are imposed on these quantities that inhibit them  from taking extreme and unrealistic values. 
#If informative data are available these priors should have limited
# effect on results, if informative data are not available estimates will reduce to the priors (Source SPICT Vignette)

ane6$priors$logn      <- c(1, 1, 0) # c(mean, sd, on/off)  # shape parameter of the Pella Tomlinso 
ane6$priors$logalpha  <- c(1, 1, 0) # noise ratios logalpha = logsdi - logsdb (sd(index)/sd(biomass))
ane6$priors$logbeta   <- c(1, 1, 0) # noise ratios logbeta = logsdc - logsdf (sd(catch)/sd(F))

ne6fit_nopriors <- fit.spict(ane6)


# Explore convergence
capture.output(summary(ne6fit_nopriors))[1:4]

#Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

ane6fit_nopriors_diagn <- calc.osa.resid(ne6fit_nopriors)

plotspict.diagnostic(ane6fitTA_diagn)


# Diagnostics, run it by taking away the last 4 years, one at a time
ane6fitTA_retro <- retro(ne6fitTA, nretroyear = 4)

plotspict.retro(ane6fitTA_retro)


# Fit Summary
summary(ne6fitTA)

x11()
plot(ne6fitTA)








#####################################################################
# Fixing prior on initial values, some factor of the max catch in the historical series
ane6$priors$logK <- c(log(2 * max(ane6$obsC, na.rm = TRUE), 2, 1))


####################################################################
# Exercize
# Run the model with also effort
ane6effort <- ane6

ane6effort$obsE <- ane$nominal_effort[59:71]
ane6effort$timeE <- ane$year[59:71]

plotspict.data(ane6effort)
ane6effortfit <- fit.spict(ane6effort)

capture.output(summary(ane6effortfit))[1:4]

plot(ane6effortfit)

ane6effortt_diagn <- calc.osa.resid(ane6effortfit)

plotspict.diagnostic(ane6effortt_diagn)

plotspict.diagnostic(ane6effortfit)




# Robust estimation for effort (or catch), helps reduce influence of idividual data points on model fit or CIs.
ane6effort$robflage <- 1 # here we turn on robust estimation on effort


# Is this a better model?
# What would you do to improve this model?
############################################################################################################################

# Short term

# ane6$manstart <- 2016
# ane6$timepredc <- 2016
# ane6$dtpredc <- 2
# ane6$timepredi <- 2020
# 
# anespict_priors <- fit.spict(ane6)
# summary(anespict_priors)
# plot(anespict_priors)
# 
# 
# sumspict.predictions(anespict_nopriors)
# par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
# plotspict.bbmsy(anespict_nopriors)
# plotspict.ffmsy(anespict_nopriors, qlegend=FALSE)
# plotspict.catch(anespict_nopriors, qlegend=FALSE)
# plotspict.fb(anespict_nopriors, man.legend=FALSE)
# 
# 
# 
# 
# 
# 
# 
# Exercize 1

# 1 Trim catch time series to start the year when the index starts, fit the best model you can
#  - does it converge?
#  - if yes, does it change the paramters and the perception of the stocks ?



# #################################################################################

# MPB

# Collapse function for age based assessment
#
# exercize, collapse ple 4 and compare with estimates already in 
