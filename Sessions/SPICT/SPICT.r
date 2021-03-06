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

# look at the file, what do we have there?
View(ane)

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


# Now what ? Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

# Explore convergence
capture.output(summary(ane6fit))[1:4]

# Calculate residuals and main diagnostics

ane6fit_diagn <- calc.osa.resid(ane6fit)

plotspict.diagnostic(ane6fit_diagn)

# Retrospective analysis
# Diagnostics, run it by taking away the last 4 years, one at a time
ane6fit_retro <- retro(ane6fit, nretroyear = 4)

# now plot it!
plotspict.retro(ane6fit_retro)

# So the model fits well, diagnostics are good, we can have a look at the final results.
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

#########################################################################################
# Exercize 1

# 1 Trim catch time series to start the year when the index starts, fit the best model you can
#  - does it converge?
#  - if yes, does it change the paramters and the perception of the stocks ?

############################################################################################################################

# re-run the assessment by assigning the correct timing of the surveys.

# First reload the stock, we truncated it in the prior exercize
ane6 <- vector("list")

# Import Catch data (Landings plus Discards)
ane6$obsC  <- ane$catch  #catch observations
ane6$timeC <- ane$year   # time of catch observations
ane6$obsC <- c(ane6$obsC, 17830.4) # adding an extra value to update the stock
ane6$timeC <- c(ane6$timeC, 2016)  # adding an extra value to update the year

# TUNING INDEXES
ane6$timeI <- ane$year[59:71] # Index 1
ane6$obsI <- ane$index[59:71]
ane6$obsI <- c(ane6$obsI, 67910.3)
ane6$timeI <- c(ane6$timeI, 2016)



# Check the timing of when your survey was performed. 
# In this case the early part of the survey was in month 11-12, the last years in 6-7
# Since the model needs to account for growth, the weight of the fish in the survey needs to be acounted for
# Add to the year the fraction of the month e.g. 11.5/12 = 0.95 and 6.5/12 = 0.54

ane6$timeI[1:5] <- ane6$timeI[1:5] + 0.95
ane6$timeI[6:14] <- ane6$timeI[6:14] + 0.54

ane6$eulertype = "soft"

# how does it look? 

plotspict.data(ane6)

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


# So what do you think, is this a better fit than the previous one?




####################################################################
# Exercize 2
# Production models can take also time series of effort, but many caveats should be made when using fishing effort! 

# Run the model with also effort
# Take the prior list generated and add effort to a new slot of the list
ane6effort <- ane6

ane6effort$obsE <- ane$nominal_effort[59:71]
ane6effort$timeE <- ane$year[59:71]

# EXERCIZE 2

# 1) Run a spict model with fishing effort

# 2) Is it converging? 

# 3) Hhow are the diagnostics? 

# 4) Is the fit better?


########################################################################################

#  Exercize 3


# Robust estimation for effort (or catch), helps reduce influence of idividual data points on model fit or CIs.

# We think that there might be some problems with the effort data, in particular with the estimate of 2007

# 1) Turn on robust estimation
ane6effort$robflage <- 1 # here we turn on robust estimation on effort

# 2) Rerun the assessment, does the fit improve?

ane6effortfitRE <- fit.spict(ane6effort)

capture.output(summary(ane6effortfitRE))[1:4]

plot(ane6effortfitRE)

ane6effortt_diagnRE <- calc.osa.resid(ane6effortfitRE)

plotspict.diagnostic(ane6effortt_diagnRE)

plotspict.diagnostic(ane6effortfit)


# 2) remove the value of fishing effort in 2007 and rurun the assessment, is it better?




############################################################################################################################

#######################################################################################
# CASE STUDY COMPARISON with a age based model
library(FLCore)

data(ple4.index)
data(ple4)

# Take an ICES Stock, like Plaice, this is normally assessed at age with an XSA or other model, the example contains the estimates in the FLStock.

#We want to compare the fit of Plaice with a SPICT production model and that of the original stock assessment

#The FLStock of plaice and the FLIndex, are both at age, and the index has only the abundance (Catch.n)

# First step is to take what is needed from the FLStock and collapse along the age dimensions.

as.vector(catch(ple4))

#Create an emplty list for Spict Object

ple <- vector("list")

# Import Catch data (Landings plus Discards)

ple$obsC  <- as.vector(catch(ple4))  #catch observations
ple$timeC <- seq(1957, 2008)   # time of catch observations


# For the index we need to build an index by biomass, we only have abundance, so as an acceptable hack we take the stock.wt * catch.n of the index to derive the total abundance by age. This is then summed by year

## weight at age index
Iwa <- catch.n(ple4.index) * trim(stock.wt(ple4),
                                  year = dimnames(ple4.index@catch.n)$year,
                                  age = dimnames(ple4.index@catch.n)$age)

## sum the index
as.vector(quantSums(Iwa))

ple$timeI <- seq(1985,2008)

ple$obsI <- as.vector(quantSums(Iwa))

# there is a very low point in the IWA
ple$obsI[[13]] <- NA

ple




# Lets have a look
x11()
plotspict.data(ple)
x11()
plot(ple4)

#Plot inital guesses on the model initial values

plotspict.ci(ple)

plefit <- fit.spict(ple)


# Explore convergence
capture.output(summary(plefit))[1:4]

plot(plefit)

plefit_diagn <- calc.osa.resid(plefit)

plotspict.diagnostic(plefit_diagn)


plefit_retro <- retro(plefit, nretroyear = 4)

# now plot it!
plotspict.retro(plefit_retro)

plot(plefit)
######################################################################################
# RUN ANE 6 with two split surveys and correct timing set up

ane6 <- vector("list")

# Import Catch data (Landings plus Discards)
ane6$obsC  <- ane$catch  #catch observations
ane6$timeC <- ane$year   # time of catch observations
ane6$obsC <- c(ane6$obsC, 17830.4) # adding an extra value to update the stock
ane6$timeC <- c(ane6$timeC, 2016)  # adding an extra value to update the year

ane6$timeI <- list(ane$year[59:63] + 0.95, (c(ane$year[64:71], 2016) + 0.54)) # 

ane6$timeI <- list(ane$year[59:63] , (c(ane$year[64:71], 2016) )) # 

#nep$timeI <- list(as.numeric(froglia$year), nep_pomo$year, medits_gsa17_annuario$Year, grund$year)


ane6$obsI <- list()
ane6$obsI[[1]] <- ane$index[59:63]
ane6$obsI[[2]] <- c(ane$index[64:71], 67910.3)

plotspict.data(ane6)

ne6fitSS2 <- fit.spict(ane6)

# Explore convergence
capture.output(summary(ne6fitSS2))[1:4]

#Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

ne6fitSS2_diagn <- calc.osa.resid(ne6fitSS2)

plotspict.diagnostic(ne6fitSS2_diagn)


# Diagnostics, run it by taking away the last 4 years, one at a time
ane6fitTA_retro <- retro(ne6fitTA, nretroyear = 4)

plotspict.retro(ane6fitTA_retro)


# Fit Summary
summary(ne6fitTA)

x11()
plot(ne6fitTA)





###################################################################
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



# #################################################################################

# MPB

# Collapse function for age based assessment
#
# exercize, collapse ple 4 and compare with estimates already in 
