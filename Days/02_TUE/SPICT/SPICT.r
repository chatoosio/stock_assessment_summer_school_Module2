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
# Exercize 1
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

# Next exercize


# Robust estimation for effort (or catch), helps reduce influence of idividual data points on model fit or CIs.

# We think that there might be some problems with the effort data, in particular with the estimate of 2007

# 1) Turn on robust estimation
ane6effort$robflage <- 1 # here we turn on robust estimation on effort
# Rerun the assessment, does the fit improve?

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


#--------------------------------------------------------------------------------------------------------------------
# HOW TO RUN A CATCH FORECAST IN SPICT

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


# Short term forecast
#To make a catch forecast a forecast interval needs to be specified. This is done by specifying the start of the interval (inp$timepredc) and the length of the interval in years (inp$dtpredc). 

#For example, if a forecast of the annual catch of 2018 is of interest, then inp$timepredc = 2018 and inp$dtpredc = 1. 

#In addition to the forecast interval a fishing scenario needs to be specified. This is done by specifying a factor (inp$ffac) to multiply the current fishing mortality by (i.e. the F at the last time point of the time period where data are available) and the time that management should start (inp$manstart). 

#The time point of the reported forecast of biomass and fishing mortality can be controlled by setting inp$timepredi. Producing short-term forecasts entails minimal additional computing time.

ane6$manstart <- 2016  # When management will start
ane6$timepredc <- 2020 # Time when we want predicted catch
ane6$dtpredc <- 1 # Time interval in years for prediction
ane6$timepredi <- 2020
ane6$ffac <- 0.75 # Specify the fishing scenario for the forecast, in this case use a factor to  


anespict <- fit.spict(ane6)
summary(anespict)
plot(anespict)

sumspict.predictions(anespict)

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(anespict)
plotspict.ffmsy(anespict, qlegend=FALSE)
plotspict.catch(anespict, qlegend=FALSE)
plotspict.fb(anespict, man.legend=FALSE)


# BUILD MANAGEMENT SCENARIOS
# The package has a function that runs several predefined management scenarios, which can be presented in a forecast table.

res <- manage(anespict)
df <- mansummary(res)

df

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(res)
plotspict.ffmsy(res, qlegend=FALSE)
plotspict.catch(res, qlegend=FALSE)
plotspict.fb(res, man.legend=FALSE)

# table of forecast for  +1 year
mansummary(res, ypred=1, include.unc = FALSE)

# table of forecast for  +4 year, in our case the period 2019-2020
mansummary(res, ypred=4, include.unc = FALSE)



##################################################################################
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



ane6$manstart <- 2016  # When management will start
ane6$timepredc <- 2020 # Time when we want predicted catch
ane6$dtpredc <- 1 # Time interval in years for prediction
ane6$timepredi <- 2020
#ane6$ffac <- 0.75 # Specify the fishing scenario for the forecast, in this case use a factor to  multiply F F 0.25


anespict2 <- fit.spict(ane6)
summary(anespict2)
plot(anespict2)

sumspict.predictions(anespict2)

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(anespict2)
plotspict.ffmsy(anespict2, qlegend=FALSE)
plotspict.catch(anespict2, qlegend=FALSE)
plotspict.fb(anespict2, man.legend=FALSE)


# BUILD MANAGEMENT SCENARIOS
res2 <- manage(anespict2)
df2 <- mansummary(res2)

df2

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(res2)
plotspict.ffmsy(res2, qlegend=FALSE)
plotspict.catch(res2, qlegend=FALSE)
plotspict.fb(res2, man.legend=FALSE)

# table of forecast for  +1 year
mansummary(res2, ypred=1, include.unc = FALSE)

# table of forecast for  +4 year, in our case the period 2019-2020
mansummary(res2, ypred=4, include.unc = FALSE)




# #################################################################################

# MPB

# Collapse function for age based assessment
#
# exercize, collapse ple 4 and compare with estimates already in 
