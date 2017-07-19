# FLStock.R - DESC
# FLStock.R

# Copyright 2013 FISHREG. Distributed under the GPL 2 or later
# Maintainer: FISHREG, JRC
# $Id: $

# Class structure
# load data from Mediterranean Hake in GSA 9-10-11

library(FLCore)

#data(hke)
load("HKE_09_10_11_EWG15_11.RData")

# Rename the Stock object to something more handy and faster to write
hke <- HKE_09_10_11_EWG15_11


help("FLStock-class")

class(hke)

slotNames(hke)

# look at the structure
str(hke)

#let's plot it'
plot(hke)


######################
#  INPUT DATA FLStock
######################

# Look  at the name of the stock, why is it different after we changed the object name? 
# name(hke) != object name
name(hke)

# Description
desc(hke)

# Range of ages, years, plus group and fbar
range(hke)


# Operations
# catch =~ landings + discards, in case discards had already been included in landings, so are 0 here. Normally discards should be be reported in the discard quant
landings(hke) + discards(hke)

catch(hke)

catch(hke) <- landings(hke) + discards(hke)

# data & results

# *, *.n & *.wt
catch.n(hke)

# Compute catch for 2006 only, this is the product of number of fish @ age times the mean weight of fish @ age
quantSums(catch.n(hke)[,"2006",,,,] * catch.wt(hke)[,"2006",,,,])

# is it in line with what is reported with the hke stock?
catch(hke)[,"2006",,,,]

# m, m.spwn
m(hke) # natural mortality

# What is the mean natural mortality applied to the stock?
mean(m(hke))

# m.spwn
m.spwn(hke) # fraction of the natural mortality ocurring before spawning

# equivalently you have some fishing mortality (harvest) occurring on the fish population before spawning. This is important when you have population that spawn in a narrow window of time, in particular with a short life cycle.
harvest.spwn(hke)


##################################
# ESTIMATED PARAMETERS
##################################

# harvest e.g. fishing mortality or harvest rate, normally defined as F
harvest(hke)

# Size of the estimated population at sea, in terms of total biomass, total numbers at age and total mean weight at age.

stock.n(hke)
stock.wt(hke)

# in this case the size of the at sea population was not compute, so we use the method:
stock(hke)

# Se we compute it
computeStock(hke)

# and assign it to the slot stock
stock(hke) <- computeStock(hke)

# Estimated Recruitment
rec(hke)

# Estimates of Spawning Stock Biomass
ssb(hke)


##################################
# METHODS for the class FLSTOCK
################################## 

# We already saw computeStock() and quantSums()

# computing discards, landings and catch
discards(hke) <- computeDiscards(hke)

# summary & plot
summary(hke)

plot(hke)

# transform

# window, [, qapply
# Check year range # window()

range(hke)
smallhke <- window(hke, start = 2008, end = 2013)

# Check new year range
range(smallhke)

# SUBSET by year
temp <- hke[,c("2008", "2009", "2010", "2011")]
# or
temp <- hke[,as.character(2008:2011)]

# many FLQuant methods also available at this level
summary(propagate(hke, 10))

summary(hke[,'2010'])

summary(trim(hke, year=1990:1999))

summary(expand(hke, year=1957:2057))


# replace using logical values
catch(hke)[[6]]<-99
plot(catch(hke))
catch(hke)[catch(hke)==99] <- 5000
plot(catch(hke))

# summary of a FLStock
summary(hke)

# plot and FLStock
# the default
plot(hke)

# or individual parts
plot(stock(hke))
plot(stock.n(hke))
plot(landings(hke))

# Methods for usual computations

#METHODS rec = stock.n[rec.age=first.age,]
rec(hke)


# METHODS Calculate Spawning Stock Biomass (SSB)
# SSB = stock.n * exp(-F * F.spwn - M * M.spwn) * stock.wt * mat

ssb(hke)

object<-hke
colSums(object@stock.n * exp(-object@harvest *
	object@harvest.spwn - object@m * object@m.spwn) *
	object@stock.wt * object@mat, na.rm = FALSE)

getMethod("ssb", "FLStock")

# METHODS Fbar = mean(F between fbar ages)
fbar(hke)
getMethod("fbar", "FLStock")

# METHODS fapex = max F per year
fapex(hke)

#METHODS ssbpurec = SSB per unit recruit
ssbpurec(hke)

#METHODS r = stock reproductive potential
r(hke)

#METHODS survprob = survival probabilities by year or cohort
survprob(hke)
survprob(hke, by ='cohort')
plot(survprob(hke))

#METHODS coercion
# as.FLSR
hkeSR<-as.FLSR(hke)
summary(hkeSR)

# METHODS convert to data frame
# entire FlStock
temp<-as.data.frame(hke)
summary(temp)

# or only some slots
as.data.frame(FLQuants(catch.n=catch.n(hke), stock.n=stock.n(hke)))

