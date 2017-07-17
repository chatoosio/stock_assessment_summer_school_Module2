# FLStock.R - DESC
# FLStock.R

# Copyright 2013 FISHREG. Distributed under the GPL 2 or later
# Maintainer: FISHREG, JRC
# $Id: $

# Class structure
# load data from Plaice stocks in N Atlantic ICES area IV

library(FLCore)

#data(hke)
load("~/GitHub/stock_assessment_summer_school_Module2/Material/stocks_for_course/HKE9_11_at_Age/HKE_09_10_11_EWG15_11.RData")

hke <- HKE_09_10_11_EWG15_11


help("FLStock-class")

class(hke)

slotNames(hke)

# look at the structure
str(hke)

#let's plot it'
plot(hke)

name(hke)
desc(hke)
range(hke)

# catch =~ landings + discards
landings(hke) + discards(hke)

catch(hke)

catch(hke) <- landings(hke) + discards(hke)

# data & results

# *, *.n & *.wt
catch.n(hke)

quantSums(catch.n(hke)[,"2006",,,,] * catch.wt(hke)[,"2006",,,,])

catch(hke)[,"2006",,,,]

# m, m.spwn
m(hke) # natural mortality

m.spwn(hke) # fraction of the natural mortality ocurring before spawning

# harvest, harvest.spwn
harvest(hke)
harvest.spwn(hke)

# stock
stock(hke)
stock.n(hke)

# in this case the size of the at sea population was not compute, so we use the method:
computeStock(hke)

stock(hke) <- computeStock(hke)

# Methods
# hke

# Methods: computing discards, landings and catch
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
#plot(smallhke)

# SUBSET
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

#METHODS Calculate Spawning Stock Biomass (SSB)
# SSB = stock.n * exp(-F * F.spwn - M * M.spwn) * stock.wt * mat

ssb(hke)

object<-hke
colSums(object@stock.n * exp(-object@harvest *
	object@harvest.spwn - object@m * object@m.spwn) *
	object@stock.wt * object@mat, na.rm = FALSE)

getMethod("ssb", "FLStock")

#METHODS Fbar = mean(F between fbar ages)
fbar(hke)
getMethod("fbar", "FLStock")

#METHODS fapex = max F per year
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
ple4SR<-as.FLSR(hke)
summary(ple4SR)

# METHODS convert to data frame
# entire FlStock
temp<-as.data.frame(hke)
summary(temp)

# or only some slots
as.data.frame(FLQuants(catch.n=catch.n(hke), stock.n=stock.n(hke)))

