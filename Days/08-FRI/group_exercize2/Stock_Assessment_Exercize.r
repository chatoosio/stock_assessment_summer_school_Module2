########################################################################## SPICT RUN CASE STUDY ANCHOVY
#
#### Load libraries ####
library(FLCore)
library(FLAssess)
library(FLXSA)
library(FLa4a)
library(ggplotFL)
library(spict)

#### Load data #######################################
load("ANE.RData")
load("ANEidx.RData")
######################################################

# Assignment

#GROUP 1-2 Keep the FLstocks and FLIndices as they are and run a statistical catch at age model
#GROUP 3-4 Collapse the FLstocks and FLIndices to obtain a time-series of Catch in tons and an index of abundance in tons and run a surplus production model in SPICT.





ane <- vector("list")

# Import Catch data (Landings plus Discards)

ane$obsC  <- as.vector(catch(ANCHOVY))  #catch observations
ane$timeC <- seq(1975, 2015)   # time of catch observations


# For the index we need to build an index by biomass, we only have abundance, so as an acceptable hack we take the stock.wt * catch.n of the index to derive the total abundance by age. This is then summed by year

## weight at age index
Iwa <- catch.n(ANCHOVY.tun[[1]]) * trim(stock.wt(ANCHOVY),
                                        year = dimnames(ANCHOVY.tun[[1]]@catch.n)$year,
                                        age = dimnames(ANCHOVY.tun[[1]]@catch.n)$age)

## sum the index
#as.vector(quantSums(Iwa))
#ane$timeI <- seq(2009,2015)
#ane$timeI <- ane$timeI + 0.584

ane$eulertype = "soft"


ane$obsI <- as.vector(quantSums(Iwa))


# Lets have a look
x11()
plotspict.data(ane)
plot(ane)

#Plot inital guesses on the model initial values

plotspict.ci(ane)

#ane$ini$logK <- log(4 * max(ane$obsC))
#ane$priors$logK <- c(log(4 * max(ane$obsC)), 2, 1)

#ane$priors$logn <- c(1, 1, 0)
#ane$priors$logalpha <- c(1, 1, 0)
#ane$priors$logbeta <- c(1, 1, 0)

#ane$phases$logsdc <- -1
#ane$ini$logsdc <- log(1e-3)
