
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

# Let have a look
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


#ple$priors$logn      <- c(1, 1, 0) # c(mean, sd, on/off)  # shape parameter of the Pella Tomlinso 
#ple$priors$logalpha  <- c(1, 1, 0) # noise ratios logalpha = logsdi - logsdb (sd(index)/sd(biomass))
#ple$priors$logbeta   <- c(1, 1, 0) # noise ratios logbeta = logsdc - logsdf (sd(catch)/sd(F))

