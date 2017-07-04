# EXFLStock.R - DESC
# EXFLStock.R

# Copyright 2013 FISHREG, EC JRC. Distributed under the GPL 2 or later
# Maintainer: FISHREG, EC JRC
# $Id: $

library(FLCore)

# Create an FLStock with;
# - years from 1980 to 2010
# - 10 ages
# - fully selected ages 3-8

stk <- FLStock(name="HAD", FLQuant(NA, dimnames=list(age=1:10, year=1980:2010)))

range(stk, c('minfbar', 'maxfbar')) <- c(3, 8)

# Load ple4

# Plot catch vs. landings

plot(landings(ple4), catch(ple4))

plot(landings(ple4) / catch(ple4), pch=19, ylim=c(0, 1), ylab="% landings",
	xlab="")


# Load the North Atlantic albacore dataset @ albN.RData

load('albN.RData')


# Plot the stock-recruitment data from ple4

data(ple4)

plot(ssb(ple4), rec(ple4), pch=19, xlab="SSB", ylab="recruits",
	xlim=c(0, max(ssb(ple4))), ylim=c(0, max(rec(ple4))))

lines(lowess(ssb(ple4), rec(ple4)), col='blue', lty=2, lwd=2)

abline(lm(rec~ssb, model.frame(FLQuants(rec=rec(ple4), ssb=ssb(ple4)))), col='red')


