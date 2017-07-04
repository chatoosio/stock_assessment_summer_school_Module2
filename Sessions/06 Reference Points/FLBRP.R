# FLBRP.R - DESC
# FLBRP.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

library(FLBRP)

# Load the example FLStock object from FLCore
 
data(ple4)
 
# Create the corresponding FLBRP object

p4brp <- FLBRP(ple4)

summary(p4brp)

# The FLBRP class has information on

# selection Patterns
catch.sel(p4brp)
discards.sel(p4brp) 

ggplot(discards.sel(p4brp), aes( age, data))+geom_point()
ggplot(catch.sel(p4brp), aes( age, data))+geom_point()
 
# mass-at-age
stock.wt(p4brp)
catch.wt(p4brp)
discards.wt(p4brp)
 
# biological parameters
m(p4brp)
mat(p4brp)

# selectivity
xyplot(data~age,data=catch.sel(p4brp),type=c('l', 'p'))

# and other quantities by age
xyplot(data~age|qname, data=FLQuants(sel=catch.sel(p4brp),
 	dsel=discards.sel(p4brp), swt=stock.wt(p4brp),
	cwt =catch.wt(p4brp), mat= mat(p4brp), m = m(p4brp)),
	type="l",scale="free")


# we have not provided a SR relationship
# so analyses wll be per-recruit

# All *.obs slots hold the observations from FLStock
fbar.obs(p4brp)

# Once an FLBRP object has been created then equilibrium
# quantities can be estimated

# we estimate equilibrium quantities
p4brp <- brp(p4brp)


# and a set of equilibirum quantities for a range of F values

# fishing mortality 
fbar(p4brp)
harvest(p4brp)
 
# abundance-at-age
stock.n(p4brp)

# catch-at-age
catch.n(p4brp)

# plus some age-aggregated values
yield.hat(p4brp)

# mean recruitment
rec.hat(p4brp)

# and we get a table of reference points
refpts(p4brp)

plot(refpts(p4brp))
# In this case, Fmsy is the same as Fmax, since the assumed stock recruitment
# relationship is mean recruitment

refpts(p4brp)[c('msy', ('fmax')), ]

# Thus plotting the reference points ans expected quantities
plot(p4brp)

# The derived reference points would be used to compare the fishing mortality (from Fbar) with the 
# reference point of choice.
fbar(ple4)[,"2008"]
refpts(p4brp)['f0.1','harvest']

# F/F0.1 ?
fbar(ple4)[,"2008"]  / refpts(p4brp)['f0.1','harvest'] 

# SR

# we can add a SR fitted model

p4sr <- as.FLSR(ple4, model=bevholt)
p4sr <- fmle(p4sr)

plot(p4sr)

# and provide it when constructing FLBRP
p4brp <- FLBRP(ple4, sr=p4sr)

model(p4brp)
params(p4brp)

# and we refit FLBRP
p4brp <- brp(p4brp)

# and see the difference in RPs
refpts(p4brp)

# and relationships
plot(p4brp)

# also with historical data
plot(p4brp, obs=TRUE)


# ITERATIONS

# We can introduce variability in some quantities
# and see the effect on estimated RPs

# add some noise on the natural mortality
p4brpi <- p4brp
m(p4brpi) <- rlnorm(100, m(p4brp), 0.3)

# calculate reference points
refpts(p4brpi) <- computeRefpts(p4brpi)

# take a look
refpts(p4brpi)

plot(refpts(p4brpi)[1:6, 1:5])

fbar(ple4)

# EXERCIZE 01
# In ple4 make a sensitivity analysis on a range of M to see how it affects the estimation of the main
# reference points.
