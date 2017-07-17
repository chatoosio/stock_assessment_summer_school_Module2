# EX FLQuant S4.R - DESC
# EX FLQuant S4.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $


library(FLCore)

# CREATE

# (1) Create an FLQuant object with elements numbered sequentially (i.e. 1 to N)
# and with ages from 1 to 6, years from 2003 to 2012 and four seasons

# (2) Create an FLQuant with dimensions 6,10,1,1,1,100 and lognormally-distributed
# random nunmbers

# SUBSET

# (3) Extract from flq the values for the first three years


# (4) Select from flq leaving out the sixth age

# (5) What if do not know the precise age names?


# APPLY

# (6) Calculate the proportion-at-age per year

# (7) How many values in flq are greater than 0?

flq > 0

sum(flq > 0)

# TRANSFORM

# PROGRAMMING

# Let's try programming a very simple population model

# First, an FLQuant for pop(ulation), age=1:6, year=1:20

pop <- FLQuant(NA, dimnames=list(age=1:6, year=1:20))

# Assign an initial abundance by age with exponential decay

pop[,1] <- 1000 * exp(-1 * cumsum(c(0, rep(0.1, 5))))

# We know very well F and M

M <- 0.2
F <- 0.4

# Loop over years and then ages to calculate numbers-at-age

for(y in 2:20) {
	# Recruitment as a random process
	pop[1,y] <-	rlnorm(1, 7, 0.2)
	for(a in 2:6) {
		pop[a,y] <- pop[a-1,y-1] * exp(-(F + M))
	}
}

# Plot population trends by age
xyplot(data~year, pop, groups=age, type='l')

# Create a weight-at-age FLQuant
waa <- FLQuant(t(t(exp(cumsum(rep(0.2, 6))))))

# Calculate biomass-at-age
bio <- pop %*% waa

# Total biomass
tot <- quantSums(bio)
