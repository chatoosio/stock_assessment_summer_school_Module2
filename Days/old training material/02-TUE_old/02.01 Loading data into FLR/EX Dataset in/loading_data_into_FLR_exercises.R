# loading_data_into_FLR_exercises.R - How to load data from outside R into FLR objects
# loading_data_into_FLR_exercises.R

# Copyright 2013 JRC FISHREG. Distributed under the GPL 2 or later
# Maintainer: JRC FISHREG
# $Id: $
# Created: 14/03/2013
# Modified:

# All the data for these exercises can be found in Exercise Data

# 1. In the 'haddock' directory there are some files for haddock in area IV in Lowestoft VPA format.
# Load the maturity at age data (in the file hadivmo.dat) into an FLQuant 

# 2. Create a complete FLStock object from the data stored as Lowestoft VPA format XXXXX.
# Assume that there are no discards. Set a plusgroup age of 7. 
# Load in the additional abundance and fishing mortality files (n.txt and f.txt) and set
# the appropriate slots of the FLStock object
# Set fbar range to be 2:5
# What is the SSB and fbar in the year 2010?
# (hint: check the dimensions of the abundance and fishing mortality data)

# 3. Given the spreadsheet haddock.ods, make a complete FLStock object
# (hint: start by saving all the data as separate *.csv files. Then load them in...)


# Suggested solutions
library(FLCore)

# 1
mat <- readVPAFile("Exercise Data/haddock/hadivmo.dat")

# 2
stk <- readFLStock("Exercise Data/haddock/hadividx.dat")
# All catch in landings, set discards to 0
discards.n(stk) <- 0
discards.wt(stk) <- landings.wt(stk)
catch(stk) <- computeCatch(stk, slot="all")
stk <- setPlusGroup(stk,7)

n <- readVPAFile("Exercise Data/haddock/n.txt")
f <- readVPAFile("Exercise Data/haddock/f.txt")

harvest(stk)[,as.character(1998:2011)] <- f
units(harvest(stk)) <- "f"
range(stk)[c("minfbar","maxfbar")] <- c(2,5)
stock.n(stk)[,as.character(1998:2011)] <- n

ssb(stk)
fbar(stk)

#3 



