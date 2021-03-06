# FLGrowth - «Short one line description»
# FLGrowth

# Copyright 2013 FISHREG. Distributed under the GPL 2 or later
# Maintainer: FISHREG, JRC


library(FLCore)

# class FLGrowth
setClass('FLGrowth', representation('FLModel',
   mass='FLArray'))

# constructor
setGeneric('FLGrowth', function(model, ...)
  standardGeneric('FLGrowth'))

setMethod('FLGrowth', signature(model='ANY'),
   function(model, ...)
     return(FLModel(model, ..., class='FLGrowth')))

setMethod('FLGrowth', signature(model='missing'),
 function(...)
  return(FLModel(formula(NULL), ..., class='FLGrowth')))

# example
data(ple4)

gro <- FLGrowth(mass~(Minf*(1-exp(-k*(age-t0))))^3, mass=FLCohort(stock.wt(ple4)))

gro <- nls(gro, start=list(Minf=20, k=0.3, t0=0))

