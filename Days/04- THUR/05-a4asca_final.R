#####################################################################
# SCA
# Statistical catch-at-age framework for stock assessment
#####################################################################
# submodels
#	fmodel
#	qmodel
#	srmodel
#	vmodel
#	n1model

# fit types = "MP" or "assessment"

# fit methods simple = "sca" or advanced = "a4aSCA" 

#====================================================================
# Load
#====================================================================
install.packages("rgl")
install.packages("diagram")


library(rgl)
library(FLa4a)
library(diagram)
library(ggplotFL)

# Load the Hake stock for the combined GSA 9-10-11

load("HKE_09_10_11_EWG15_11.RData")
load("HKE_09_10_11_idx.Rdata")

#data(hke)
#data(hke.idx)
source("funs.R")
keylst <- list(points=FALSE, lines=TRUE, space="right")

#====================================================================
# Quick and dirty
#====================================================================
# rename the stocks
hke <- HKE_09_10_11_EWG15_11
hke.idx <- flq.idx 

# Adjust Fbar range

units(harvest(hke))<-"f"
range(hke)["minfbar"] <- 0   
range(hke)["maxfbar"] <- 3


# To fit a simple default a4a model, use the function sca()
# to explore a simple diagnostics, we run it only with the tuning index MEDITS 09

fit <- sca(hke, hke.idx)

# diagnostics
res <- residuals(fit, hke, hke.idx)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- hke + fit
plot(stk, main="Stock summary")


jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
z <- as.matrix(harvest(stk)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -60, y= - 45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches", screen = list(x = -60, y= - 45))


# Explore how well the model is predicitng the catches
plot(fit, hke)

# Explore how well the model is predicitng survey abundances
plot(fit, hke.idx)

# Individual indexes can be called with
# Explore how well the model is predicitng the catches

plot(fit, hke.idx[1])
plot(fit, hke.idx[2])
plot(fit, hke.idx[3])
plot(fit, hke.idx[4])


#####
#To get information about the likelihood fit the method fitSumm() will extract information about likelihood, number of parameters, etc, and the methods AIC() and BIC() will compute the information criteria.
#Get the fit parameters

fitSumm(fit)

AIC(fit) # Akike Information Criterion, the smaller the better, but be careful that the model is fitting something sensible!
BIC(fit) # Bayesian Information Criterion

#====================================================================
# The sca method - statistical catch-at-age
#====================================================================

?sca

# submodels
    #	fmodel
    #	qmodel
    #	srmodel
    #	vmodel
    #	n1model



#--------------------------------------------------------------------
# fishing mortality submodel
#--------------------------------------------------------------------

# fix catchability model (qmodel)
# The qmodel is a list where a catchability model needs to be set up for each index, hence here we have 3 Medits and one commercial CPUE.
qmodel <- list(~ factor(age), # q of MEDITS 10
               ~ factor(age), # q of CPUE LLS 10
               ~ factor(age), # q of MEDITS 9
               ~ factor(age)) # q of MEDITS 11


# separable Fay = Fa * Fy
fmodel <- ~ factor(age) + factor(year)

fit <- sca(stock = hke, indices = hke.idx, fmodel=fmodel, qmodel=qmodel)

#fit <- sca(hke, hke.idx, fmodel, qmodel)


hke.sep <- stk + fit

z <- as.matrix(harvest(hke.sep)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

# diagnostics
res <- residuals(fit, hke, hke.idx)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

AIC(fit)
BIC(fit)
================================================================================
#  Next we may make things a bit more interesting by using an (unpenalised) thin plate spline, where we'll borrow the smoothing splines method (s()) provided by package mgcv. We're using the Hake data again, and since it has 6 ages we will use a simple rule of thumb that the spline should have fewer than 6/2 = 3 degrees of freedom, and so we opt for 3-4 degrees of freedom. We will also do the same for year and model the change in F through time as a smoother with 5 degrees of freedom.
  
  
# smooth separable Fay = smooth Fa * smooth Fy
fmodel <- ~ s(age, k = 4) + s(year, k = 5)

fit1 <- sca(hke, hke.idx, fmodel, qmodel)

hke1 <- stk + fit1

z <- as.matrix(harvest(hke1)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


# diagnostics
res1 <- residuals(fit1, hke, hke.idx)
plot(res1, main="Residuals")
bubbles(res1)
qqmath(res1)

AIC(fit1)
BIC(fit1)
================================================================================

# interaction Fa * Fy
fmodel <- ~ te(age, year, k = c(4,5))
fit2 <- sca(hke, hke.idx, fmodel, qmodel)

hke2 <- stk + fit2

z <- as.matrix(harvest(hke2)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

# diagnostics
res2 <- residuals(fit2, hke, hke.idx)
plot(res2, main="Residuals")
bubbles(res2)
qqmath(res2)

AIC(fit2)
BIC(fit2)

================================================================================

# smooth separable + interaction Fa,Fy
fmodel <- ~ s(age, k=4) + s(year, k = 5) + te(age, year, k = c(3,3))

fit3 <- sca(hke, hke.idx, fmodel, qmodel)
hke3 <- stk + fit3

# diagnostics
res3 <- residuals(fit3, hke, hke.idx)
plot(res3, main="Residuals")
bubbles(res3)
qqmath(res3)

z <- as.matrix(harvest(hke3)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

AIC(fit3)
BIC(fit3)

================================================================================

# interaction Fa * Fy + recruitment F extra smooth
fmodel <- ~ te(age, year, k = c(4,5)) + s(year, k = 6, by = as.numeric(age==0))

fit4 <- sca(hke, hke.idx, fmodel, qmodel)
hke4 <- stk + fit4

# diagnostics
res4 <- residuals(fit4, hke, hke.idx)
plot(res4, main="Residuals")
bubbles(res4)
qqmath(res4)

z <- as.matrix(harvest(hke4)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit4, hke)
plot(fit4, hke.idx[1])

AIC(fit4)
BIC(fit4)

# What happens if we compare the AIC's and BIC's of all the fitted models?

# Which model fits best? 

AIC(fit, fit1, fit2, fit3, fit4)
BIC(fit, fit1, fit2, fit3, fit4)



#--------------------------------------------------------------------
# Exercise 01
#--------------------------------------------------------------------
# 1.1) fit a linear F model with a quadratic term at age, using in the fmodel a command like fmodel <- ~ age + I(age^2) + factor(year)

#  - is it improving the fit? What do the diagnostic support?

# 1.2) fit other linear models that use transformations of age or year



# 1.3) fit different smoothers and different degrees of freedom (see ?s)

# - does it crash when you have higher K that your data can support?
# - did fit a better model, what are the AIC and BIC of you models


# Solution 1
fmodel <- ~ age + I(age^2) + factor(year)
fitQ <- sca(hke, hke.idx, fmodel, qmodel)
hkeQ <- stk + fitQ

# diagnostics
resQ <- residuals(fitQ, hke, hke.idx)
plot(resQ, main="Residuals")
bubbles(resQ)
qqmath(resQ)

z <- as.matrix(harvest(hkeQ)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit4, hke)
plot(fit4, hke.idx[1])

AIC(fit4)
BIC(fit4)


#--------------------------------------------------------------------
##############################
# catchability submodel
##############################
#--------------------------------------------------------------------

#The catchability submodel is set up the same way as the F submodel and the tools available are the same. The only difference is that the submodel is set up as a list of formulas, where each formula relates with one abundance index. For Hake in GSA 9-10-11 we have been running the model with 4 tuning indexes, so we need to set up the catchability of each tuning index.
#We'll start by fixing the F and R models and compute the fraction of the year the index relates to, which will allow us to compute catchability at age and year.


# fix fmodel, remember this is the simples model we used before
fmodel <- ~ factor(age) + factor(year)

# The Q model allows now one catchability coefficient for each age
qmodel <- list(~ factor(age), ~ factor(age), ~ factor(age), ~ factor(age)) 

fit5 <- sca(hke, hke.idx, fmodel, qmodel)
hke5 <- stk + fit5

res5 <- residuals(fit5, hke, hke.idx)
plot(res5, main="Residuals")

bubbles(res5)
qqmath(res5)

z <- as.matrix(harvest(hke5)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit5, hke)
plot(fit5, hke.idx)

AIC(fit5)
BIC(fit5)

#==============================================================================

# smooth age catchability
qmodel <- list(~ s(age, k=4),~ s(age, k=4),~ s(age, k=4),~ s(age, k=4))

fit6 <- sca(hke, hke.idx, fmodel, qmodel)
hke6 <- stk + fit6

res6 <- residuals(fit6, hke, hke.idx)
plot(res6, main="Residuals")
bubbles(res6)
qqmath(res6)

z <- as.matrix(harvest(hke6)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit6, hke)
plot(fit6, hke.idx)

AIC(fit6)
BIC(fit6)

#----------------------------------------------------------------------------------

# age-year interaction

qmodel <- list(~ te(age, year, k = c(3,5)),~ te(age, year, k = c(3,5)),~ te(age, year, k = c(3,5)),~ te(age, year, k = c(3,5)))
fit7 <- sca(hke, hke.idx, fmodel, qmodel)
hke7 <- stk + fit7

res7 <- residuals(fit7, hke, hke.idx)
plot(res7, main="Residuals")
bubbles(res7)
qqmath(res7)

z <- as.matrix(harvest(hke7)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit7, hke)
plot(fit7, hke.idx)

AIC(fit7)
BIC(fit7)

#-------------------------------------------------------------------------------

# smooth age catchability + year linear effect

qmodel <- list( ~ s(age, k=4) + year, ~ s(age, k=4) + year, ~ s(age, k=4) + year, ~ s(age, k=4) + year)

fit8 <- sca(hke, hke.idx, fmodel, qmodel)
hke8 <- stk + fit8

res8 <- residuals(fit8, hke, hke.idx)
plot(res8, main="Residuals")
bubbles(res8)
qqmath(res8)

z <- as.matrix(harvest(hke8)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit8, hke)
plot(fit8, hke.idx)

AIC(fit8)
BIC(fit8)

#
AIC(fit, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
BIC(fit, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)

#-------------------------------------------------------------------------
# EXERCIZE 2

# 1) Fit an a4a sca to hake by trying the best combination of fmodel and qmodel

fmodel <- ~ te(age, year, k = c(4,5)) + s(year, k = 5, by = as.numeric(age==0))
qmodel <- list(~ factor(age), ~ factor(age), ~ factor(age), ~ factor(age)) 

fit9 <- sca(hke, hke.idx, fmodel, qmodel, fit = "assessment")
hke9 <- stk + fit9

res9 <- residuals(fit9, hke, hke.idx)
plot(res9, main="Residuals")

bubbles(res5)
qqmath(res5)

z <- as.matrix(harvest(hke5)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit5, hke)
plot(fit5, hke.idx)


# 2) You want to make sure that your assessment converged, in the call to sca() make sure you now change the fitting method to fit = "assessment" 

# - you might need to adjust the k in the smoothers
# - use all the diagnostics to come to a valid conclusion on your model fit
# - tell us what is the best model!

# If you are not happy with the results, what would you do to improve the assessment?

    # 1) Try to change the plusgroup from 6+ to 5+ as it look like the internal consistency of the coohrts is bad with age 5 and 6.

hke <- setPlusGroup(hke, 5)
hke.idx[[1]] <- FLIndex(index=setPlusGroup(index(hke.idx[[1]]), 5))

hke.idx[[1]] <- FLIndex(index=setPlusGroup(index(hke.idx[[1]]), 5))
hke.idx[[1]] <- FLIndex(index=setPlusGroup(index(hke.idx[[1]]), 5))

range(flq.idx[[1]], c('startf', 'endf')) <- c( 0.5, 0.75)
range(flq.idx[[2]], c('startf', 'endf')) <- c( 0.5, 0.75)
range(flq.idx[[3]], c('startf', 'endf')) <- c( 0.5, 0.75)

is(hke.idx)

# interaction Fa * Fy + recruitment F extra smooth
fmodel <- ~ te(age, year, k = c(4,5)) + s(year, k = 5, by = as.numeric(age==0))
qmodel <- list( ~ factor(age), ~ factor(age), ~ factor(age)) 

fitplus5 <- sca(hke, hke.idx, fmodel, qmodel)
hkeTRIM <- stk + fitTRIM

# diagnostics
resT <- residuals(fitTRIM, hke, hke.idxTRIM)
plot(resT, main="Residuals")
bubbles(resT)
qqmath(resT)

z <- as.matrix(harvest(hkeTRIM)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fitTRIM, hke)
plot(fit4, hke.idxTRIM)




    # 2) Try to remove the worst fitting tuning index
    # Trim the FLIndices of hke.idx to remove the MEDITS GSA 11 as it is the more problematic survey index.

hke.idxTRIM <- hke.idx[1:3]

# interaction Fa * Fy + recruitment F extra smooth
fmodel <- ~ te(age, year, k = c(4,5)) + s(year, k = 5, by = as.numeric(age==0))
qmodel <- list( ~ factor(age), ~ factor(age), ~ factor(age)) 

fitTRIM <- sca(hke, hke.idxTRIM, fmodel, qmodel)
hkeTRIM <- stk + fitTRIM

# diagnostics
resT <- residuals(fitTRIM, hke, hke.idxTRIM)
plot(resT, main="Residuals")
bubbles(resT)
qqmath(resT)

z <- as.matrix(harvest(hkeTRIM)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n")
)
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fitTRIM, hke)
plot(fit4, hke.idxTRIM)

AIC(fit4)
BIC(fit4)

plot(fitTRIM)

#====================================================================
# Predict and simulate
# To simulate we need a fit="assessment" so that the hessian/vcov
# is computed
# a4aSCA
#====================================================================
?a4aSCA

fmodel <- ~ te(age, year, k = c(3,5)) + s(year, k = 5, by = as.numeric(age==1))
qmodel <- list( ~ s(age, k=3) + year, ~ s(age, k=3) + year, ~ s(age, k=3) + year, ~ s(age, k=3) + year)

#srmodel <- ~s(year, k=5)
#fit.sim1 <- sca(hke, hke.idx, fmodel, qmodel)#, srmodel) 
fit.sim <- a4aSCA(hke, hke.idx, fmodel, qmodel)#, srmodel) 


#--------------------------------------------------------------------
# Simulate
#--------------------------------------------------------------------

fits <- simulate(fit.sim, 1000)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")), auto.key=keylst)

stks <- hke + fits
plot(stks)

#-------------------------------------------------------------------
# EXERCIZE 03

# Compare your best model fit with the best fit from the XSA of Day 3



#--------------------------------------------------------------------
# stock-recruitment submodel
#--------------------------------------------------------------------

fmodel <- ~ s(age, k=4) + s(year, k = 5)
qmodel <- list(~ s(age, k=4))

srmodel <- ~ factor(year)
fit <- sca(hke, hke.idx[1], fmodel=fmodel, qmodel=qmodel, srmodel=srmodel) 
srmodel <- ~ s(year, k=5)
fit1 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
srmodel <- ~ ricker(CV=0.05)
fit2 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
srmodel <- ~ bevholt(CV=0.05)
fit3 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
srmodel <- ~ hockey(CV=0.05)
fit4 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
srmodel <- ~ geomean(CV=0.05)
fit5 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 

flqs <- FLQuants(fac=stock.n(fit)[1], bh=stock.n(fit3)[1])

plot(as.FLIndices(flqs))
xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment models", auto.key = keylst)

#--------------------------------------------------------------------
# Exercise 04
#--------------------------------------------------------------------
# compare S/R fitted with 'sca' and 'fmle'

#====================================================================
# The a4aSCA method - statistical catch-at-age
#====================================================================

fmodel <- ~ s(age, k=4) + s(year, k = 5)
qmodel <- list( ~ s(age, k=4) + year,~ s(age, k=4) + year,~ s(age, k=4) + year,~ s(age, k=4) + year)
srmodel <- ~s(year, k=8)
fit <- a4aSCA(hke, hke.idx, fmodel, qmodel) 
hkea4a <- hke + fit

plot(hkea4a)
#--------------------------------------------------------------------
# Exercise 03 
#--------------------------------------------------------------------

# compare defaults with 'sca'

#--------------------------------------------------------------------
# N1 submodel
#--------------------------------------------------------------------

n1model <- ~s(age, k=4)
fit1 <- a4aSCA(hke, hke.idx, fmodel, qmodel, srmodel, n1model) 
flqs <- FLQuants(smo=stock.n(fit1)[,1], fac=stock.n(fit)[,1])

xyplot(data~age, groups=qname, data=flqs, type="l", main="N1 models", auto.key=keylst)

#--------------------------------------------------------------------
# Variance submodel
#--------------------------------------------------------------------

vmodel <- list(~1, ~1)
fit <- a4aSCA(hke, hke.idx, fmodel, qmodel, srmodel, n1model, vmodel) 
vmodel <- list(~ s(age, k=5), ~1)
fit1 <- a4aSCA(hke, hke.idx, fmodel, qmodel, srmodel, n1model, vmodel) 
flqs <- FLQuants(cts=catch.n(fit), smo=catch.n(fit1))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Variance models", scales=list(y=list(relation="free")), auto.key=keylst)

predict(fit)$vmodel$catch

wireframe(data ~ age + year, data = as.data.frame(predict(fit1)$vmodel$catch), drape = TRUE, screen = list(x = -90, y=-45))

#--------------------------------------------------------------------
# Exercise 04
#--------------------------------------------------------------------

# test alternative n1models, 
# do you expect large impacts in recent year's estimates ?

#--------------------------------------------------------------------
# Working with covariates
#--------------------------------------------------------------------

nao <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
nao <- FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")
nao <- seasonMeans(trim(nao, year=dimnames(stock.n(hke))$year))
nao <- as.numeric(nao)

srmodel <- ~ nao
fit2 <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel)
flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit2)[1])

xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates", auto.key=keylst)

srmodel <- ~ ricker(a=~nao, CV=0.1)
fit3 <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel)
flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit3)[1])

xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates", auto.key=keylst)

#--------------------------------------------------------------------
# External weigthing of likelihood components
#--------------------------------------------------------------------

data(hke)
data(hke.idx)

stk <- hke
idx <- hke.idx[1]
# variance of observed catches
varslt <- catch.n(stk)
varslt[] <- 1
catch.n(stk) <- FLQuantDistr(catch.n(stk), varslt) # show: remove var
# variance of observed indices
varslt <- index(idx[[1]])
varslt[] <- 0.05
index.var(idx[[1]]) <- varslt

# run
fit <- a4aSCA(hke, hke.idx[1], vmodel=list(~1, ~1))
fit1 <- a4aSCA(stk, idx, vmodel=list(~1, ~1)) 

flqs <- FLQuants(nowgt=stock.n(fit), extwgt=stock.n(fit1))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Likelihood weighting", scales=list(y=list(relation="free")), auto.key=keylst)

#--------------------------------------------------------------------
# Assessing ADMB files
#--------------------------------------------------------------------

fit1 <- a4aSCA(stk, idx, fmodel, qmodel, srmodel, n1model, vmodel=list(~1, ~1), wkdir="mytest") 

#--------------------------------------------------------------------
# More models
#--------------------------------------------------------------------

# constant fishing mortality for ages older than 5
fmodel = ~ s(replace(age, age>5, 5), k=4) + s(year, k=20)
fit <- sca(hke, hke.idx, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

# the same model with to two periods
fmodel=~s(age, k = 3, by = breakpts(year, 2000))
fit <- sca(hke, hke.idx, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

# smoother for each age 
fmodel <- ~ factor(age) + s(year, k=10, by = breakpts(age, c(2:8)))
fit <- sca(hke, hke.idx, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

#--------------------------------------------------------------------
# Exercise 05
#--------------------------------------------------------------------

# All together now ! What's your best assessment of hke ? Why ?

#====================================================================
# Predict and simulate
# To simulate we need a fit="assessment" so that the hessian/vcov
# is computed
#====================================================================

fmodel <- ~ s(age, k=4) + s(year, k = 5)
qmodel <- list( ~ s(age, k=4) + year,~ s(age, k=4) + year,~ s(age, k=4) + year,~ s(age, k=4) + year)
srmodel <- ~s(year, k=5)
fit <- a4aSCA(hke, hke.idx, fmodel, qmodel, srmodel) 

#--------------------------------------------------------------------
# Predict
#--------------------------------------------------------------------

fit.pred <- predict(fit)
lapply(fit.pred, names)

#--------------------------------------------------------------------
# Simulate
#--------------------------------------------------------------------

fits <- simulate(fit4, 1000)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")), auto.key=keylst)

stks <- hke + fits
plot(stks)

#--------------------------------------------------------------------
# WKSAM exercise
#--------------------------------------------------------------------
fits <- simulate(fit, 25)
stk <- hke + fits

fits2 <- a4aSCA(stk, hke.idx[1], fmodel, qmodel, srmodel, fit="MP")  
flqs <- FLQuants(fit=stock.n(fit), repl=stock.n(fits2))
xyplot(data~year|age, groups=qname, data=flqs, type="l", scales=list(y=list(relation="free")), auto.key=keylst)

#--------------------------------------------------------------------
# Exercise 06
#--------------------------------------------------------------------

# Assess your stock or hke (don't forget to simulate)

# Fit a periodic function to recruitment

# Fit a logistic to F
