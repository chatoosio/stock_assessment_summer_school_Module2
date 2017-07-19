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

library(FLa4a)
library(diagram)
library(ggplotFL)

# Load the Hake stock for the combined GSA 9-10-11

load("~/GitHub/stock_assessment_summer_school_Module2/Material/stocks_for_course/HKE9_11_at_Age/HKE_09_10_11_EWG15_11.RData")
load("~/GitHub/stock_assessment_summer_school_Module2/Material/stocks_for_course/HKE9_11_at_Age/HKE_09_10_11_idx.Rdata")

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




# fitting
fit <- sca(hke, hke.idx[3])

# diagnostics
res <- residuals(fit, hke, hke.idx[3])
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- hke + fit
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")



# Explore how well the model is predicitng the catches

plot(fit, hke)

# Explore how well the model is predicitng survey abundances


plot(fit, hke.idx[3])

# Individual indexes can be called with
# Explore how well the model is predicitng the catches

plot(fit, hke.idx[1])


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
qmodel <- list(~ factor(age), ~ factor(age), ~ factor(age), ~ factor(age)) 

# separable Fay = Fa * Fy
fmodel <- ~ factor(age) + factor(year)
fit <- sca(stock = hke, indices = hke.idx, fmodel=fmodel, qmodel=qmodel)

wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

# smooth separable Fay = smooth Fa * smooth Fy
fmodel <- ~ s(age, k=4) + s(year, k = 5)
fit1 <- sca(hke, hke.idx, fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit1)), drape = TRUE, screen = list(x = -90, y=-45))

# interaction Fa * Fy
fmodel <- ~ te(age, year, k = c(3,5))
fit2 <- sca(hke, hke.idx[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit2)), drape = TRUE, screen = list(x = -90, y=-45))

# smooth separable + interaction Fa,Fy
fmodel <- ~ s(age, k=4) + s(year, k = 5) + te(age, year, k = c(3,3))
fit3 <- sca(hke, hke.idx, fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit3)), drape = TRUE, screen = list(x = -90, y=-45))

# interaction Fa * Fy + recruitment F extra smooth
fmodel <- ~ te(age, year, k = c(4,5)) + s(year, k = 5, by = as.numeric(age==1))
fit4 <- sca(hke, hke.idx, fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit4)), drape = TRUE, screen = list(x = -90, y=-45))

#--------------------------------------------------------------------
# Exercise 01
#--------------------------------------------------------------------
# fit a linear F model with a quadratic term at age

# fit other linear models that use transformations of age or year

# fit different smoothers and different degrees of freedom (see ?s)

#--------------------------------------------------------------------
# catchability submodel
#--------------------------------------------------------------------

# year fraction before the survey
sfrac <- mean(range(hke.idx[[1]])[c("startf", "endf")])

# fix fmodel
fmodel <- ~ factor(age) + factor(year)

# one coefficient for each age
qmodel <- list(~ factor(age)) 
fit <- sca(hke, hke.idx[1], fmodel, qmodel)

# mambo jambo to plot index in the right period 
Z <- (m(hke) + harvest(fit))*sfrac # check M * sfrac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)

wireframe(data ~ age + year, data = as.data.frame(index(fit)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

# smooth age catchability
qmodel <- list(~ s(age, k=4))
fit1 <- sca(hke, hke.idx[1], fmodel, qmodel)
Z <- (m(hke) + harvest(fit1))*sfrac
lst <- dimnames(fit1@index[[1]])
lst$x <- stock.n(fit1)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit1)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

# age-year interaction
qmodel <- list(~ te(age, year, k = c(3,40)))
fit2 <- sca(hke, hke.idx[1], fmodel, qmodel)
Z <- (m(hke) + harvest(fit2))*sfrac
lst <- dimnames(fit2@index[[1]])
lst$x <- stock.n(fit2)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit2)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

# smooth age catchability + year linear effect
qmodel <- list( ~ s(age, k=4) + year)
fit3 <- sca(hke, hke.idx[1], fmodel, qmodel)
Z <- (m(hke) + harvest(fit3))*sfrac
lst <- dimnames(fit3@index[[1]])
lst$x <- stock.n(fit3)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit3)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

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
# Exercise 02
#--------------------------------------------------------------------
# compare S/R fitted with 'sca' and 'fmle'

#====================================================================
# The a4aSCA method - statistical catch-at-age
#====================================================================

fmodel <- ~ s(age, k=4) + s(year, k = 5)
qmodel <- list( ~ s(age, k=4) + year)
srmodel <- ~s(year, k=8)
fit <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel) 

#--------------------------------------------------------------------
# Exercise 03 
#--------------------------------------------------------------------

# compare defaults with 'sca'

#--------------------------------------------------------------------
# N1 submodel
#--------------------------------------------------------------------

n1model <- ~s(age, k=4)
fit1 <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel, n1model) 
flqs <- FLQuants(smo=stock.n(fit1)[,1], fac=stock.n(fit)[,1])

xyplot(data~age, groups=qname, data=flqs, type="l", main="N1 models", auto.key=keylst)

#--------------------------------------------------------------------
# Variance submodel
#--------------------------------------------------------------------

vmodel <- list(~1, ~1)
fit <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel, n1model, vmodel) 
vmodel <- list(~ s(age, k=5), ~1)
fit1 <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel, n1model, vmodel) 
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

# nao <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
# dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
# nao <- FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")
# nao <- seasonMeans(trim(nao, year=dimnames(stock.n(hke))$year))
# nao <- as.numeric(nao)
# 
# srmodel <- ~ nao
# fit2 <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel) 
# flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit2)[1])
# 
# xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates", auto.key=keylst)
# 
# srmodel <- ~ ricker(a=~nao, CV=0.1)
# fit3 <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel) 
# flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit3)[1])
# 
# xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates", auto.key=keylst)
# 
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

fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4) + year)
srmodel <- ~s(year, k=20)
fit <- a4aSCA(hke, hke.idx[1], fmodel, qmodel, srmodel) 

#--------------------------------------------------------------------
# Predict
#--------------------------------------------------------------------

fit.pred <- predict(fit)
lapply(fit.pred, names)

#--------------------------------------------------------------------
# Simulate
#--------------------------------------------------------------------

fits <- simulate(fit, 1000)
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
