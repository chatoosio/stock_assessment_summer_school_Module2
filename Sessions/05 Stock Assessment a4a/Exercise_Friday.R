# Removing previous objects and clean environment
rm(list=ls())



#### Load libraries ####
library(FLCore)
library(FLAssess)
library(FLXSA)
library(FLa4a)
library(ggplotFL)
#### Load data ####
load("ANE.RData")
load("ANEidx.RData")


################################
#### Explore data structure ####
################################

#### Exploring commercial data ####

landings(ANCHOVY)#landings
units(landings(ANCHOVY))<-"t"

catch(ANCHOVY)#catch
units(catch(ANCHOVY))<-"t"

catch.n(ANCHOVY) #catch number by age
units(catch.n(ANCHOVY)) <- "10^3"

catch.wt(ANCHOVY) #catch weight by age
units(catch.wt(ANCHOVY)) <- "kg"

stock.n(ANCHOVY)  #stock number by age - Not filled in yet
units(stock.n(ANCHOVY)) <- "10^3"

stock.wt(ANCHOVY)#stock weight by age
units(stock.wt(ANCHOVY))<-"kg"

mat(ANCHOVY)#maturity by age

m(ANCHOVY)#natural mortality by age

harvest(ANCHOVY)#fishing mortality by age  - Not filled in yet
units(harvest(ANCHOVY))<-"f"

range(ANCHOVY)
range(ANCHOVY)[c("minfbar","maxfbar")]

harvest.spwn(ANCHOVY) <- 0.5
m.spwn(ANCHOVY) <- 0.5

############################
### Exploring data loaded ##
############################

plot(catch(ANCHOVY),xlab="Year",ylab="Catch_in_tonnes")

catches<-as.data.frame(catch.n(ANCHOVY))

ggplot(catches, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Age structure catches")+xlab("Age") +ylab("N (thousands)")+ scale_colour_discrete(name  ="Year")


ggplot(catches, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("Catches_ANCHOVY_GSA9-11")+xlab("Year") +ylab("N (thousands)")+ scale_colour_discrete(name  ="Age")



weights <- as.data.frame(catch.wt(ANCHOVY))

ggplot(weights, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Mean weight at age of catches")+xlab("Age") +ylab("Kg")+ scale_colour_discrete(name  ="Year")


ggplot(weights, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("Mean weight at age of catches")+xlab("Year") +ylab("Kg")+ scale_colour_discrete(name  ="Age")

# Set fbar in the stock 

range(ANCHOVY)["minfbar"] <- 1
range(ANCHOVY)["maxfbar"] <- 2


# Set the plus group in the stock
ANCHOVY <- setPlusGroup(ANCHOVY, 4)


#### Exploring survey index ####

names(ANCHOVY.tun)


index(ANCHOVY.tun[[1]])



Echo_1718 <- as.data.frame(index(ANCHOVY.tun[[1]]))

ggplot(Echo_1718, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Age structure Echo_1718")+xlab("Age") +ylab("Index")+ scale_colour_discrete(name  ="Year")



################################################
### Cohorts consistence in the tuning fleets ###
################################################

ggplot(Echo_1718, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("abundance in the Echo_1718 survey by age")+xlab("Year") +ylab("Index")+ scale_colour_discrete(name  ="Age")

ggplot(Echo_1718, aes(year,data))+geom_line(size=1.1)+facet_wrap(~age, scales = "free_y")+ggtitle("abundance in the Echo_1718 survey by age")+xlab("Year") +ylab("Index")+ scale_colour_discrete(name  ="Age")


plot(ANCHOVY.tun[[1]], type="internal",main="Cohorts consistence in the Echo_1718 survey")













#====================================================================
# The sca method - statistical catch-at-age
#====================================================================

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
qmodel <- list(~ factor(age)) 

# separable Fay = Fa * Fy
fmodel <- ~ factor(age) + factor(year)
fit <- sca(stock = ANCHOVY, indices = ANCHOVY.tun, fmodel=fmodel, qmodel=qmodel)


# diagnostics
res <- residuals(fit, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit, ANCHOVY.tun)



# smooth separable Fay = smooth Fa * smooth Fy
fmodel <- ~ s(age, k=4) + s(year, k = 10)
fit1 <- sca(ANCHOVY, ANCHOVY.tun, fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit1)), drape = TRUE, screen = list(x = -90, y=-45))

# diagnostics
res <- residuals(fit1, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit1
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit1, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit1, ANCHOVY.tun)






# interaction Fa * Fy
fmodel <- ~ te(age, year, k = c(4,8))
fit2 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit2)), drape = TRUE, screen = list(x = -90, y=-45))

# diagnostics
res <- residuals(fit2, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit2
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit2, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit2, ANCHOVY.tun)





# smooth separable + interaction Fa,Fy
fmodel <- ~ s(age, k=4) + s(year, k = 10) + te(age, year, k = c(3,7))
fit3 <- sca(ANCHOVY, ANCHOVY.tun, fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit3)), drape = TRUE, screen = list(x = -90, y=-45))

# diagnostics
res <- residuals(fit3, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit3
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit3, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit3, ANCHOVY.tun)



# # interaction Fa * Fy + recruitment F extra smooth
# fmodel <- ~ te(age, year, k = c(4,8)) + s(year, k = 5, by = as.numeric(age==0))
# fit4 <- sca(ANCHOVY, ANCHOVY.tun, fmodel, qmodel)
# wireframe(data ~ age + year, data = as.data.frame(harvest(fit4)), drape = TRUE, screen = list(x = -90, y=-45))
# 
# # diagnostics
# res <- residuals(fit4, ANCHOVY, ANCHOVY.tun)
# plot(res, main="Residuals")
# bubbles(res)
# qqmath(res)
# 
# # update stock object with assessment results
# stk <- ANCHOVY + fit4
# plot(stk, main="Stock summary")
# 
# # F 3D
# wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))
# 
# # N 3D
# wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))
# 
# # C 3D
# wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")
# 
# # Explore how well the model is predicting the catches
# 
# plot(fit4, ANCHOVY)
# 
# # Explore how well the model is predicting survey abundances
# 
# plot(fit4, ANCHOVY.tun)
# 
# 

#--------------------------------------------------------------------
# catchability submodel
#--------------------------------------------------------------------

# year fraction before the survey
sfrac <- mean(range(ANCHOVY.tun[[1]])[c("startf", "endf")])

# fix fmodel
fmodel <- ~ s(age, k=4) + s(year, k = 10) + te(age, year, k = c(3,7))

# one coefficient for each age
qmodel <- list(~ factor(age)) 
fit <- sca(ANCHOVY, ANCHOVY.tun, fmodel, qmodel)

# mambo jambo to plot index in the right period 
Z <- (m(ANCHOVY) + harvest(fit))*sfrac # check M * sfrac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)

wireframe(data ~ age + year, data = as.data.frame(index(fit)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

# diagnostics
res <- residuals(fit, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit, ANCHOVY.tun)







# smooth age catchability
qmodel <- list(~ s(age, k=4))
fit1 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel)
Z <- (m(ANCHOVY) + harvest(fit1))*sfrac
lst <- dimnames(fit1@index[[1]])
lst$x <- stock.n(fit1)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit1)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))


# diagnostics
res <- residuals(fit1, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit1
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit1, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit1, ANCHOVY.tun)






# age-year interaction
qmodel <- list(~ te(age, year, k = c(4,10)))
fit2 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel)
Z <- (m(ANCHOVY) + harvest(fit2))*sfrac
lst <- dimnames(fit2@index[[1]])
lst$x <- stock.n(fit2)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit2)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))


# diagnostics
res <- residuals(fit2, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit2
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit2, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit2, ANCHOVY.tun)




# smooth age catchability + year linear effect
qmodel <- list( ~ s(age, k=4) + year)
fit3 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel)
Z <- (m(ANCHOVY) + harvest(fit3))*sfrac
lst <- dimnames(fit3@index[[1]])
lst$x <- stock.n(fit3)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit3)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))


# diagnostics
res <- residuals(fit3, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit3
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit3, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit3, ANCHOVY.tun)


#--------------------------------------------------------------------
# stock-recruitment submodel
#--------------------------------------------------------------------

fmodel <- ~ s(age, k=4) + s(year, k = 10) + te(age, year, k = c(3,7))
qmodel <- list(~ s(age, k=4))

##maybe
srmodel <- ~ factor(year)
fit <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel=fmodel, qmodel=qmodel, srmodel=srmodel) 
# diagnostics
res <- residuals(fit, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit, ANCHOVY.tun)



#### no
srmodel <- ~ s(year, k=5)
fit1 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel, srmodel) 

# diagnostics
res <- residuals(fit1, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit1
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit1, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit1, ANCHOVY.tun)


#### no
srmodel <- ~ ricker(CV=0.05)
fit2 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel, srmodel) 
# diagnostics
res <- residuals(fit2, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit2
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit2, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit2, ANCHOVY.tun)



#### maybe
srmodel <- ~ bevholt(CV=0.05)
fit3 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel, srmodel) 

# diagnostics
res <- residuals(fit3, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit3
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit3, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit3, ANCHOVY.tun)


#### no
srmodel <- ~ hockey(CV=0.05)
fit4 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel, srmodel) 

# diagnostics
res <- residuals(fit4, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit4
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit4, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit4, ANCHOVY.tun)




##### no
srmodel <- ~ geomean(CV=0.05)
fit5 <- sca(ANCHOVY, ANCHOVY.tun[1], fmodel, qmodel, srmodel) 

# diagnostics
res <- residuals(fit5, ANCHOVY, ANCHOVY.tun)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

# update stock object with assessment results
stk <- ANCHOVY + fit5
plot(stk, main="Stock summary")

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

# Explore how well the model is predicting the catches

plot(fit5, ANCHOVY)

# Explore how well the model is predicting survey abundances

plot(fit5, ANCHOVY.tun)

AIC(fit,fit1,fit2,fit3,fit4,fit5)
BIC(fit,fit1,fit2,fit3,fit4,fit5)


flqs <- FLStocks(fac=ANCHOVY + fit, sm_year=ANCHOVY + fit1, ricker=ANCHOVY + fit2, bh=ANCHOVY + fit3, hockey=ANCHOVY + fit4, geom_mean=ANCHOVY + fit5)

plot(flqs)


