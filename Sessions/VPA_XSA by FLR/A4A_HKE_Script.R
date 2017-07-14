#' ---
#' title: "A4A in FLR"
#' author: "Alessandro Mannini"
#' date: "July 14th, 2017"
#' ---

# Removing previous objects and clean environment
rm(list=ls())

# Use underneath code to open FLR installation window
# utils::install.packages(repos="http://flr-project.org/R")

#### Load libraries ####
library(FLCore)
library(FLAssess)
library(FLXSA)
library(ggplotFL)
library(FLa4a)
#### Load data ####
load("HKE_09_10_11_stk.RData")
HKE=stk
load("HKE_09_10_11_idx.RData")
HKE.idx=flq.idx

# Set fbar in the stock 
range(stk)["minfbar"] <- 0
range(stk)["maxfbar"] <- 3
range(HKE)["minfbar"] <- 0
range(HKE)["maxfbar"] <- 3

# Set the plus group in the stock
stk <- setPlusGroup(stk, 6)
HKE <- setPlusGroup(HKE, 6)

#####################
###### Run XSA ######
#####################
FLXSA.control=FLXSA.control()
slot(FLXSA.control, "rage") <- as.integer(0)
slot(FLXSA.control, "qage") <- as.integer(5)
slot(FLXSA.control, "shk.ages") <-as.integer(2)
slot(FLXSA.control, "shk.yrs") <- as.integer(3)
slot(FLXSA.control, "fse") <- 2
slot(FLXSA.control, "maxit") <- as.integer(30)
#### TUNING using all indices #####
HKE.xsa <- FLXSA(HKE, HKE.idx, FLXSA.control)
HKE.new_xsa <- HKE + HKE.xsa





# FIT a4a Assessment
# Mod 0
fit0 <- sca(stock = stk, indices = flq.idx,  fit = "assessment")
plot(stk + fit0)
plot(FLStocks(a4a=stk+simulate(fit0, 250)))
res0 <- residuals( fit0, stk, flq.idx)
plot(res0)
bubbles(res0)
qqmath(res0)

plot(fit0, stk)
plot(fit0, flq.idx[1])
plot(fit0, flq.idx[2])
plot(fit0, flq.idx[3])
plot(fit0, flq.idx[4])

wireframe(data ~ age + year, data = as.data.frame(harvest(fit0)),
          drape = TRUE, zlab="F", screen = list(x = -90, y=-45))
plot(FLStocks(XSA=HKE.new_xsa,a4a=stk+simulate(fit0, 250)))


# Mod 1
fmod1 <- ~factor(year) + factor(age)
qmod1 <- list(~factor(age),~factor(age),~factor(age),~factor(age))
srmod1 <- ~factor(year)
fit1 <- sca(stock = stk, indices = flq.idx, qmodel=qmod1,fmodel = fmod1,  fit = "assessment")
plot(stk + fit1)
res1 <- residuals( fit1, stk, flq.idx)
plot(res1)
bubbles(res1)
qqmath(res1)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit1)),
          drape = TRUE, zlab="F", screen = list(x = -90, y=-45))
plot(FLStocks(XSA=HKE.new_xsa,fit0=stk+simulate(fit0, 250),fit1=stk+simulate(fit1, 250)))

# # Mod 2
# qmod <- list(~factor(age), ~factor(age),~factor(age))
# fit2 <- sca(stock = stk, indices = flq.idx, fmodel = fmod, qmodel = qmod,  fit = "assessment")

# Mod 3
qmod3 <- list(~s(age, k=3), ~s(age, k=3),~s(age, k=3),~s(age, k=3))
fit3 <- sca(stock = stk, indices = flq.idx, fmodel = fmod1, qmodel = qmod3,  fit = "assessment")
plot(stk + fit3)
res3 <- residuals( fit3, stk, flq.idx)
plot(res3)
bubbles(res3)
qqmath(res3)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit3)),
          drape = TRUE, zlab="F", screen = list(x = -90, y=-45))
plot(FLStocks(XSA=HKE.new_xsa,fit3=stk+simulate(fit3, 250)))

fbar(HKE.new_xsa)[,"2014"]
fbar(stk+fit3)[,"2014"]
fbar(HKE.new_xsa)/fbar(stk+fit3)


# Mod 4
fmod4 <- ~s(year, k=5) + s(age, k=3)
fit4 <- sca(stock = stk, indices = flq.idx, fmodel = fmod4, qmodel = qmod1,  fit = "assessment")
plot(stk + fit4)
res4 <- residuals( fit4, stk, flq.idx)
plot(res4)
bubbles(res4)
qqmath(res4)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit4)),
          drape = TRUE, zlab="F", screen = list(x = -90, y=-45))


# Mod 6
qmode6 <- list(~ te(age, year, k = c(3,3)), ~ te(age, year, k = c(3,3)), ~ te(age, year, k = c(3,3)), ~ te(age, year, k = c(3,3)))
fit6 <- sca(stock = stk, indices = flq.idx, qmodel = qmode6,  fit = "assessment")
plot(stk + fit6)
res6 <- residuals( fit6, stk, flq.idx)
plot(res6)

bubbles(res6)
qqmath(res6)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit6)),
          drape = TRUE, zlab="F", screen = list(x = -90, y=-45))

plot(FLStocks(XSA=HKE.new_xsa,fit0=stk+simulate(fit0, 250),fit1=stk+simulate(fit1, 250),
              fit3=stk+simulate(fit3, 250)
              #fit4=stk+simulate(fit4, 250),
              #fit6=stk+simulate(fit6, 250)
              ))



# Retrospective
nyears <- 3
spe.stk_retro1 <- FLStocks()
for (i in 0:nyears){
  print(i)
  # window stock and index
  stk_tmp <- window(stk, end = 2014 - i)
  ids_tmp <- window(flq.idx, end = 2014 - i)
  fit <- sca(stk_tmp,
             ids_tmp,
             fmodel = fmod1, qmodel = qmod1, srmodel=srmod1)
  spe.stk_retro1[[paste("year minus ", i, sep="")]] <- stk_tmp + fit
}
plot(spe.stk_retro1)


# stkbrp <-FLBRP(stk + fit1)
# 
# # Calculate Ref Points
# stksr <- as.FLSR(stk + fit1, model = bevholt)
# stksr <- as.FLSR(stk + fit1, model = "geomean")
# 
# stksr <- fmle(stksr)
# 
# plot(stksr)
# 
# # and provide it when constructing FLBRP
# stkbrp <- FLBRP(stk + fit1, sr=stksr)
# stkbrp <-  brp(stkbrp)
# refpts(stkbrp)
# plot(stkbrp)
