#' ---
#' title: "VPA & XSA in FLR"
#' author: "Alessandro Mannini"
#' date: "July 12nd, 2017"
#' ---


# This document is based on:
# Stock Assessment Using Tuned VPA. Rob Scott & Clara Ulrich (December 2010) and
# Virtual Population analysis using eXtended Survivor Analysis.
#Dorleta Garcia & Alessandro MANNINI (March 2017)
  
# Removing previous objects and clean environment
rm(list=ls())

# Use underneath code to open FLR installation window
# utils::install.packages(repos="http://flr-project.org/R")

#### Load libraries ####
library(FLCore)
library(FLAssess)
library(FLXSA)
library(ggplotFL)
#### Load data ####
load("HKE_09_10_11_stk.RData")
HKE=stk
load("HKE_09_10_11_idx.RData")
HKE.idx=flq.idx

################################
#### Explore data structure ####
################################

#### Exploring commercial data ####
str(HKE)
landings(HKE)[]#landings
units(landings(HKE))<-"t"

catch.n(HKE)[] #catch number by age
units(catch.n(HKE)) <- "10^3"

catch.wt(HKE)[] #catch weight by age
units(catch.wt(stk)) <- "kg"

stock.n(HKE)[]  #stock number by age
units(stock.n(stk)) <- "10^3"

stock.wt(HKE)#stock weight by age
units(stock.wt(HKE))<-"kg"

mat(HKE)[]#maturity by age

m(HKE)#natural mortality by age

harvest(HKE)[]#fishing mortality by age  - Not filled in yet
units(harvest(HKE))<-"f"

range(HKE)[c("minfbar","maxfbar")]

fbar(HKE)[] # Not filled in yet


############################
### Exploring data loaded ##
############################

plot(catch(HKE),xlab="Year",ylab="Catch_in_tonnes")
catture=as.data.frame(catch.n(HKE))
xyplot(data~age,type=c('l','p'),groups=year,data=catture, xlab="Ages",
       ylab="Numbers in thousands",main="Catches_HKE_GSA9-11",
       auto.key=list(corner=c(1,1), columns=1,
                     title="Year", cex.title=1))

xyplot(data~year,type=c('l','p'),groups=age,data=catture, xlab="Year",
       ylab="Numbers in thousands",main="Catches_HKE_GSA9-11",
       auto.key=list(corner=c(1,1), columns=1,
                     title="Age", cex.title=1))

# Set fbar in the stock 

range(HKE)["minfbar"] <- 0
range(HKE)["maxfbar"] <- 3
 

# Set the plus group in the stock
HKE <- setPlusGroup(HKE, 6)

# Set the plus group in the index (if needed!)
# HKE.idx[[1]] <- FLIndex(index=setPlusGroup(index(HKE.idx[[1]]), 5))
# range(HKE.idx[[1]], c("startf", "endf")) <- c(0.66, 0.75)


#### Exploring survey index ####
str(HKE.idx)
index(HKE.idx[[1]])
index(HKE.idx[[2]])
index(HKE.idx[[3]])
index(HKE.idx[[4]])


Med_SA10=as.data.frame(catch.n(HKE.idx[[1]]))
xyplot(data~age,type=c('l','p'),groups=year,data=Med_SA10, xlab="Ages",
       ylab="Numbers/sqkm",main="Med_SA10 survey",
       auto.key=list(corner=c(1,1), columns=1,
                     title="Year", cex.title=1))

LLS_SA10=as.data.frame(catch.n(HKE.idx[[2]]))
xyplot(data~age,type=c('l','p'),groups=year,data=LLS_SA10, xlab="Ages",
       ylab="Numbers/sqkm",main="LLS_SA10",
       auto.key=list(corner=c(1,1), columns=1,
                     title="Year", cex.title=1))

Med_SA9=as.data.frame(catch.n(HKE.idx[[3]]))
xyplot(data~age,type=c('l','p'),groups=year,data=Med_SA9, xlab="Ages",
       ylab="Numbers/sqkm",main="Med_SA9",
       auto.key=list(corner=c(1,1), columns=1,
                     title="Year", cex.title=1))

Med_SA11=as.data.frame(catch.n(HKE.idx[[4]]))
xyplot(data~age,type=c('l','p'),groups=year,data=Med_SA11, xlab="Ages",
       ylab="Numbers/sqkm",main="Med_SA11",
       auto.key=list(corner=c(1,1), columns=1,
                     title="Year", cex.title=1))


###########################
### Cohorts consistence ###
###########################

my.stock <- HKE
catchn.fli <- FLIndex(FLQuant(NA, dimnames=list(year = dimnames(catch.n(HKE))$year,
                                                age = dimnames(catch.n(HKE))$age)))
index(catchn.fli) <- catch.n(my.stock)
plot(catchn.fli, type="ts",main="abundance in the catch by age")
plot(catchn.fli, type="internal",main="Cohorts consistence in the catch")

# plot(HKE.idx[[1]], type="ts",main="abundance in the MDTS GSA10 survey by age") 
# plot(HKE.idx[[1]], type="internal",main="Cohorts consistence in the MDTS10 survey") 
#survey #1 MEDITS 10

#if you have more than one index
plot(HKE.idx[[2]], type="ts",main="abundance in the LLS GSA10 survey by age") 
plot(HKE.idx[[2]], type="internal",main="Cohorts consistence in the LLS10 cpue") 
#cpue #2 LLS 10 

plot(HKE.idx[[3]], type="ts",main="abundance in the MDTS GSA9 survey by age") 
plot(HKE.idx[[3]], type="internal",main="Cohorts consistence in the MDTS9 survey")
#survey #3 MEDITS 9

plot(HKE.idx[[4]], type="ts",main="abundance in the MDTS GSA11 survey by age") 
plot(HKE.idx[[4]], type="internal",main="Cohorts consistence in the MDTS11 survey") 
#survey #3 MEDITS 11



####################################
################# VPA ##############
####################################



###########################################
#### Setting F in terminal age and year####
###########################################

# The VPA method estimates population numbers and fishing mortalities
# at age by back-calculating values down each cohort. To do this, the
# method requires initial values of harvest for the terminal age and
# terminal year in the FLStock object. These terminal values must be
# specified by the user prior to running the VPA.
# We can use this information to manually specify the terminal values 
# in the harvest slot. In this instance we will set these values to 0.5

#### AGE ####
harvest(HKE)[ac(range(HKE)["max"])]
harvest(HKE)[ac(range(HKE)["max"]), ] <- 0.5
harvest(HKE)[ac(range(HKE)["max"])]

#### YEAR ####
harvest(HKE)[, ac(range(HKE)["maxyear"])]
harvest(HKE)[, ac(range(HKE)["maxyear"])] <- 0.5
harvest(HKE)[, ac(range(HKE)["maxyear"])]

#################
#### Run VPA ####
#################

HKE.vpa <- VPA(HKE, fratio = 1, fit.plusgroup = T)

# fratio: A numeric value giving tha ratio of the fishing mortality in the
# oldest to that of the next oldest age.
# An F ratio can only be specified when fit.plusgroup is TRUE.Default value is "missing".
# fit.plusgroup:A boolean value specifyng wheter, or not, the oldest age represents
# a plusgroup. The default value is TRUE.

#### Adding results to the stock object ####
HKE.new <- HKE + HKE.vpa

#### Plot main results ####
plot(HKE.new)


## Have a look in stock number and harvest##
harvest(HKE.new) # Fishing mortality by age
HKE.new@harvest[c("5","6")]
z(HKE.new)["0","2006"] # Total mortality estimates
stock.n(HKE.new) # Stock number by age

##########################################################
#### Checking the nb of surving from age t to age t+1 ####
##########################################################

# The numbers surviving from age(a) to age(a+1) are found by multiplying by
# the exponential of the total mortality for that age: Nb_survivors=N(a+1)=N(a)*exp-Z(a)

as.numeric(HKE.new@stock.n["0","2006"])*exp(-(as.numeric(z(HKE.new)["0","2006"])))
stock.n(HKE.new)["1","2007"]

as.numeric(HKE.new@stock.n["2","2008"])*exp(-(as.numeric(z(HKE.new)["2","2008"])))
stock.n(HKE.new)["3","2009"]

as.numeric(HKE.new@stock.n["5","2008"])*exp(-(as.numeric(z(HKE.new)["5","2008"])))
stock.n(HKE.new)["6","2009"]# plugroup effects (sum of Nb belonging to different ages)

## Have a look in stock number andfishing mortality in a time range e.g. 2010:2014 ##
stock.n(HKE.new)[, ac(2010:range(HKE)["maxyear"])]
harvest(HKE.new)[, ac(2010:range(HKE)["maxyear"])]

###############################################################
####################### EXERCISE 1 ############################
#### Run VPA with different Fterminal and compare results #####
###############################################################

###########################################################
######################## Exercise 2 #######################
#### Run VPA with different fratio and compare results ####
###########################################################



####################
#### Tuning VPA ####
####################

# As noted above the VPA method requires user defined terminal estimates
# of fishing mortality. This dependency limits the usefulness of
# the method since it is often the most recent, terminal, estimates that
# are of most concern to fishery managers.

# Additional catch at age and effort information, derived either from a sub component
# of the fishery or from a fishery independent source such as a research survey, 
# can be used to 'tune' the assessment, as described above, and thereby
# obtain better estimates of fishing mortality and stock numbers in
# the most recent years. Several so-called ad hoc techniques for tuning
# a VPA have been developed. Extended Survival Analysis (XSA) is one of the most common.

###############
#### FLXSA ####
###############

# XSA is a more sophisticated method that uses information on individual
# cohort sizes to estimate survivors at each age in the terminal population.
# Although the modelling approach is more involved the method
# requires the same input of catch numbers at age and indices of catch
# per unit effort and it retains at its core the basic VPA method. The
# details of the XSA method are too complex to show here, or to code
# individually as we have for the Laurec-Shepherd approach. Instead
# the FLXSA method has been developed as an additional package to
# FLAssess.

# FLXSA(stock, indices, control, desc, diag.flag=TRUE)

# stock= An object of class FLStock.

# indices= An object of class FLIndices that contains one or more indices of catch at age
# and effort.

# control= An object of class FLXSA.control that specifies the model parameters.

# desc= An optional character string holding a short description of the data and model

# diag.flag= Boolean. If true the method will return the full diagnostics for the analysis. If
# false, only the estimated stock numbers and harvest values will be returned.
# Defalut setting is True.


##################################
#### The FLXSA control object ####
##################################

# The FLXSA.control object contains all of the user defined model
# settings for running an XSA analysis. It can be created in several
# different ways. 
# The simplest method is to accept all of the default settings by calling the FLXSA.control function
# without any extra arguments:

FLXSA.control()

#FLXSA.control(x=NULL, tol=1e-09, maxit=30, min.nse=0.3, fse=0.5,
             # rage=0, qage=10, shk.n=TRUE, shk.f=TRUE, shk.yrs=5, shk.ages=5,
             # window=100, tsrange=20, tspower=3, vpa=FALSE)

# x= An object of class FLXSA. If specified the control object
# is initialised with the same settings as the XSA analysis stored in the object.

# tol= The convergence tolerance. The model is considered to have converged once the
# sum of the absolute differences in terminal F values between two successive
# iterations is less than the specified value.

# maxit= The maximum number of iterations that the model can run

# min.nse= The minimum standard error to be used for inverse variance weghting of the
# survivors estimates.
# 
# fse= User defined standard error when shrinking the mean F
# 
# rage= The oldest age for which the two parameter model is used for determining
# catchability at age
# 
# qage= The age after which catchability is no longer estimated. Catchability at older
# ages will be set to the value of catchability at this age.
# 
# shk.n Boolean. If TRUE apply shrinkage to the population mean. Applies to the
# recruiting ages only.
# 
# shk.f= Boolean. If TRUE apply shrinkage to the mean F.
# 
# shk.yrs= The number of years to be used for shrinkage to the mean F.
# 
# shk.ages= The ages over which shrinkage to the mean F should be applied.
# 
# window= The specific year range for which the model should be run.
# 
# tsrange= The number of years to be used in the time series weighting.
# 
# tspower= The power to be used in the time series taper weighting.
# 
# vpa= Boolean. If TRUE, use VPA to calculate historical values of F and population
# abundance. If FALSE, use cohort approximation.


# Alternatively the default settings can be over-written by specifying
# values at the point of creation or by overwriting them afterwards.

FLXSA.control <- FLXSA.control(maxit = 50, qage = 8)
FLXSA.control=FLXSA.control()
slot(FLXSA.control, "qage") <- as.integer(8)
slot(FLXSA.control, "maxit") <- as.integer(50)
FLXSA.control

############################################################################################
########## FLXSA Control used as best run in the HKE_9-11 official assessment EWG15-11 #####
############################################################################################

# FLXSA.control(x=NULL, tol=1e-09, maxit=30, min.nse=0.3, fse=2,
#               rage=0, qage=5, shk.n=TRUE, shk.f=TRUE, shk.yrs=3, shk.ages=2,
#               window=100, tsrange=20, tspower=3, vpa=FALSE)
##########################################################################################

FLXSA.control=FLXSA.control()
FLXSA.control
slot(FLXSA.control, "rage") <- as.integer(0)
slot(FLXSA.control, "qage") <- as.integer(5)
slot(FLXSA.control, "shk.ages") <-as.integer(2)
slot(FLXSA.control, "shk.yrs") <- as.integer(3)
slot(FLXSA.control, "fse") <- 2
slot(FLXSA.control, "maxit") <- as.integer(30)
FLXSA.control

# This is because the default type numeric cannot be used in this slot. Such
# coercion is not necessary when using the FLXSA.control() function as this check is performed
# internally by the function.

# Once the control object has been created, the XSA analysis can be run as a
# simple one-line command.The FLXSA function returns an object of class FLXSA
# which extends the FLAssess class. The FLXSA object
# contains all of the information in the FLAssess class plus additional information
# specific to the XSA assessment method, such as the survivors estimates and their internal
# and external standard errors. The control object used for the assessment is also
# stored in the returned FLXSA object to provide a record of
# what settings were used for that particular run. All of the settings in the
# returned control object will remain the same except for the maxit slot that
# contains the maximum number of iterations for the analysis.
# This value will be overwritten with the actual number of iterations taken to reach
# convergence, if indeed the model had converged before the maximum number initially specified.



#####################
###### Run XSA ######
#####################

#### TUNING using all indices #####
HKE.xsa <- FLXSA(HKE, HKE.idx, FLXSA.control)
HKE.new_xsa <- HKE + HKE.xsa

#### TUNING by index only for comparison ####
"MDTS_GSA10"
HKE.xsa.t1 <- FLXSA(HKE, HKE.idx[[1]],
                    FLXSA.control)
HKE.xsa_idx1 <- HKE + HKE.xsa.t1

"LLS_GSA10"
HKE.xsa.t2 <- FLXSA(HKE, HKE.idx[[2]],
                    FLXSA.control)
HKE.xsa_idx2 <- HKE + HKE.xsa.t2

#### Plotting main results #####
plot(FLStocks(vpa=HKE.new,XSA = HKE.new_xsa))
wireframe(data~year+age, data=harvest(HKE.new_xsa))
#### Plotting main results adding the two run with using only one index ####
plot(FLStocks(vpa=HKE.new,XSA = HKE.new_xsa,XSA_idx1 = HKE.xsa_idx1,XSA_idx2 = HKE.xsa_idx2))


# Appart from the model diagnostics, the FLXSA method returns two important results,
# namely the estimated values of fishing mortality and population numbers at age.
# These are returned as FLQuants and are stored in the harvest and stock.n slots,
# respectively, of the FLXSA object. These estimated values can be
# very easilly read back into an FLStock object using the + operator.
# Once the results have been read back into a FLStock object we can look at some of the key
# information such as SSB, recruitment and mean fishing mortality values.
# But before concentrating too much on the results of the assessment
# it is advisable to first investigate some of the model diagnostics.


#####################
#### Diagnostics ####
#####################

# There are many diagnostic checks that one might be interested in conducting to examine
# the model fit. The first might be to see if the model has reached convergence within
# the specified number of iterations. 

slot(slot(HKE.xsa, "control"), "maxit")


# Additionally one can check for discrepancies between the internal and
# external standard errors of the survivors estimates
# Very often plots of the catchability residuals are made to inspect for any obvious trends
# or departures from the assumption of constant catchability over time.


# There are several ways to access diagnostic information about your fitted XSA model.
# The easiest is perhaps to use the diagnostics function, which will replicate the
# diagnostic output produced by the original VPA suite (developed in the early 1990's).
# Note that this function merely outputs the results to the screen
# and no object is created by the method. The function was created to allow the user to cut
# and paste the information from the console to a report.
# The output can be quite long, particularly if the assessment
# comprises a large numberof ages and many tuning indices.
# The standard output can be divided roughly into
# eight sections each providing different information about the model and the fit.
# These sections comprise the model dimensions; parameter settings
# regression weights; the estimated fishing mortalities and population
# numbers for the last 10 years; the aggregated survivors estimates;
# the log catchability residuals for each of the tuning indices and finally
# the individual survivors estimates for each year-class represented in the terminal year.
# In order to make this document more readable we will print out only a few sections of
# the diagnostic output at a time (this feature is not yet distributed with FLXSA).
# We can do this by passing a vector of TRUE and FALSE values to the sections argument of
# the diagnostics method. By default all sections are set to
# TRUE so that all of the information is output to the screen.
# In order to reduce the quantity of output further
# we will run a new XSA for a reduced number of ages and with only one tuning index and
# will start by outputting only the dimension information and the parameter settings
# from our diagnostics.
           

HKE.xsa2 <- FLXSA(trim(HKE, age = 1:4), HKE.idx[[3]],
                   FLXSA.control)
diagnostics(HKE.xsa2)
diagnostics(HKE.xsa2, sections = c(T, rep(F,
                                              7)))
diagnostics(HKE.xsa2, sections = c(T, T, rep(F,
                                              6)))
diagnostics(HKE.xsa2, sections = c(T, T, T,rep(F,
                                              5)))

# Next we can output the regression weights and the fishing mortalities
# and population numbers for the last 10 years and also the aggregated survivors estimates.


diagnostics(HKE.xsa2, sections = c(F, F, T, T,
                                    T, T, F, F))

# And finally we can output the catchability residuals and the individual survivors estimates.
# Note that very little thought went into the parameter settings for this particular model
# fit so please don't interrogate the output presented here too closely.
# Also note that we do not normally expect the diagnostics output to be
# broken up as we have here. We present it in this way purely to make it more
# presentable in this document.
# By default all sections are set to TRUE so
# it is very likely that you won't need to give this argument at all
# when calling the diagnostics method.

diagnostics(HKE.xsa2, sections = c(F, F, F, F,
                                    F, F, T, T))

# Very often the quickest and simplest way to determine the fit of the model is through
# visual inspection of the various diagnostic outputs.
# Here we provide examples of how to extract the relevant information from the
# return FLXSA object and to plot it using a variety of lattice functions available to R.
# We start by plotting the log catchability residuals at age from each of the three tuning series.
# The data are stored as an FLQuants object in the index.res slot of the FLXSA object
# First we need to assign names to each of the FLQuant objects so we know which fleet
# they represent


names(HKE.xsa@index.res) <- names(HKE.idx)
pfun <- function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.loess(x, y, ...)
  panel.abline(h = 0, col = "grey", lty = 2)
}
plot(xyplot(data ~ year | ac(age) + qname, data = index.res(HKE.xsa),
            panel = pfun))


#### RESIDUALS BUBBLE PLOTS ####
index.res(HKE.xsa)
bubbles(age ~ year|qname, data = index.res(HKE.xsa)
        , main = "Proportion at age by year Sh0.5")
####################################################

res_fin=as.data.frame(index.res(HKE.xsa))

xyplot(data~age,type="p",groups=year,data=res_fin,main="residuals by year",
       auto.key=list(corner=c(1,1), columns=1,
                     title="Year", cex.title=1))

xyplot(data~year,type=c('l','p'),groups=age,data=res_fin,main="residuals by age",
       auto.key=list(corner=c(1,1), columns=1,title="Age", cex.title=1),
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0,col="green",lwd=2)
       })

bubbles(age ~ year, data = index(HKE.idx[[1]]), main = "Proportion at age by year in the survey")

bubbles(age ~ year, data = catch.n(HKE.new_xsa), main = "Proportion at age by year in the catch")


# A simple comparison of the terminal year survivors estimates can be obtained
# from the information stored in the diagnostics slot of the FLXSA object.
# In the following example we first extract the information relevant
# to the survivors estimates in the final year and store it as a temporary object.
# The weights values contained in this data set are the raw fleet based weights that
# have been calculated from the standard errors of the fleet based survivors estimates at each age
# in the cohort. To aid visualisation and to see the relative contribution
# of each fleets estimate to the final estimated value of survivors
# we re-scale the weights to a maximum value of 1 and plot both the fleet
# based survivors estimates from each fleet and their scaled weight. The results
# show relatively consistent estimates of survivors from all fleets across most ages.
# The scaled weights show the some series to have the greatest influence on the terminal estimates
# at the younger ages whilst others have greater influence at older ages and that throughout
# all ages F shrinkage recieves very little weighting.

# kk <- slot(HKE.xsa,"diagnostics")[is.element(slot(HKE.xsa,"diagnostics")$year, 2014), ]
# kk <- cbind(kk, w.scaled = kk$w/rep(tapply(kk$w, kk$yrcls, sum),c(table(kk$yrcls))))
# nplot <- barchart(ac(yrcls) ~ nhat, groups = source, data = kk,
#                     col = grey(c(0.1, 0.6, 0.3, 0.8, 1)), main = "N Estimates",
#                     ylab = "Year Class", key = list(x = 0.6, y = 0.2, 
#                     text = list(legend =rev(c("BTS-Isis","BTS-Tridens", "fshk", "SNS","pi"))),
#                      rectangles = list(col = grey(rev(c(0.1,0.6, 0.3, 0.8, 1))))))
# wplot <- barchart(ac(yrcls) ~ w.scaled, groups = source, data = kk,
#                     col = grey(c(0.1, 0.6, 0.3, 0.8,1)), main = "Scaled Weights",
#                     ylab = "", xlab = "Relative Weight")
# print(nplot, position = c(0, 0, 0.5, 1), more = TRUE)
# print(wplot, position = c(0.5, 0, 1, 1))

#############################
### Sensitivity analysis ####
#############################

# The simplified calling format of FLXSA makes it very easy to run multiple analyses to
# investigate model sensitivity to parameter settings.
# A wide variety of such investigations are possible.
# In this simple example we will look at the effect that different F shrinkage standard errors
# have on the terminal estimates of fishing mortality.
# We start by creating a vector of F shrinkage values to be used in the anlyses and by creating an
# FLQuant with sufficient dimensions to store the results.
# To do this we use the propagate function to
# extend an FLQuant in the 6th dimension by the number of runs that we are going to perform.
# The estimates of fishing mortality for each XSA run are stored in the FLQuant using
# the 6th dimension to hold each iteration.
# The results show little sensitivity to increasing F shrinkage values.

#### Looping XSA with different fse values and store harvest
fsevals <- seq(0.5, 2.5, by = 0.5)
res <- propagate(harvest(HKE), length(fsevals))
for (i in 1:length(fsevals)) {
  xsa.control <- FLXSA.control(fse = fsevals[i])
  iter(res, i) <- harvest(FLXSA(HKE, HKE.idx,
                                xsa.control))
}
# plot(xyplot(data ~ year | age, groups = iter,
#             data = res, type = "l", col = "black", xlim = c(2006:2014)))

res_df=as.data.frame(res)

xyplot(data~year|age,groups=iter,data=res_df, xlab="year", ylab="fishing mortality",
       main="Sensitivity by different fse values",type=c('l','p'),
       auto.key=list(corner=c(1,1), columns=1,
                     title="iter_fse", cex.title=1))


#### Looping XSA with different fse values
fsevals <- seq(0.5, 3, by = 0.5)
XSA_f <- list()
for (i in 1:length(fsevals)) {
  xsa.control <- FLXSA.control(fse = fsevals[i])
  XSA <- FLXSA(HKE, HKE.idx,xsa.control)
XSA_f[i]=HKE+ XSA
  }
XSA_f
plot(FLStocks(XSA_f))



# An important diagnostic check is to see how the estimated values vary as the time series
# of the input data changes.
# We can make use of existing R functions to apply the same assessment model to successively
# truncated the time series of input data. In this example we are using window to truncate
# the FLStock object to the specified year range, the + operator to pass the results of the XSA
# into the FLStock object and the tapply function to perform this action over the year range 2011:2014.
# Note that the resulting object, called stk.retro, is of class FLStocks
# i.e. a list of FLStock objects, each one having a separate year range

## Retrospective analysis ##
retro.years <- 2011:2014# retro years range
stk.retro <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.new_xsa,end=x)+FLXSA(window(HKE.new_xsa,end=x),HKE.idx, FLXSA.control)))
stk.retro<- FLStocks(stk.retro)
# stk.retro@names=c("2013","2014","2015")
stk.retro@names=ac(unique(retro.years))
plot(stk.retro)


#######################################################################
####################### EXERCISE 3 ####################################
#### Run XSA with different Fcontrol settings and compare results #####
#######################################################################


###########################################################
####################### EXERCISE 4 ########################
#### Run XSA with different Fcontrol settings and based on 
# diagnostics choose the best model #####
##########################################################


########## FINAL OUTPUTS ############
(TOTAL_BIOMASS=quantSums(stock.n(HKE.new_xsa)*stock.wt(HKE.new_xsa)))
(TOTAL_CATCH=quantSums(catch.n(HKE.new_xsa)*catch.wt(HKE.new_xsa)))
(SSB=ssb(HKE.new_xsa))
(RECRUITS=rec(HKE.new_xsa))
(FISHING_MORTALITY=fbar(HKE.new_xsa))
(FISHING_MORTALITY_AGE=harvest(HKE.new_xsa))
(TOTAL_MORTALITY=z(HKE.new_xsa))
(STOCK_NB=stock.n(HKE.new_xsa))
plot(harvest(HKE.new_xsa))
plot(HKE.new_xsa)



###Final comparison Fcurr(f_bar) against reference point (f0.1)

#f0.1=c(refpts(yprec)["f0.1","harvest"]) # if FBRP has been already used
#fmax=c(refpts(yprec)["fmax","harvest"]) # if FBRP has been already used

f0.1=0.25 # as an example
fmax=0.35 # as an example
fbar(HKE.new_xsa)/f0.1

F_bar=as.data.frame(fbar(HKE.new_xsa))
F_bar=cbind(F_bar,f0.1,fmax)

plot(F_bar$year,F_bar$data,type="b",col="red",main = "HKE_GSA9-11",
     ylim=c(0,max(F_bar$data)+0.5),ylab=" F_current",xlab="year",lwd=3)
lines(F_bar$year,F_bar$f0.1,col="green",lwd=4)
legend(min(F_bar$year),max(F_bar$data)+0.5, # places a legend at the appropriate place 
       c("Fcurr","F0.1"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines
       lwd=c(2.5,2.5),col=c("red","green")) # gives the legend lines the correct color and width


###################################################################
####################### EXERCISE 5 ################################
#### Run XSA with different Fbar settings and compare results #####
###################################################################


###########################################################################
####################### EXERCISE 6 ########################################
#### Run XSA with different  natural mortality values (vector or costant)
# and compare results #####
###########################################################################



# to save the stock object:
HKE_final=HKE.new_xsa
HKE_indices_final=HKE.idx
save(HKE.new_xsa, file="HKEFbar0_3.Rdata")
## change the file name output on the basis of your stock
save(HKE.idx, file="HKEFbar0_3_idx.Rdata")#
save.image(file="Ple4Fbar1_6.RData")



############ END OF THE SCRIPT #############