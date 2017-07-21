#' ---
#' title: "Short-term Forecasts, Summer School in Quantative Fisheries Stock Assessment, Capo Granitola"
#' author: "Alessandro Ligas"
#' date: "July 13th, 2017"
#' ---


#let's clear the space
# rm(list=ls())


# Load the libraries
library(FLCore)
library(FLAssess)
library(FLash)
library(ggplotFL)
# library(FLEDA)
library(FLXSA)
require(plyr)
require(FLBRP)

# STF
# Here we run the STF for 3 years, 2015, 2016, 2017
# You can change these as appropriate
# The first year of the STF should be the next one after the final year in your stock data
# For example, the final year in the HKE09_10_11 stk object is 2014 so the first year of the STF is 2015

# load the stock object
load("HKE9_10_11.xsa.RData")
hke<-HKE.stk

stf_years <- c(2015,2016,2017)
no_stf_years <- length(stf_years)

# For the STF we would like to run a F0.1 scenario
# Use FLBRP to get F0.1       #SET RP ACCORDING TO THE LAST ASSESSMENT RUN
stk_brp <- brp(FLBRP(hke))
refpts(stk_brp)
f01 <- c(refpts(stk_brp)["f0.1","harvest"])
# f01=0.195 # please verify that f0.1 is similar to that estimated by the assessment


# We also need F status quo - the geometric mean of the last X years
# Here we use 3 years
no_stk_years <- dim(rec(hke))[2]
no_fbar_years <- 3 # Or set your own as appropriate
fbars <- fbar(hke)[,(no_stk_years - no_fbar_years + 1):no_stk_years]
fbar_status_quo <- exp(mean(log(c(fbars))))
fbar_status_quo
ssb(hke)
catch(hke)



# Set up the future stock object.
# Here we use the default assumptions about what happens to weights, maturity and selection pattern in the future
# (e.g. weights are means of the last 3 years)
# NOTE: You may want to change some of these assumptions by hand
# See the help page for stf: ?stf for more details
stf_stk <- stf(hke, nyears = no_stf_years, wts.nyears = 3)

# Set up future recruitment to be mean of last X years
# Here we set as geometric mean of the last 3 years
no_rec_years <- 3 # Change number of years as appropriate
recs <- rec(hke)[,(no_stk_years - no_rec_years + 1):no_stk_years]
mean_rec <- exp(mean(log(c(recs))))

# We are going to run several STFs with different values for the fbar_multiplier
# The fbar_multiplier ranges from 0.1 to 2 by 0.1
fbar_multiplier <- seq(from = 0, to = 2, by = 0.1)

# We are going to build a data.frame that builds these scenarios
# Each column in the dataframe is a year
# Each row is a scenario
# Set up the fbar scenarios - note that if you project for more than 3 years you will need to add more columns / years to the matrix
fbar_scenarios <- cbind(rep(fbar_status_quo,length(fbar_multiplier)),
                        fbar_multiplier*fbar_status_quo,
                        fbar_multiplier*fbar_status_quo)
# Add the F0.1 scenario as a final scenario
fbar_scenarios <- rbind(fbar_scenarios, c(fbar_status_quo,f01,f01))

# There are various results we want to extract from the STF
# Make an empty matrix in which to store the results
stf_results <- matrix(NA,nrow = nrow(fbar_scenarios),ncol = 10) # change ncol if needed
# Change the column names to reflect years
colnames(stf_results) <- c('Ffactor','Fbar','Catch_2014','Catch_2015','Catch_2016','Catch_2017','SSB_2016','SSB_2017','Change_SSB_2016-2017(%)','Change_Catch_2014-2016(%)')

# Store the FLStock each time
stk_stf <- FLStocks()
# Loop over the scenarios
for (scenario in 1:nrow(fbar_scenarios)) {
  cat("Scenario: ", scenario, "\n")
  # Make a target object withe F values for that scenario
  ctrl_target <- data.frame(year = stf_years,
                            quantity = "f",
                            val = fbar_scenarios[scenario,])
  # Set the control object - year, quantity and value for the moment
  ctrl_f <- fwdControl(ctrl_target)
  # Run the forward projection. We include an additional argument, maxF.
  # By default the value of maxF is 2.0
  # Here we increase it to 10.0 so that F is not limited
  stk_stf_fwd <- fwd(stf_stk, ctrl = ctrl_f, sr = list(model="mean", params=FLPar(a = mean_rec)), maxF = 10.0)
  ## Check it has worked - uncomment out to check scenario by scenario
  #plot(stk_stf_fwd)
  # Store the result - if you want to, comment out if unnecessary
  stk_stf[[as.character(scenario)]] <- stk_stf_fwd
  
  # Fill results table
  stf_results[scenario,1] <- fbar_scenarios[scenario,2] / fbar_scenarios[scenario,1] # fbar status quo ratio
  stf_results[scenario,2] <- fbar(stk_stf_fwd)[,ac(2017)] # final stf year
  stf_results[scenario,3] <- catch(stk_stf_fwd)[,ac(2014)] # last 'true' year
  stf_results[scenario,4] <- catch(stk_stf_fwd)[,ac(2015)] # 1st stf year
  stf_results[scenario,5] <- catch(stk_stf_fwd)[,ac(2016)] # 2nd stf year
  stf_results[scenario,6] <- catch(stk_stf_fwd)[,ac(2017)] # final stf year
  stf_results[scenario,7] <- ssb(stk_stf_fwd)[,ac(2016)] # 2nd stf year
  stf_results[scenario,8] <- ssb(stk_stf_fwd)[,ac(2017)] # final stf year
  # Change in SSB
  stf_results[scenario,9] <- (ssb(stk_stf_fwd)[,ac(2017)]-ssb(stk_stf_fwd)[,ac(2016)])/ssb(stk_stf_fwd)[,ac(2016)]*100 # change in ssb in last two stf years
  stf_results[scenario,10] <- (catch(stk_stf_fwd)[,ac(2016)]-catch(stk_stf_fwd)[,ac(2014)])/catch(stk_stf_fwd)[,ac(2014)]*100 # change in catch from true year, to 2nd to last stf year
}
#}
# Look at the table of results
stf_results
# export this if necessary
write.csv(stf_results, file="stf_results2015_2017.csv")

# Plotting
# Plotting is not necessary for the report but here is a crude one anyway
plot(window(stk_stf, start=2014, end=2017))


########     END OF SCRIPT ######################################################


