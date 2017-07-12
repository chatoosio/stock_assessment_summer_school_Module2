#
# Day 2

#  Surplus Production in Continuous-Time (SPICT)
library(spict)

# Data Structure


# Build spict stock

# Let's work with a good time series for Anchovy in GSA 6
# Load the .csv file

ane <- read_csv("~/GitHub/stock_assessment_summer_school_Module2/Material/stocks_for_course/ANE_6_notAge/ANE_06_nonAge.csv")
# Assemple stock

ane6 <- vector("list")


ane6$obsC  <- ane$catch  #catch observations
ane6$timeC <- ane$year   # time of catch observations


# TUNING INDEXES
# Pick surveys index, in this case an acoustic biomass index

ane6$timeI <- list(  # time of index
  ane$year) # Index 1
# Index 2,
# Index 3,
# etc )


ane6$obsI <- list() # index observations

ane6$obsI[[1]] <- ane$index
#ane6$obsI[[2]] <- Index 2
#ane6$obsI[[3]] <- Index 3
#ane6$obsI[[4]] <- Index 4

# Inspect the file
ane6

# Plot file

x11()

plotspict.data(ane6)

plotspict.ci(ane6)

# Fit base model
ane6fit <- fit.spict(ane6)

# Explore results
capture.output(summary(ane6fit))



# Priors

# CVs

# Run spict

jpeg(file = "ane6fit#1.jpg", bg = "white",  width = 1200, height = 1200,units = "px", pointsize = 25,      quality = 100)
plot(ane6fit)
dev.off()

ane6fit_diagn <- calc.osa.resid(ane6fit)

jpeg(file = "ane6fit_diagnostics#1.jpg", bg = "white",  width = 1200, height = 1200,units = "px", pointsize = 25,      quality = 100)
plotspict.diagnostic(ane6fit_diagn)
dev.off()

# Short term prediction
sumspict.predictions(ane6fit)


# Fit Summary
summary(ane6fit)

jpeg(file = "ane6fit_forecast#1b.jpg", bg = "white",  width = 1200, height = 1200,units = "px", pointsize = 25,      quality = 100)
par(mfrow=c(3, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(ane6fit)
plotspict.ffmsy(ane6fit, qlegend=FALSE)
plotspict.catch(ane6fit, qlegend=FALSE)
plotspict.fb(ane6fit, man.legend=FALSE)
plotspict.tc(ane6fit)
dev.off()



ane6fit <- manage(ane6fit)
head(mansummary(ane6fit))


ane6fit_nopriors <- retro(ane6fit, nretroyear = 4)

jpeg(file = "ane6_retro#1.jpg", bg = "white",  width = 1200, height = 1200,units = "px", pointsize = 25,      quality = 100)
plotspict.retro(ane6fit_nopriors)
dev.off()



# Diagnostics

# Exercize

# Short term


