# Spict Exercize

#########################################################################################
# Exercize 1

# 1 Trim catch time series to start the year when the index starts, fit the best model you can
#  - does it converge?
#  - if yes, does it change the paramters and the perception of the stocks ?

ane6$obsC <- ane6$obsC[c(59:72)]
ane6$timeC <- ane6$timeC[c(59:72)]


plotspict.data(ane6)

ane6TRUNC <- fit.spict(ane6)

# Explore convergence
capture.output(summary(ane6TRUNC))[1:4]

# Calculate residuals and main diagnostics

ane6fitT_diagn <- calc.osa.resid(ane6TRUNC)

plotspict.diagnostic(ane6fitT_diagn)

# Retrospective analysis
# Diagnostics, run it by taking away the last 4 years, one at a time
ane6fitT_retro <- retro(ane6TRUNC, nretroyear = 4)

# now plot it!
plotspict.retro(ane6fitT_retro)

# So the model fits well, diagnostics are good, we can have a look at the final results.
# Fit Summary
summary(ane6TRUNC)

x11()
plot(ane6TRUNC)

######################################################################################
# EXERCIZE 2

# 1) Run a spict model with fishing effort

# 2) Is it converging? 

# 3) Hhow are the diagnostics? 

# 4) Is the fit better?


plotspict.data(ane6effort)
ane6effortfit <- fit.spict(ane6effort)

capture.output(summary(ane6effortfit))[1:4]

plot(ane6effortfit)

ane6effortt_diagn <- calc.osa.resid(ane6effortfit)

plotspict.diagnostic(ane6effortt_diagn)

plotspict.diagnostic(ane6effortfit)

