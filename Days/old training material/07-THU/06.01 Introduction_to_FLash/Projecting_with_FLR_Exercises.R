# Projecting with FLR: An introduction to FLash
# Exercises

# Copyright 2013 JRC FISHREG. Distributed under the GPL 2 or later
# Maintainer: JRC FISHREG
# $Id: $
# Created: 20/03/2013
# Modified:

#------------------------------------------------------------------------------------- 
# A short term forecast
#------------------------------------------------------------------------------------- 
# A short term forecast (STF) typically involves a three year projection
# where the future F is the mean of the last 3 years
# The recruitment is also usually set as the geometric mean of the last 3 years
# Set this up for the ple4 object and run it

# Example solution
# First set up the FLSR object
ple4_sr <- as.FLSR(ple4, model="geomean")
# What happens if you fit it?
ple4_sr <- fmle(ple4_sr)
# Looks OK but we want the mean of the last 3 years only
# So we set the parameter value by hand
params(ple4_sr)
params(ple4_sr)["a"] <- exp(mean(log(rec(ple4)[,as.character(2006:2008)])))
# Can plot it, but beware that the residuals will be wrong as we have not fitted the 'a' value
plot(ple4_sr)

mean_f <- mean(fbar(ple4)[,as.character(2006:2008)])

ple4_stf <- stf(ple4,3)
ctrl_stf <- fwdControl(data.frame(year=2009:2011,
				  quantity = 'f',
				  val = mean_f))
ple4_stf <- fwd(ple4_stf, ctrl=ctrl_stf, sr=ple4_sr)
plot(window(ple4_stf, start = 2000, end = 2011))


#------------------------------------------------------------------------------------- 
# A recovery HCR
#------------------------------------------------------------------------------------- 

# Set up a projection for 10 years, where SSB in 2009 increases by 10% each year, until F = F0.1
# Use a Beverton Holt stock recruitment function
# Assume that F0.1 = 0.09

# Beware of what years you set
# With SSB targets, remember that the target year is actually the one BEFORE you want the SSB target
# But... rel.year is the SSB year you want to be relative to
# (This is hard to explain but have a go and we'll look at an example solution)

# Example solution
yrs <- 2009:2018
ple4_sr <- fmle(as.FLSR(ple4,model="bevholt"))
ple4_stf <-stf(ple4, length(yrs))
f0.1 <- 0.09
ctrl <- fwdControl(data.frame(year=rep(yrs-1, each=2), 
                              rel.year=rep(c(0,NA),length(yrs))+rep(yrs-1,each=2), 
			      # Be careful with years!
			      # 'val' years are the year to set F
			      # but 'rel.year' is the year of the actual target we want to be relative to
			      # So the following line might look right, but is not...
                              #rel.year=rep(c(0,NA),length(yrs))+rep(yrs-2,each=2), 
                              val=rep(c(1.1, NA), length(yrs)), 
                              min=rep(c(NA, f0.1), length(yrs)),
			      quantity=c("ssb","f")))
recovery   <-fwd(ple4_stf, ctrl=ctrl, sr=ple4_sr)
plot(window(recovery, start=1999, end=2017))
#------------------------------------------------------------------------------------- 
# Bonus recipe! Mystery cake!
#------------------------------------------------------------------------------------- 
# Multiple ABSOLUTE targets in the same year
# What happens if you set ABSOLUTE catch AND SSB targets that are incompatible
future_ssb <- 300000
future_catch <- 150000
# Set up two control objects - note the slight difference
ctrl_ssb_catch <- fwdControl(
	data.frame(
		year = c(2008,2008,2009,2009,2010,2010),
		quantity = c("ssb","catch","ssb","catch","ssb","catch"),
		val = c(future_ssb,future_catch,future_ssb,future_catch,future_ssb,future_catch)))

ctrl_catch_ssb <- fwdControl(
	data.frame(
		year = c(2008,2008,2009,2009,2010,2010),
		quantity = c("catch","ssb","catch","ssb","catch","ssb"),
		val = c(future_catch,future_ssb,future_catch,future_ssb,future_catch,future_ssb)))
# Project them both
ple4_fwd_ssb_catch <- fwd(ple4_stf, ctrl_catch_ssb, sr = ple4_sr)
ple4_fwd_catch_ssb <- fwd(ple4_stf, ctrl_ssb_catch, sr = ple4_sr)
# What happened?
ssb(ple4_fwd_ssb_catch)
catch(ple4_fwd_ssb_catch)
# Compared to...
ssb(ple4_fwd_catch_ssb)
catch(ple4_fwd_catch_ssb)
# What have we learned?
# The order of targets in the control object is molto importante
# The first target takes precedence
# The second target (and maybe the third etc) are only met AFTER the preceding target has been met.
# And, as we see here, this is not always possible
