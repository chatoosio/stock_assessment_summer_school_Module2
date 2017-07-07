# MSE.R - DESC
# MSE.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLash)
library(FLBRP)
library(FLa4a)
library(FLAssess)

data(ple4)
data(ple4.indices)

# Settings

nits <- 25
yrs <- 10

# OBJECTIVES

# Target
#  - B_MSY

# Limit
#  - 0.20 * B_MSY

# Risk
#  - P(B_end < B_MSY) < 20%

# Performance indicators
#  - P(B < B_MSY)
#  - Var(catch)


# OM Conditioning

# Uncertainty in M
shape <- FLModelSim(model=~exp(-age-0.5))

level <- FLModelSim(model=~k^0.66*t^0.57, params = FLPar(k=0.4, t=10), vcov=matrix(c(0.002, 0.01,0.01, 1), ncol=2))

trend <- FLModelSim(model=~b, params=FLPar(b=0.5), vcov=matrix(0.02))

m4 <- a4aM(shape=shape, level=level, trend=trend)
m4 <- mvrnorm(nits, m4)

range(m4,c("minmbar","maxmbar"))<-c(1,1)
range(m4,c(1:5)) <- range(ple4,c(1:5))

flq <- m(m4)[]

com <- propagate(ple4, nits)
m(com) <- flq

# OM fit
com <- a4aSCA(com, ple4.indices, srmod=~bevholt(CV=0.1))

om <- ple4 + com
srm <- fmle(as.FLSR(om, model=bevholt))

save(om, srm, file='om.RData')
load('om.RData')

# Target and Limit (ICES)
target <- 230000
limit <- 50000


# Prepare for simulations
om <- stf(om, yrs)

# MP

for (i in seq(2009, length=yrs)) {

	cat('## --- ', i, ' ---\n')

	# Data collection

	caa <- window(catch.n(om), end=i-1)

	idx <- window(stock.n(om)[-c(1:3)], end=i-1)
	idx <- idx + rnorm(length(idx), 0, 0.1)

	# SA

	stk <- window(om, end=i-1)
	catch.n(stk) <- caa

	ind <- FLIndex(index=idx, name='ind')
	range(ind)[c('startf', 'endf')] <- c(0.5, 0.5)

	sa <- sca(stk, FLIndices(ind=ind),
		fmodel=~s(age, k=4) + s(year, k=30),
		qmodel=list(~s(age, k=4)))
	stk <- stk + sa

	# ssr <- fmle(as.FLSR(stk, model=bevholt), control=list(trace=0))

	# HCR

	tac <- c(catch(stk[,ac(i-1)]))
	st <- c(stock(stk[,ac(i-1)]))

	tac[st >= target]  <- tac[st >= target] * 1.25
	tac[st < target]  <- tac[st < target] * 0.85

	# Management
	
	# fwdControl
	ctl <- fwdControl(data.frame(year=i, quantity='catch', val=median(tac)),
		trgtArray=array(c(rbind(NA, tac, NA)), dim=c(1, 3, nits),
		dimnames=list(1, c('min','val','max'), iter=seq(nits))))

	om <- fwd(om, ctl, sr=srm)

}

save(om, file='omRUN.RData')

# RESULTS

# Stock trajectory ##### doesn't work... it misses rp
plot(om, rp['msy']) + geom_vline(aes(xintercept=2008), colour="red", linetype=2)

# Performance
sum(stock(om)[,ac(i)] > st) / nits
yearMeans(catch(om)[,ac(2009:2018)])
