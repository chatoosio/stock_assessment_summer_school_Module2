library(FLa4a)
library(XML)
data(rfLen)
niters <- 1

#====================================================================
# EDA ...
#====================================================================

#====================================================================
# growth model
#====================================================================

#--------------------------------------------------------------------
# get lh pars for redfish from fishbase
#--------------------------------------------------------------------

addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=501"
# Scrape the data
tab <- try(readHTMLTable(addr))
# Interrogate the data table and get vectors of the values
linf <- as.numeric(as.character(tab$dataTable[,2]))
k <- as.numeric(as.character(tab$dataTable[,4]))
t0 <- as.numeric(as.character(tab$dataTable[,5]))
# vec with pars
p <- c(linf=median(linf, na.rm=T), k=median(k, na.rm=T), t0=0)

#--------------------------------------------------------------------
# build a covariance matrix
#--------------------------------------------------------------------

cm <- diag(c(1,1,1))
# k and linf are negatively correlated while t0 is independent
cm[1,2] <- cm[2,1] <- -0.7
# scale cor to var using CV=0.1
cv <- 0.1
vc <- matrix(1, ncol=3, nrow=3)
l <- vc
l[1,] <- l[,1] <- p[1]*cv
kk <- vc
kk[,2] <- kk[2,] <- p[2]*cv
t <- vc
t[3,] <- t[,3] <- 0.1
mm <- t*kk*l
diag(mm) <- diag(mm)^2
mm <- mm*cm
# check that we have the intended correlation
all.equal(cm, cov2cor(mm))

#--------------------------------------------------------------------
# create the gr object and simulate
#--------------------------------------------------------------------

vbObj <- a4aGr(grMod=~linf*(1-exp(-k*(t-t0))), 
			   grInvMod=~t0-1/k*log(1-len/linf), 
			   params=FLPar(linf=p["linf"], k=p["k"], t0=0, 
			   			 units=c("cm","year-1","year")), 
			   vcov=mm)

# Simulate using mvrtriangle
triPars <- list(list(a=min(linf), b=max(linf), c=median(linf)),
				list(a=min(k), b=max(k), c=median(k)),
				list(a=0-0.1, b=0+0.1))

set.seed(1234)
vbTri <- mvrtriangle(niters, vbObj, paramMargins=triPars)

# plots
# par(mfrow=c(3,1))
# hist(c(params(vbTri)["linf",]), main="linf", xlab="")
# hist(c(params(vbTri)["k",]), main="k", prob=TRUE, xlab="")
# hist(c(params(vbTri)["t0",]), main="t0", xlab="")
# splom(data.frame(t(params(vbTri)@.Data)), pch=".")
# par(mfrow=c(1,1))
boxplot(t(predict(vbTri, t=0:50+0.5)))

#====================================================================
# converting lengths to ages
#====================================================================
stk0 <- l2a(rfLen.stk, vbTri, plusgroup=25)
stk <- qapply(stk0, seasonMeans)
catch.n(stk) <- seasonSums(catch.n(stk))
catch.n(stk)[catch.n(stk)==0] <- 0.1
catch(stk) <- computeCatch(stk)
landings(stk) <- catch(stk)
landings.n(stk) <- catch.n(stk)
landings.wt(stk) <- catch.wt(stk)
discards(stk)[] <- discards.wt(stk)[] <- discards.n(stk)[] <- 0

idx <- l2a(rfTrawl.idx, vbTri)
idx <- idx[ac(0:10)]
idx@range[c("plusgroup", "startf", "endf")] <- c(10, 0, 1)
idxs <- FLIndices(myIdx=idx)

#====================================================================
# adding M
#====================================================================

# level
level2 <- FLModelSim(model=~1.5*k, params=FLPar(k=p["k"]))

# shape
shape2 <- FLModelSim(model=~exp(-age-0.5))

# trend
nao.orig <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=64, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2011, unit="unique", season=1:12, area="unique")
nao.flq <- FLQuant(unlist(nao.orig[,-1]), dimnames=dnms, units="nao")
dim(nao.flq)
nao <- seasonMeans(nao.flq[,,,1:3]) 
dim(nao)
nao <- (nao>0) * 1
trend3 <- FLModelSim(model=~1+b*nao, params=FLPar(b=0.5))

# M model
m3 <- a4aM(shape=shape2, level=level2, trend=trend3)
rngage(m3) <- range(stk)[c("min", "max")]
rngyear(m3) <- range(stk)[c("minyear", "maxyear")]

flq <- m(m3, nao = c(nao[, ac(range(stk)["minyear"]:range(stk)["maxyear"])]))
dimnames(flq)[1:5] <- dimnames(m(stk))[1:5] 

# update stock with new M
m(stk) <- flq

#====================================================================
# run assessment
#====================================================================

# assessment method
fm <- ~ s(age, k=3) + s(year, k=6) + te(age, year, k=c(3, 5))
fit <- sca(stk, idxs, fmodel=fm, fit="assessment")
res <- residuals(fit, stk, idxs)
plot(res)
wireframe(data~age+year, data=as.data.frame(harvest(fit)))
stk1 <- stk + fit
plot(stk1)

# stock recruitment
sr1 <- fmle(as.FLSR(stk1, model="bevholt"))
plot(sr1)

# biol ref points
rp1 <- brp(FLBRP(stk1, sr=sr1))

#====================================================================
# forecast
#====================================================================

#====================================================================
# stock status
#====================================================================
