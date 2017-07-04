library(FLash)
library(FLAssess)
data(ple4)

# cost function f1
f1c <- function(f,q,b) (f/q)^b

# cost function f2
f2c <- function(f,q,b) b*(f/q)

# price function
pf <- function(x,a,x0) a*exp(x)/(exp(x)+exp(x0))

# f allocating function
nyf <- function(f1,f2,lambda) (1-lambda)*f1+lambda*(f2)

# age vector
av <- c(1:10)

# partial fishing mortalities
sv <- exp(av)/(exp(av)+exp(5))
sflq <- catch.n(ple4)
sflq[] <- sv
f1y <- sflq*catch.n(ple4)
f2y <- catch.n(ple4)-f1y
f1f <- sflq*harvest(ple4)
f2f <- harvest(ple4)-f1f

flqs <- FLQuants(f1=f1y,f2=f2y)
xyplot(data~age|year, groups=qname, data=flqs, type="l", scales=list(y=list(relation="free")))

flqs <- FLQuants(f1=f1f,f2=f2f)
xyplot(data~age|year, groups=qname, data=flqs, type="l", scales=list(y=list(relation="free")), main="partial fishing mortalities", auto.key=list(columns=2, points=FALSE, lines=TRUE))

# prices
pflq <- catch.n(ple4)
pflq[] <- pf(1:10, 3, 2)

# revenues & costs

f1r <- quantSums(f1y*pflq)
f1cst <- f1c(quantMeans(f1f), 0.006, 3)
xyplot(data~year, groups=qname, data=FLQuants(r=f1r,c=f1cst), type="l", auto.key=list(columns=2, points=FALSE, lines=TRUE))

f2r <- quantSums(f2y*pflq)
f2cst <- f2c(quantMeans(f1f), 0.0001, 200)
xyplot(data~year, groups=qname, data=FLQuants(r=f2r,c=f2cst), type="l", auto.key=list(columns=2, points=FALSE, lines=TRUE))

# Simulation
yrs <- 2009:2018

# status quo
ple4_mtf <- stf(ple4, nyears = 10)
ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"))
f_status_quo <- mean(fbar(ple4)[,as.character(2005:2008)])

ctrl_target <- data.frame(year = yrs,
			  quantity = "f",
			  val = f_status_quo)

ctrl_f <- fwdControl(ctrl_target)
ple4_f_sq <- fwd(ple4_mtf, ctrl = ctrl_f, sr = ple4_sr)

# Optimization of effort allocation

foo <- function(lambda, stock, sr, yrs){ 
  harvest(stock)[,ac(yrs)] <- yearMeans(nyf(f1f, f2f, lambda))
  stock <- fwd(stock, ctrl = ctrl_f, sr = sr)
  fb <- fbar(stock)[,ac(yrs)]
  cst <- f1c((1-lambda)*fb, 0.006, 3) + f2c(lambda*fb, 0.0001, 200)
  rev <- pflq[,1,drop=T]%*%catch.n(stock)[,ac(yrs),drop=T]
  -(sum(rev)-sum(cst))
  #-ssb(stock)[,"2018"]
  #-sum(cst)
  }

bestlambda <- optim(0.8, foo, method="Brent", lower=0, upper=1, stock=ple4_mtf, sr=ple4_sr, yrs=yrs)

