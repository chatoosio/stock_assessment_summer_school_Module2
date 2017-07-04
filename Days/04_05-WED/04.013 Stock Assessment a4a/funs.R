# quant 2 quant
qt2qt <- function(object, id=5, split="-"){
	qt <- object[,id]
	levels(qt) <- unlist(lapply(strsplit(levels(qt), split=split), "[[", 2))
	as.numeric(as.character(qt))
}

# check import and massage
cim <- function(object, n, wt, hrv="missing"){
	v <- object[sample(1:nrow(object), 1),]
	c1 <- c(n[as.character(v$V5),as.character(v$V1),1,as.character(v$V2)]==v$V6)
	c2 <- c(wt[as.character(v$V5),as.character(v$V1),1,as.character(v$V2)]==v$V7)
	if(missing(hrv)){
		c1 + c2 == 2	
	} else {
		c3 <- c(hrv[as.character(v$V5),as.character(v$V1),1,as.character(v$V2)]==v$V8)
		c1 + c2 + c3 == 3	
	}
}

# plot S4 classes
plotS4 <- function(object, linktext="typeof", main="S4 class", ...){
	args <- list(...)
	obj <- getClass(as.character(object))
	df0 <- data.frame(names(obj@slots), unlist(lapply(obj@slots, "[[", 1)))
	nms <- c(t(df0))
	nslts <- length(nms)/2
	M <- matrix(nrow = length(nms), ncol = length(nms), byrow = TRUE, data = 0)
	for(i in 1:nslts){
		M[i*2,i*2-1] <- linktext 
	}	
	args$A=M
	args$pos=rep(2, length(nms)/2)
	args$name = nms
	args$main=main
	do.call("plotmat", args)
}


