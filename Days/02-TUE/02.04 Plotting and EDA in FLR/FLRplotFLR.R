# ggplotFLR.R - DESC
# ggplotFLR.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# ggplotFL gives access to ggplot2 methods for FLR classes
library(ggplotFL)

# Our standard datasets
data(ple4)
data(nsher)

# how to plot and FLQuant? e.g. the catch or some other part of a FLStock?
# basically need to convert to a data frame
head(as.data.frame(catch(ple4)))

# LATTICE
# lattice plot functions are defined for FLQuant

# xyplot
xyplot(data~year, data=catch(ple4), type='b', pch=19)

# The conversion to data.frame requires columns names for dims and 'data'
head(as.data.frame(catch(ple4)))

# values in FLQuant turn into a 'data' column to use in formulas
xyplot(data~year, groups=age, data=catch.n(ple4), type='l', ylab='Catch (t)')

# Methods are also available for FLQuants, with elements names' variable 'qname'
xyplot(data~year|qname, data=FLQuants(SSB=ssb(ple4), Yield=catch(ple4)))

# Conversion is done by as.data.frame
head(as.data.frame(FLQuants(SSB=ssb(ple4), Yield=catch(ple4))))

# Other plot functions are also available

# bwplot
bwplot(data~year, rlnorm(200, fbar(ple4), 0.15))

# dotplot
dotplot(data~year, groups=age, rlnorm(200, harvest(ple4), 0.15), cex=0.4)

# histogram
histogram(~data|year, catch.n(ple4))

# Our own bubbles method
bubbles(age~year, data=catch.n(ple4), bub.scale=10)

# Finally some extra examples

# Plot catch data by age group in one panel per age
print(xyplot(data~year|factor(age), data=catch.n(ple4), type=c("g","l"), scales=list(y=list(relation="free")), layout=c(2,5)))


# GGPLOT

# Call ggplot() on an FLQuant
ggplot(data=catch(ple4), aes(year, data)) + geom_point() + geom_line() + ylab("Catch (t)") + xlab("")


# Same for an FLQuants, so we can extract slots or compite values as needed, ssb and fbar are specific methods
ggplot(data=FLQuants(Yield=catch(ple4), SSB=ssb(ple4), F=fbar(ple4)), aes(year, data)) + geom_line() + facet_wrap(~qname, scales="free", nrow=3) + labs(x="", y="")

# We can even plot a whole FLStock
ggplot(data=ple4, aes(year, data)) + geom_line(aes(group=age, colour=factor(age))) + facet_wrap(~slot, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "none")

# Standard FLR class plots are also redefined

# FLQuant
plot(catch.n(ple4))

# with iters
plot(rlnorm(200, fbar(ple4), 0.15))

# and with user-defined quantiles
plot(rlnorm(200, fbar(ple4), 0.15), probs=c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))

# FLQuants w/ iters
fqs <- FLQuants(F=rlnorm(200, fbar(ple4), 0.15), SSB=ssb(ple4), Rec=rec(ple4))

# FLStock
plot(ple4)

# FLSR
plot(nsher)

# Many other plots can be built with ggplotFL, for example

# A bubble plot
ggplot(catch.n(ple4), aes(year, as.factor(age), size=data)) + geom_point(shape=21) + scale_size(range = c(1, 20)) + ylab("age") + theme(legend.position = "none")

# Remember ggplot allows storing and adding plot objects

p <- plot(catch.n(ple4))

p + ylab('Catch PLE4')

