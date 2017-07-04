# plot.R - DESC
# plot.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLCore)
library(ggplotFL)
data(ple4)
opts_chunk$set(dev="png", dev.args=list(bg ="transparent"), dpi=300)

plot(ple4) + theme(plot.background=element_rect(fill = "transparent",colour = NA))

