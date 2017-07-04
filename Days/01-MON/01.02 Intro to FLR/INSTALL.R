# INSTALL.R - Installation script
# INSTALL.R

# Copyright 2013-2014 FLR Team. FLR Team. Distributed under the GPL 3
# Maintainer: MAU G03, IPSC, EC JRC.

# Install dependencies

install.packages(c('copula', 'triangle', 'mgcv', 'splines', 'plyr', 'ggplot2', 'knitr'))

# Install FLR packages
install.packages(c('FLCore', 'FLa4a', 'FLash', 'ggplotFL', 'FLBRP', 'FLEDA'),
	repos='http://flr-project.org/R')

# Install all
source("http://flr-project.org/R/instFLR.R")
