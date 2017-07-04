% Modelling growth and converting from length-based to age-based data
% a4a Team
% April 2014

# Introduction

Stock assessment model is age-based.

But 'raw' data is often length-based.

Need to convert length-based to age-based data.

Also need to include uncertainty in the conversion.

# a4aGr - the growth class

Relates age to length (and vice versa).

Requirements:

* Growth model: age ~ length (formula)
* Inverse growth model: length ~ age (formula)
* model parameters (FLPar)
* variance-covariance matrix (optional - parameter uncertainty)

# Modelling uncertainty

Uncertainty in growth comes through parameter uncertainty.

Variance-covariance matrix PLUS an assumed distribution, e.g.:

* Multivariate normal
* Multivariate with triangular marginals (copulas)
* Something else using copulas

Result is an a4aGr object with iterations.

# l2a() - length to age method

Individual FLQuant, FLStockLen, FLIndex

* 'Number' slots (e.g. 'catch.n') are summed
* 'Weight' slots (e.g. 'catch.wt') use mean, weighted by numbers
* Other slots (e.g. 'mat') use mean

# General approach (summary)

* Make an a4aGr object (growth model + parameters)
* Add uncertainty on the parameters
* Convert FLQuant/Stock/Index using growth model

