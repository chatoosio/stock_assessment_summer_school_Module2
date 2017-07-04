% The FLStock class
% FLR Team
% August 2014

# The FLStock class
- **Our view** of the dynamics of a stock.
- Composed of FLQuant objects
- Class validity (`validObject()`)
	- year, unit, season, area must equal
	- quant according to slot
	- iter, 1 or N

#
\centering \includegraphics[keepaspectratio, height=0.9\textheight]{graphics/plot.png}

# Slots

- name, desc, range

## Fishery data

- catch: in weight
- catch.n: by quant (age)
- catch.wt: mean-weight-at-quant
- landings, landings.n, landings.wt
- discards, discards.n, discards.wt

# Slot
## Biology
- m
- mat

## Timing
- harvest.spwn, m.spwn

## Results
- stock, stock.n, stock.wt
- harvest: units = 'f' or 'hr'

#  Methods

* Calculations
* Summary
* Subset
* Operations
* Plot
* Accessors
* Conversions

# Methods Map

\centering \includegraphics[keepaspectratio, height=0.9\textheight]{graphics/FLStock.png}
