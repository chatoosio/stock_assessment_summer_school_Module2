Reproducible research with FLR
=========================

Ispra, 18th - 22nd March, 2013
-------------------------

> BE PRAGMATIC !!

> THINK AUTOMATIC REPORTING

---

## What if:

* you're asked to repeat something you did 6 month ago ?
* someone want to repeat your analisys
* you need to replace someone on a project
* the RAC wants to check you assessment
* the stock assessment review panel want to **do** a review

---

## Donald Knuth. "Literate Programming (1984)"

*Literate programming* is a methodology that **combines a programming language with a documentation language**, thereby making programs more robust, more portable, more easily maintained, and arguably more fun to write than programs that are written only in a high-level language. 

The main idea is to treat a program as a **piece of literature**, addressed to human beings rather than to a computer.

---

## Reproducing our work or the work of others ?

 - Why we need it 
 - Why we need it in fisheries (focus on advice)
 - Tools in R (Sweave, knitr, markdown)
 - Output to pdf or html (publish on the fly with Rpubs)
 - If nothing else it will force you to organize your work.
 - Weave and tangle

---

# Example: course notes

---
## Day 01

### Intro to FLR by Iago

* FLR rocks !! Although I'm a bit lost :(
* Something about versions and doggy rock bands from the 70' ...
* It can be installed from the repository

```
install.packages(repos="http://flr-project.org")
```

### Intro to FLQuants by Iago


```r
library(FLCore)
```

```
## Loading required package: grid
```

```
## Loading required package: lattice
```

```
## Loading required package: MASS
```

```
## FLCore 2.5.0 development version
```

```
## Attaching package: 'FLCore'
```

```
## The following object(s) are masked from 'package:base':
## 
## cbind, rbind
```



```r
# creator for FLQuant
FLQuant()
```

```
## An object of class "FLQuant"
## , , unit = unique, season = all, area = unique
## 
##      year
## quant 1 
##   all NA
## 
## units:  NA
```

```r
# some random thing
flq <- FLQuant(rnorm(100), dimnames = list(age = 1:10, year = 1:10))
```



```r
# This code doesn't work needs checking ...
flq <- FLQuant(rnorm(100), dimnames = c(age = 1:10, year = 1:10))
```

```
## Error: more than one vector of names given for the first dimension
```



```r
# This code doesn't work needs checking ...
flq <- FLQuant(rnorm(100), dimnames = c(age = 1:10, year = 1:10))
```


Plots are easy, just call plot.


```r
plot(flq)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


**NOTE:** How can I change the plot ...

---
## Day 02

???

---

# links

http://www.literateprogramming.com

http://www.ctan.org

http://en.wikipedia.org/wiki/LaTeX

http://en.wikibooks.org/wiki/LaTeX
