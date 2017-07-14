---
output:
  pdf_document: 
    keep_tex: yes
  html_document: default
---
% The FLR platform and the a4a initiative
% FLR Team
% August 2014


# Why, oh why?

Schnute *et al.* (2007 and 1998) compared the number of software tools
and languages currently available for stock assessments with the Babel
tower myth and concluded that: "The cosmic plan for **confounding
software languages** seems to be working remarkably well among the
community of quantitative fishery scientists!"

# A brief history of FLR

- Started by FEMS FP5, COMMIT & EFIMAS FP6
- Beta ICES WG Methods 2004
- FLCore version 1.0 - 2005
- FLCore version 1.4 *The Golden Jackal* - 2007
\hfill\includegraphics[keepaspectratio, height=0.15\textheight]{graphics/WolfGoldenJackal.jpg}
- FLCore version 2.2 *Swordfish Polka* - 2010
\hfill\includegraphics[keepaspectratio, height=0.15\textheight]{graphics/flr20.png}
- FLR 2.4 *The Duke of Prawns* - 2011
\hfill\includegraphics[keepaspectratio, height=0.15\textheight]{graphics/flr24.png}

#

\centering
\includegraphics[keepaspectratio, height=0.85\textheight]{graphics/flr30.png}

# Current

- FLR 2.5.*, in continuous development
- Main packages are stable
- Keep track of versions you used: local copies, github or packrat
- FLR 2.6 - *Black Swan*
\hfill\includegraphics[keepaspectratio, height=0.15\textheight]{graphics/theeraser.jpg}

# FLR development

- Collaborative development
- Informal team
- Indirect funding
- Open Source

# GNU project (http://gnu.org)

\centering

*Free software is a matter of liberty, not price*

\medskip

\Huge{free = free speech}

\medskip

\Huge{free != free beer}

# Mission statement

The FLR project provides a **platform for quantitative fisheries
science** based on the R statistical language. The guiding principles of
FLR are:

- **openness** - through community involvement and the open source ethos
- **flexibility** - through a design that does not constrain the user to a given paradigm
- **extendibility** - through the provision of tools that are ready to be personalized and adapted.

# Really, what is FLR?

- Extendable toolbox for implementing bio-economic simulation models of fishery systems
- Tools used by managers (hopefully) as well as scientists
- With many applications including:
    - Fit stock-recruitment relationships,
    - Model fleet dynamics (including economics),
    - Simulate and evaluate management procedures and HCRs,
    - More than just stock assessment (VPA, XSA, ICES uptake)
- A software platform for quantitative fisheries science
- A collection of R packages
- A team of devoted developers
- A community of active users

# Design principles

- OOP - S4
- Classes: elements in system
    - `FLStock`, fish stock
		- `FLBRP` inputs for BRP calc
- Methods: link objects
- Mid-steepenes learning curve

# Packages

\centering
\includegraphics[keepaspectratio, width=0.95\textwidth]{graphics/flrpkgs.png}

# a4a - Assessment for All

## Long term vision

- Standard methods to apply rapidly to a large number of stocks
- No strong statistical technical background
- Using technical knowledge on the fisheries, stocks and ecosystem

## Why

- Demand for abundance and exploitation estimates
- Large investments in collecting information
- Scientific advice for fisheries management.


# a4a - Sampled species (PT)

\centering
\includegraphics[keepaspectratio, height=0.6\textheight]{graphics/pt.png}

> **What if we have to assess hundreds of stocks?**
>	Estimate what you know, simulate what you don't

# a4a Initiative EC JRC

1. Develop a4a SA method
2. Discussion on *massive* stock assessment
3. Capacity building (this course)

\centering
<https://fishreg.jrc.ec.europa.eu/web/a4a>

# a4a SA model

- *Moderate* data stock (Catch, Survey/CPUE, little bio)
- NL CaA model, R/FLR/ADMB
- *Simple* syntax

```r
> fmodel = separable()
> qmodel = trawl(techcreep=0.03)
> rmodel = beverton(a=s(NAO))
```

# a4a MSE

Building an STANDARD MSE

1. OM uncertainty in growth, S/R and selectivity
2. HCRs based on catch, surveys, assessments
3. Assessment models of increasing complexity
4. OE for catch and index
5. IE in F or catch

# MSE - The Lego block approach

\centering
\includegraphics[keepaspectratio, height=0.8\textheight]{graphics/MSE.png}

# More information

- [FLR Project @ http://flr-project.org](http://flr-project.org)
- [Source code @ http://github.com/flr/](http://gtihub.com/flr/)
- [a4a Initiative @ https://fishreg.jrc.ec.europa.eu/web/a4a](https://fishreg.jrc.ec.europa.eu/web/a4a)

#

\centering
\includegraphics[keepaspectratio, height=0.8\textheight]{graphics/keep-calm-and-code-flr.png}
