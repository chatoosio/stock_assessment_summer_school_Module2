% The FLCore package of FLR
% FLR Team
% August 2014

# FLCore in FLR
\centering
\includegraphics[keepaspectratio, width=0.95\textwidth]{graphics/flrpkgs.png}

# Some OOP basics

* Class: e.g. `data.frame`, `vector`, `matrix`
* Method: e.g. `summary`, `plot`
* Signature: `plot(data.frame)` vs. `plot(vector)`

\flushright
\includegraphics[keepaspectratio, height=0.5\textheight]{graphics/oop.png}

# S4 classes

* Full OOP mechanism in R
* Extension of S3
* Used extensively by FLR

# Basic classes

* FLArray, **FLQuant** & FLCohort
* **FLPar**
* FLQuantPoint
* FLQuantDistr
* FLQuantJK

# Composite classes

* **FLStock**
* **FLSR**
* **FLIndex**
* FLBiol

# Plural classes

Lists with all elements being of the same class

* **FLQuants**
* **FLStocks**
* **FLIndices**
* FLBiols

# Current status

* Version 2.5.*, always in development
* Currently has
  * 12,351 lines of code
	* Exports 28 classes
	* 289 methods
	* 7 datasets

# Go for it!

* `install.packages('FLCore', repos='http://flr-project.org/R')`
* `install_github('FLCore', 'flr')`
* FLCore pages: <http://flr-project.org/FLCore>
* FLCore repository: <http://github.com/flr/FLCore>
