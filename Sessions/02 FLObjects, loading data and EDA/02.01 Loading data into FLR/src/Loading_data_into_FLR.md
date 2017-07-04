% Loading data into FLR
% Nekane Alzorriz
% March 2013

# Loading your data into FLR

\includegraphics[keepaspectratio, width=\textwidth]{graphics/data_input.png}

# Dedicated FLR import functions

Data needs to be in specific format

* `readVPAFile()`
* `readFLStock()` : Lowestoft VPA, ICA, Adapt, CSA
* `readFLIndex()`
* `readFLIndices()`
* `readMFCL()`
* `readADMB()`

# Standard R import functions

http://cran.r-project.org/doc/manuals/R-data.html 

* Enter by hand into R
* CSV files : `read.table()`
* Text files : `scan()`
* Database : `RODBC` package
* Direct link to spreadsheets : `excel.link`, `RExcelInstaller` packages

# Loading data is just the start

"Happy families are all alike; every unhappy family is unhappy in its own way."

	Leo Tolstoy



"Clean datasets are all alike; every messy dataset is messy in its own way."

	Hadley Wickham

# To learn more

<http://blog.datacamp.com/r-data-import-tutorial/>

<http://blog.datacamp.com/importing-data-r-part-two/>
