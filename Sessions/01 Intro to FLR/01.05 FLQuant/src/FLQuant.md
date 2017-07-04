% The FLQuant class
%
% March 2013

# Class structure

## `array`
* 6 dims: `(quant)`, `year`, `unit`, `season`, `area`, `iter`

## attributes
* units: `character`

# 6D array
	> array(1:144, dim=c(3,3,2,2,2,2))
	, , 1, 1, 1, 1
	
	     [,1] [,2] [,3]
	[1,]    1    4    7
	[2,]    2    5    8
	[3,]    3    6    9
	
	, , 2, 1, 1, 1
	
	     [,1] [,2] [,3]
	[1,]   10   13   16
	[2,]   11   14   17
	[3,]   12   15   18

# 2D vs ...
	> dfa[1:12,]
	   d1 d2 d3 d4 d5 d6 data
	1   1  1  1  1  1  1    1
	2   2  1  1  1  1  1    2
	3   3  1  1  1  1  1    3
	4   1  2  1  1  1  1    4
	5   2  2  1  1  1  1    5
	6   3  2  1  1  1  1    6
	7   1  3  1  1  1  1    7
	8   2  3  1  1  1  1    8
	9   3  3  1  1  1  1    9
	10  1  1  2  1  1  1   10
	11  2  1  2  1  1  1   11
	12  3  1  2  1  1  1   12

# ... 6D

	> arr[,,,1,1,1]
	, , 1
	
	     [,1] [,2] [,3]
	[1,]    1    4    7
	[2,]    2    5    8
	[3,]    3    6    9
	
	, , 2
	
	     [,1] [,2] [,3]
	[1,]   10   13   16
	[2,]   11   14   17
	[3,]   12   15   18


# Characteristics

\begin{columns}[lr]
\column{0.6\textwidth}
\begin{itemize}
\item \texttt{Arith} re-defined to always return \texttt{FLQuant}
\item Dimensions are not dropped even if of length=1
\end{itemize}
\column{0.4\textwidth}
\includegraphics[keepaspectratio, height=0.4\textheight]{graphics/3d_array.png}
\end{columns}

# Methods
\centering
\includegraphics[keepaspectratio, height=0.9\textheight]{graphics/FLQuant-methods.png}


