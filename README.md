[![Build Status](https://travis-ci.org/Swarchal/TISS.svg?branch=master)](https://travis-ci.org/Swarchal/TISS)
[![codecov.io](https://codecov.io/github/Swarchal/TISS/coverage.svg?branch=master)](https://codecov.io/github/Swarchal/TISS?branch=master)

Titration-invariant similarity score (TISS)
============================================

Implementation of the method detailed by Perlman *et al* in their 2004 paper
['Multidimensional Drug Profiling by Automated Microscopy'](http://www.sciencemag.org/content/306/5699/1194.long).

Original was released as Matlab code for batch analysis. This is an attempt to re-write the methods in R with more emphasis on interactive use, and to make the method easier to apply to multiple use-cases.

To install from GitHub:
```r
if (!require(devtools)) install.packages('devtools')
devtools::install_github('Swarchal/TISS')
```