[![Build Status](https://travis-ci.org/Swarchal/TISS.svg?branch=master)](https://travis-ci.org/Swarchal/TISS)
[![codecov.io](https://codecov.io/github/Swarchal/TISS/coverage.svg?branch=master)](https://codecov.io/github/Swarchal/TISS?branch=master)

Titration-invariant similarity score (TISS)
============================================

Implementation of the method detailed by Perlman *et al* in their 2004 paper
['Multidimensional Drug Profiling by Automated Microscopy'](http://www.sciencemag.org/content/306/5699/1194.long).

To install from GitHub:
```r
if (!require(devtools)) install.packages('devtools')
devtools::install_github('Swarchal/TISS')
```

**Work in progress.**

-------------

## Workflow

An typical workflow using the example dataset provided:


Construct a metadata object and parse the data:

```r
data(ex_data)

metadata <- construct_metadata(ex_data,
                               compound_col = 'Metadata_compound',
                               conc_col = 'Metadata_concentration',
                               feature_cols = 2:68,
                               negative_control = "DMSO")


compound_data <- get_compound_data(ex_data, metadata)

negative_control <- get_negative_control(ex_data, metadata)
```

Then calculate the D-values and scale them via a z-score to get a numerical vector for each compound:

```r
d_out <- calculate_d(compound_data, negative_control)

d_scale <- scale_d(d_out)
```

Align the compound vectors by maximimum correlation to account for differences in potency:

```r
out <- correlate(d_scale, metadata)

ans <- trim(d_scale, out, metadata = metadata)
```


Finally produce a TISS from the Euclidean distance between compound vectors:

```
similarity_list(ans)

[1]
                 ALLM  ARQ-621 camptothecin dasatinib  emetine    MG132 nocodazole saracatinib     SN38      STS
ALLM         0.000000                                                                                           
ARQ-621      5.808700 0.000000                                                                                  
camptothecin 5.600153 4.739405     0.000000                                                                     
dasatinib    6.260747 4.500577     5.067570  0.000000                                                           
emetine      5.517413 6.259493     5.380277  5.753920 0.000000                                                  
MG132        5.781806 3.732473     4.476155  4.241627 6.070044 0.000000                                         
nocodazole   6.122251 5.141940     4.590035  5.190964 5.566783 4.579403   0.000000                              
saracatinib  6.705706 4.825882     5.754696  4.999944 6.988628 4.705102   5.995406    0.000000                  
SN38         5.523525 5.918995     5.560406  6.061008 4.905090 5.714413   5.605970    7.044819 0.000000         
STS          6.022998 5.926852     5.945048  5.715045 5.687065 5.948143   6.110516    6.185883 5.898727 0.000000

```