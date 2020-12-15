# lptools

This package runs local projections (Jorda, 2005) in a flexible way and provides some useful functions for manipulating datasets. 
The function used to estimate coefficients of linear regressions is passed as an argument, 
and therefore it can accomodate any estimator available in R that has fields formula and data as arguments (for instance, lm and felm, to name a few). 
Any option of the estimator used can be passed as an argument as well.
Therefore, it can handle fixed effects, instrumental variables and many other models.

This package accomplishes something similar to the lpirfs package (see Adämmer, 2019). The main difference is that here the estimator of the regression coefficients and its options are passed as arguments. Also, it handles regressions in which the endogenous variable is a first difference a bit differently. See the documentation of each function for details.

## Instalation
To install run `devtools::install_github("caiohm/lptools")`.

## Usage
For details on how to use some particularly useful functions run `? local_projection` and `? gen_datalp`.
It requires the dplyr package to run (besides the package of any estimator passed as an argument).

## References

Adämmer, Phillip. lpirfs: An R Package to Estimate Impulse Response Functions by Local Projections. The R Journal, v.11, n.2, p. 421-238, 2019.

Jordà, Òscar. Estimation and Inference of Impulse Responses by Local Projections. American Economic Review, v. 95, n. 1, p. 161-182, 2005.
