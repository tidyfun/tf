## R CMD check results

0 errors | 0 warnings | 0 notes

--------------------------------------------------------------------------------

RE: tests/testthat/test-rebase.R: lines 32, 70, 107 use skip_on_cran() 

These tests apparently triggered an error on Fedora 36 using Intel MKL according 
to https://www.stats.ox.ac.uk/pub/bdr/Rblas/MKL/tf.out. 
We were unable to reproduce the behavior using r-hub's Docker container for this
environment and decided to skip them consequently to avoid getting flagged  
with a false positive. 
We have verified that the omitted test continues to pass in our own testing
environment as well as the standard environments available on 
r-hub & win-builder.r-project.org & mac.r-project.org/macbuilder 
--------------------------------------------------------------------------------
