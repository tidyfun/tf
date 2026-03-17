# Pinch force data

Measurements of pinch force during 20 replications, with 151
observations recorded every 2 milliseconds over 300 milliseconds. The
data captures the dynamics of finger pinch force applied during
controlled motor tasks.

## Usage

``` r
pinch
```

## Format

An object of class `tfd_reg` (inherits from `tfd`, `tf`, `vctrs_vctr`,
`list`) of length 20.

## Details

Data is also include in the
[fda](https://CRAN.R-project.org/package=fda) package in another format.

## References

Ramsay, O. J, Hooker, Giles, Graves, Spencer (2009). *Functional Data
Analysis with R and MATLAB*, series Use R!, 1 edition. Springer New
York, New York. ISBN 978-0-387-98184-0,
[doi:10.1007/978-0-387-98185-7](https://doi.org/10.1007/978-0-387-98185-7)
.

Ramsay, O. J, Silverman, W. B (2005). *Functional Data Analysis*, series
Springer Series in Statistics, 2nd edition. Springer, New York. ISBN
978-0-387-40080-8.

Ramsay, O. J, Silverman, W. B (2002). *Applied Functional Data
Analysis*. Springer.

## Examples

``` r
pinch
#> tfd[20]: [0,0.3] -> [1.165771,12.28027] based on 151 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▁▁▁▂▃▆▇▇▆▅▄▃▂▂▁▁▁▁▁▁▁▁▁▁▁▁
#> [2]: ▁▁▁▁▁▁▁▁▁▃▅▆▆▆▅▃▂▂▂▁▁▁▁▁▁▁
#> [3]: ▁▁▁▁▃▅▇▇▇▆▄▃▂▂▁▁▁▁▁▁▁▁▁▁▁▁
#> [4]: ▁▁▁▁▁▁▁▁▁▂▄▇██▆▅▄▃▂▂▁▁▁▁▁▁
#> [5]: ▁▁▁▁▁▃▅▇▇▆▅▃▂▂▁▁▁▁▁▁▁▁▁▁▁▁
#> [6]: ▁▁▁▁▃▆██▇▅▄▃▂▂▁▁▁▁▁▁▁▁▁▁▁▁
#>     [....]   (14 not shown)
```
