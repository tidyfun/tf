# Berkeley growth study data

Heights of 39 boys and 54 girls measured from age 1 to 18 years as part
of the Berkeley Growth Study. The data tracks physical development over
time with measurements at 31 different ages that are not equally spaced.

## Usage

``` r
growth
```

## Format

A data frame with 93 rows and 2 variables:

- gender:

  sex of the subject (boy/girl)

- height:

  height in centimeters

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

Tuddenham, D R (1954). “Physical growth of California boys and girls
from birth to eighteen years.” *University of California Publications in
Child Development*, **1**, 183–364.

## Examples

``` r
head(growth)
#>   gender                     height
#> 1 female ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇▇▇▇██████
#> 2 female ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇██████████
#> 3 female ▁▁▂▂▃▄▄▅▅▆▆▇▇▇▇███████████
#> 4 female ▁▁▁▂▃▄▄▅▅▅▆▆▆▇▇▇██████████
#> 5 female ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇██████████
#> 6 female ▁▁▁▁▂▃▄▄▄▅▅▆▆▆▆▇▇█████████
```
