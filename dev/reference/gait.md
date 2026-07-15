# Hip and knee angle while walking data

Hip and knee angle measurements in degrees through a 20-point movement
cycle for 39 children. The data represents the angular positions of hip
and knee joints during normal walking gait, captured at evenly spaced
time points throughout the gait cycle.

## Usage

``` r
gait
```

## Format

A data frame with 39 rows and 3 variables:

- subject_id:

  subject identifier

- knee_angle:

  a `tfd` column of knee-joint angles (degrees)

- hip_angle:

  a `tfd` column of hip-joint angles (degrees)

## Details

Data is also included in the
[fda](https://CRAN.R-project.org/package=fda) package in another format.

## References

Olshen, A R, Biden, N E, Wyatt, P M, Sutherland, H D (1989). “Gait
Analysis and the Bootstrap.” *The Annals of Statistics*, **17**(4),
1419–1440.

## Examples

``` r
head(gait)
#>   subject_id           knee_angle            hip_angle
#> 1          1 ▂▂▂▂▂▂▂▂▂▃▃▄▆▇█▇▆▅▃▂ ▆▆▅▅▄▃▃▃▂▂▂▂▃▄▅▆▇▇▆▆
#> 2          2 ▂▃▃▃▂▂▂▁▁▁▂▄▆▇█▇▆▄▃▂ ▇▇▇▅▅▄▃▃▂▁▁▁▃▄▅▇▇▇▇▇
#> 3          3 ▂▃▄▄▃▃▂▂▁▂▂▄▆███▇▅▃▂ ▇▇▆▅▅▅▄▃▂▁▁▁▂▄▅▇███▇
#> 4          4 ▁▂▂▂▂▁▁▁▁▁▃▅▆▇▇▇▅▂▁▁ ▆▆▅▄▃▃▂▂▁▁▁▂▂▃▄▅▅▆▅▅
#> 5          5 ▁▁▁▁▁▁▁▁▁▂▃▅▆▇█▇▅▃▁▁ ▄▃▂▂▂▂▁▁▁▁▁▁▂▃▅▆▅▅▅▅
#> 6          6 ▂▂▃▃▃▂▂▂▁▂▂▃▅▆▇▇▇▅▃▁ █▇▇▆▅▅▄▃▃▂▂▂▂▄▅▆▇███
```
