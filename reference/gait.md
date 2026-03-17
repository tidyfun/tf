# Hip and knee angle while walking data

Hip and knee angle measurements in degrees through a 20-point movement
cycle for 39 boys. The data represents the angular positions of hip and
knee joints during normal walking gait, captured at evenly spaced time
points throughout the gait cycle.

## Usage

``` r
gait
```

## Format

A data frame with 39 rows and 3 variables:

- subject_id:

  subject identifier

- knee_angle:

  knee joint angles in degrees

- hip_angle:

  hip joint angle in degrees

## References

Olshen, A R, Biden, N E, Wyatt, P M, Sutherland, H D (1989). “Gait
Analysis and the Bootstrap.” *The Annals of Statistics*, **17**(4),
1419–1440.

Data is also include in the `datasets` package in another format.

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
