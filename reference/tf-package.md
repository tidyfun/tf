# tf: S3 Classes and Methods for Tidy Functional Data

`tf` is a light-weight package with few dependencies that provides the
class definitions and methods infrastructure for `tidyfun` – `tf` gives
you:

- new S3 data types for representing (vectors of) functional data:
  [`tfd()`](https://tidyfun.github.io/tf/reference/tfd.md) &
  [`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md)

- arithmetic operators for such data

- simple descriptive statistics: e.g.
  [`mean.tf()`](https://tidyfun.github.io/tf/reference/tfsummaries.md),
  [`median.tf()`](https://tidyfun.github.io/tf/reference/tfsummaries.md)

- base graphics functions for such data:
  [`plot.tf()`](https://tidyfun.github.io/tf/reference/tfviz.md)

- functions to do smoothing
  ([`tf_smooth.tfd()`](https://tidyfun.github.io/tf/reference/tf_smooth.md)),
  differentiation
  ([`tf_derive.tfd()`](https://tidyfun.github.io/tf/reference/tf_derive.md))
  and integration
  ([`tf_integrate.tfd()`](https://tidyfun.github.io/tf/reference/tf_integrate.md))

The goal of the add-on package `tidyfun` is to make data wrangling and
exploratory analysis for functional data in `R` quick and easy, using
`tidyverse` syntax and standards.  
  
Please also install `tidyfun` for the full functionality to access the
full documentation including a number of vignettes and case studies, or
visit the [`tidyfun` website](https://tidyfun.github.io/tidyfun/).

## See also

Useful links:

- <https://tidyfun.github.io/tf/>

- <https://github.com/tidyfun/tf/>

- Report bugs at <https://github.com/tidyfun/tf/issues>

## Author

**Maintainer**: Fabian Scheipl <fabian.scheipl@googlemail.com>
([ORCID](https://orcid.org/0000-0001-8172-3603)) \[copyright holder\]

Authors:

- Jeff Goldsmith

- Maximilian Mücke ([ORCID](https://orcid.org/0009-0000-9432-9795))

Other contributors:

- Julia Wrobel ([ORCID](https://orcid.org/0000-0001-6783-1421))
  \[contributor\]

- Sebastian Fischer ([ORCID](https://orcid.org/0000-0002-9609-3197))
  \[contributor\]

- Trevor Hastie (softImpute author) \[contributor\]

- Rahul Mazumder (softImpute author) \[contributor\]

- Chen Meng (mogsa author) \[contributor\]
