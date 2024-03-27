
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tf <a href="https://tidyfun.github.io/tf/"><img src="https://github.com/tidyfun/tidyfun/blob/master/man/figures/logo.gif?raw=true" align="right" height="150" alt="tf website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov.io](https://codecov.io/github/tidyfun/tf/coverage.svg?branch=main)](https://app.codecov.io/github/tidyfun/tf/branch/main/)
[![R-CMD-check](https://github.com/tidyfun/tf/actions/workflows/full-check.yaml/badge.svg)](https://github.com/tidyfun/tf/actions/workflows/full-check.yaml)
<!-- badges: end -->

The **`tf`** package provides necessary infrastructure for
[**`tidyfun`**](https://tidyfun.github.io/tidyfun/) with minimal
dependencies – specifically: no `tidyverse`-dependencies.

The goal of **`tidyfun`**, in turn, is to provide accessible and
well-documented software that **makes functional data analysis in `R`
easy** – specifically data wrangling and exploratory analysis.

**`tf`** includes definitions of new `S3` data types for vectors of
functional data and associated methods. These `tf`-vectors, with
subclasses `tfd` and `tfb`, use the
[**`vctrs`**](https://vctrs.r-lib.org/)-framework, can be operated on
using most standard functions (`+`, `mean()`, `c()`, etc.) as well as
several new functions in `tf` that implement operations specific for
*functional* data (`tf_smooth`, `tf_derive`, `tf_integrate`).

**Crucially**, vectors of class `tf` can be included in data frames
containing other variables, for simple and reliable data manipulation.
This approach is connected to the conceptual framework in functional
data analysis which assumes that *complete functions* are the unit of
observation. With `tidyfun` and `tf`, you can keep full curves alongside
numeric, factor, and other observations on the same subject in one data
frame.

## Installation

You can install the latest release from GitHub with:

``` r
pak::pak("tidyfun/tf")
```

## Overview

**`tf`** provides:

- new **data types** for representing functional data: `tfd` & `tfb`
- arithmetic **operators** and descriptive **statistics** for such data
- basic **graphics** functions for `tf` vectors
- basic data **wrangling** for functional data: reshaping from list
  columns to wide to long and back, interpolating on different grids,
  filtering and zooming, etc.

Please see the [`tidyfun` website](https://tidyfun.github.io/tidyfun/)
for the full documentation including vignettes etc.

## What does it do?

#### New vector-like data types for functional data

**`tf`** provides [new `S3`-classes for functional
data](https://tidyfun.github.io/tidyfun/reference/index.html#section-tf-sub-classes-constructors-converters),
either as raw data (class `tfd` for *t*idy *f*unctional *d*ata) or in
basis representation (class `tfb` for *t*idy *f*unctional *b*asis data).

Such `tf`-objects can be subsetted or subassigned, computed on and
summarized.

Almost all

- operators like `==`, `+` or `*`
- math functions like `sum`, `log` or `abs`
- and statistics functions like `mean` or `sd`

are defined for the vector classes defined in **`tf`**
([more](https://tidyfun.github.io/tidyfun/reference/index.html#section-arithmetic-logical-and-summary-functions)).

The `tf` objects are just glorified lists, so they work well as columns
in data frames. That makes it a lot easier to keep your other data and
functional measurements together in one object for preprocessing,
exploratory analysis and description. At the same time, these objects
actually behave like vectors of *functions* to some extent, i.e., they
can be evaluated on any point in their domain, they can be integrated or
differentiated, etc.

[See
here](https://tidyfun.github.io/tidyfun/articles/x01_tf_Vectors.html)
for more information on the operations defined for `tf` vectors.

#### Methods for converting existing data to `tf` and back

**`tf`** includes functions `tfd` and `tfb` for converting matrices,
data frames, etc. to `tf` vectors and back. More data wrangling methods
in a `tidyverse`-inspired way and `ggplot2`-geoms for functional data
are available in [**`tidyfun`**](https://tidyfun.github.io/tidyfun/).

[See
here](https://tidyfun.github.io/tidyfun/articles/x02_Conversion.html)
for details on getting data into (and out of) the `tf` format.

------------------------------------------------------------------------

Found a bug? Got a question? Missing some functionality?  
Please let us know so we can make it better.
