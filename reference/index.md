# Package index

## tf - (Sub)classes: Constructors & converters

Defining and converting functional data objects

- [`tfd()`](https://tidyfun.github.io/tf/reference/tfd.md)
  [`as.tfd()`](https://tidyfun.github.io/tf/reference/tfd.md)
  [`as.tfd_irreg()`](https://tidyfun.github.io/tf/reference/tfd.md) :
  Constructors for vectors of "raw" functional data

- [`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md)
  [`tfb_wavelet()`](https://tidyfun.github.io/tf/reference/tfb.md)
  [`as.tfb()`](https://tidyfun.github.io/tf/reference/tfb.md) :
  Constructors for functional data in basis representation

- [`as.data.frame(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/converters.md)
  [`as.matrix(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/converters.md)
  [`as.function(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/converters.md)
  : Convert functional data back to tabular data formats

- [`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)
  : Spline-based representation of functional data

- [`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md) :
  Functional data in FPC-basis representation

- [`fpc_wsvd()`](https://tidyfun.github.io/tf/reference/fpc_wsvd.md) :
  Eigenfunctions via weighted, regularized SVD

- [`tf_rebase()`](https://tidyfun.github.io/tf/reference/tf_rebase.md) :

  Change (basis) representation of a `tf`-object

## Evaluating, indexing & re-arranging

Accessing, appending, evaluating, splitting & combining functional data
objects

- [`` `[`( ``*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfbrackets.md)
  [`` `[<-`( ``*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfbrackets.md)
  :

  Accessing, evaluating, subsetting and subassigning `tf` vectors

- [`tf_approx_linear()`](https://tidyfun.github.io/tf/reference/tf_approx.md)
  [`tf_approx_spline()`](https://tidyfun.github.io/tf/reference/tf_approx.md)
  [`tf_approx_none()`](https://tidyfun.github.io/tf/reference/tf_approx.md)
  [`tf_approx_fill_extend()`](https://tidyfun.github.io/tf/reference/tf_approx.md)
  [`tf_approx_locf()`](https://tidyfun.github.io/tf/reference/tf_approx.md)
  [`tf_approx_nocb()`](https://tidyfun.github.io/tf/reference/tf_approx.md)
  :

  Inter- and extrapolation functions for `tfd`-objects

- [`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md)
  :

  Evaluate `tf`-vectors for given argument values

- [`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)
  :

  Re-evaluate `tf`-objects on a new grid of argument values.

- [`tf_split()`](https://tidyfun.github.io/tf/reference/tf_splitcombine.md)
  [`tf_combine()`](https://tidyfun.github.io/tf/reference/tf_splitcombine.md)
  : Split / Combine functional fragments

## Arithmetic, logical and summary functions

Functionality for computing with and comparing functional data

- [`` `==`( ``*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`` `!=`( ``*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`` `==`( ``*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`` `!=`( ``*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`vec_arith(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`vec_arith(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`Math(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`Math(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`Summary(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cummax(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cummin(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cumsum(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cumprod(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cummax(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cummin(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cumsum(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  [`cumprod(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.md)
  :

  Math, Summary and Ops Methods for `tf`

- [`mean(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfsummaries.md)
  [`median(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfsummaries.md)
  [`sd()`](https://tidyfun.github.io/tf/reference/tfsummaries.md)
  [`var()`](https://tidyfun.github.io/tf/reference/tfsummaries.md)
  [`summary(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfsummaries.md)
  :

  Functions that summarize `tf` objects across argument values

- [`tf_fwise()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_fmax()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_fmin()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_fmedian()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_frange()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_fmean()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_fvar()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_fsd()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_crosscov()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  [`tf_crosscor()`](https://tidyfun.github.io/tf/reference/functionwise.md)
  :

  Summarize each `tf` in a vector (function-wise)

- [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md) :
  Functional Data Depth

- [`rank()`](https://tidyfun.github.io/tf/reference/tf_order.md)
  [`xtfrm(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tf_order.md)
  [`sort(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tf_order.md)
  :

  Rank, order and sort `tf` vectors

- [`min(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tf_minmax.md)
  [`max(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tf_minmax.md)
  [`range(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tf_minmax.md)
  :

  Depth-based minimum, maximum and range for `tf` vectors

- [`fivenum()`](https://tidyfun.github.io/tf/reference/fivenum.md) :

  Tukey's Five Number Summary for `tf` vectors

## Differentiation, integration and simple smoothing

- [`tf_derive()`](https://tidyfun.github.io/tf/reference/tf_derive.md) :
  Differentiating functional data: approximating derivative functions

- [`tf_integrate()`](https://tidyfun.github.io/tf/reference/tf_integrate.md)
  : Integrals and anti-derivatives of functional data

- [`tf_smooth()`](https://tidyfun.github.io/tf/reference/tf_smooth.md) :

  Simple smoothing of `tf` objects

## Registering and warping

Functions for registering, and warping functional data

- [`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md)
  :

  Register / align a `tf` vector against a template function

- [`tf_estimate_warps()`](https://tidyfun.github.io/tf/reference/tf_estimate_warps.md)
  : Estimate warping functions for registration

- [`tf_aligned()`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`tf_inv_warps()`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`tf_template()`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`print(`*`<tf_registration>`*`)`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`summary(`*`<tf_registration>`*`)`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`print(`*`<summary.tf_registration>`*`)`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`plot(`*`<tf_registration>`*`)`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`` `[`( ``*`<tf_registration>`*`)`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  [`length(`*`<tf_registration>`*`)`](https://tidyfun.github.io/tf/reference/tf_registration.md)
  : Registration Result Object

- [`tf_align()`](https://tidyfun.github.io/tf/reference/tf_align.md) :
  Apply warping functions to align functional data

- [`tf_landmarks_extrema()`](https://tidyfun.github.io/tf/reference/landmarks.md)
  [`detect_landmarks()`](https://tidyfun.github.io/tf/reference/landmarks.md)
  [`cluster_landmarks()`](https://tidyfun.github.io/tf/reference/landmarks.md)
  [`build_landmark_matrix()`](https://tidyfun.github.io/tf/reference/landmarks.md)
  : Find Extrema Locations in Functional Data

- [`tf_warp()`](https://tidyfun.github.io/tf/reference/tf_warp.md) :

  Elastic Deformation: warp and align `tf` vectors

- [`tf_invert()`](https://tidyfun.github.io/tf/reference/tf_invert.md) :

  Invert a `tf` vector

## Visualization & Display

Graphics functions and print formats

- [`plot(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfviz.md)
  [`lines(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfviz.md)
  [`points(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfviz.md)
  :

  `base` plots for `tf`s

- [`print(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.md)
  [`print(`*`<tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.md)
  [`print(`*`<tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.md)
  [`print(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.md)
  [`format(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.md)
  : Pretty printing and formatting for functional data

- [`prep_plotting_arg()`](https://tidyfun.github.io/tf/reference/prep_plotting_arg.md)
  : Preprocess evaluation grid for plotting

## Querying functional data

Locating functional features like peaks or zero-crossings

- [`tf_where()`](https://tidyfun.github.io/tf/reference/tf_where.md)
  [`tf_anywhere()`](https://tidyfun.github.io/tf/reference/tf_where.md)
  : Find out where functional data fulfills certain conditions.
- [`tf_zoom()`](https://tidyfun.github.io/tf/reference/tf_zoom.md) :
  Functions to zoom in/out on functions

## Utilities

Utility functions

- [`tf_arg()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`tf_evaluations()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`tf_count()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`tf_domain()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`` `tf_domain<-`() ``](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`tf_evaluator()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`` `tf_evaluator<-`() ``](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`tf_basis()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`` `tf_arg<-`() ``](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`coef(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`rev(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is.na(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is.na(`*`<tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_tf()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_tfd()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_reg()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_tfd_reg()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_irreg()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_tfd_irreg()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_tfb()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_tfb_spline()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  [`is_tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfmethods.md)
  :

  Utility functions for `tf`-objects

## Generating random functional data

RNGs for curves

- [`tf_rgp()`](https://tidyfun.github.io/tf/reference/tf_rgp.md) :
  Gaussian Process random generator

- [`tf_jiggle()`](https://tidyfun.github.io/tf/reference/tf_jiggle.md)
  [`tf_sparsify()`](https://tidyfun.github.io/tf/reference/tf_jiggle.md)
  :

  Make a `tf` (more) irregular

## Accessories

Tooling functions

- [`in_range()`](https://tidyfun.github.io/tf/reference/in_range.md)
  [`` `%inr%` ``](https://tidyfun.github.io/tf/reference/in_range.md) :
  Find out if values are inside given bounds

- [`ensure_list()`](https://tidyfun.github.io/tf/reference/ensure_list.md)
  : Turns any object into a list

- [`unique_id()`](https://tidyfun.github.io/tf/reference/unique_id.md) :
  Make syntactically valid unique names

- [`vec_cast(`*`<tfd_reg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfd_reg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfd_reg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfd_reg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfd_irreg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfd_irreg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfd_irreg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfd_irreg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_spline.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_spline.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_fpc.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_fpc.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_spline.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_spline.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_fpc.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_cast(`*`<tfb_fpc.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_reg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_reg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_reg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_reg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_irreg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_irreg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_irreg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfd_irreg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_spline.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_spline.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_spline.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_spline.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_fpc.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_fpc.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_fpc.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  [`vec_ptype2(`*`<tfb_fpc.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.md)
  :

  `vctrs` methods for `tf` objects

- [`tf`](https://tidyfun.github.io/tf/reference/tf-package.md)
  [`tf-package`](https://tidyfun.github.io/tf/reference/tf-package.md) :
  tf: S3 Classes and Methods for Tidy Functional Data

## Data

Real world functional data

- [`gait`](https://tidyfun.github.io/tf/reference/gait.md) : Hip and
  knee angle while walking data
- [`growth`](https://tidyfun.github.io/tf/reference/growth.md) :
  Berkeley growth study data
- [`pinch`](https://tidyfun.github.io/tf/reference/pinch.md) : Pinch
  force data
