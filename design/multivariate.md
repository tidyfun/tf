# Design notes: vector-valued functional data `f: R -> R^d`

This document records the design of `tfd_mv` / `tfb_mv`, the `tf` classes for
*vector-valued* functional data — vectors of functions
\(f: \mathcal{T}\subset\mathbb{R} \to \mathbb{R}^d\). The motivating use cases
are GitHub issues [#18](https://github.com/tidyfun/tf/issues/18) ("multivariate
evaluations (curves)") and [#27](https://github.com/tidyfun/tf/issues/27)
(movement / tracking data, à la Joo et al. 2019), e.g. an animal's position
\((x(t), y(t))\) or \((x(t), y(t), z(t))\) over time.

**Scope.** This covers multivariate *output* (curves into `R^d`). Multivariate
*input* (surfaces `f: R^p -> R`, the other branch of #18) is **out of scope**
here; see "Surfaces (future work)" below.

## The problem

`tf` already represents univariate functional data `f: R -> R` with four leaf
classes, all built on `vctrs::new_vctr()`:

| class        | parents     | per-curve data                  |
|--------------|-------------|---------------------------------|
| `tfd_reg`    | `tfd`, `tf` | numeric value vector (shared `arg`) |
| `tfd_irreg`  | `tfd`, `tf` | `list(arg=, value=)`            |
| `tfb_spline` | `tfb`, `tf` | spline coefficients             |
| `tfb_fpc`    | `tfb`, `tf` | FPC scores                      |

We want `f: R -> R^d` while (a) keeping both the raw (`tfd`) and basis (`tfb`)
representations, (b) supporting regular *and* irregular sampling, (c) full
`vctrs` compatibility (subset, `c()`, casting, tibble columns), (d) an elegant
UI, and — emphatically — (e) **maximal reuse** of the existing univariate
machinery, with no new numeric kernels.

## Approaches considered

### (A) Composition — bundle `d` univariate `tf` vectors  *(chosen)*

A `tf_mv` object of length `n` stores, as an attribute, a named list of `d`
univariate `tf` vectors (the components / output dimensions), each of length
`n`. Almost every method is obtained by mapping the existing univariate code
over the `d` components.

* **+** Massive reuse: evaluation, arithmetic, `Math`/`Summary`, smoothing,
  basis fitting, casting — all delegate to univariate methods via `map`/`map2`.
* **+** `tfd` **and** `tfb`, regular **and** irregular, all "for free" because a
  component is itself any univariate `tf`. Components may even live on
  *different* argument grids (natural for gappy movement data).
* **+** Intuitive UI: `f$x`, `tf_component(f, "x")`, `tf_components(f)`.
* **−** Needs one genuinely new piece of `vctrs` machinery: a custom
  `vec_proxy`/`vec_restore` pair (the package otherwise uses `new_vctr`
  defaults). This is a small, well-understood, localized addition.

### (B) Matrix-valued evaluations — one class, each curve's value is an `n_arg × d` matrix

Extend the `tfd` internals so each curve stores an `(n_arg × d)` matrix and the
evaluator returns a matrix; `d` is an attribute.

* **+** Keeps the default `vctrs` proxy (no custom restore).
* **−** Large blast radius: every consumer of an evaluation vector
  (`evaluate.R`, `summarize.R`, `ops.R`, `print-format.R`, the spline fitter,
  plotting helpers, ...) must branch matrix-vs-vector. That is exactly the code
  bloat/duplication we were asked to avoid.
* **−** `tfb` is awkward: fitting a basis to matrix-valued evaluations either
  reduces to fitting per column (i.e. approach A in disguise) or needs a
  genuinely multivariate basis (much harder, out of scope).
* **−** Forces a single shared `arg` across dimensions within a curve.

### (C) Long / stacked encoding — a `d·n`-length univariate object + dimension index

Stack the `d` components into one univariate vector of length `n·d` plus a
component-index attribute, reshaping on access.

* **+** Reuses univariate storage verbatim.
* **−** Breaks `vctrs` semantics: `length()`, `[`, `c()`, recycling and tibble
  row counts would all operate on `n·d`, not `n`. Recovering correct
  per-observation semantics needs a fragile grouped proxy — i.e. all of (A)'s
  custom-proxy work plus brittle index arithmetic. Irregular + per-dimension
  grids become a bookkeeping nightmare, and the conceptual model leaks into the
  UI.

### Decision

**(A) Composition.** It is the only option that satisfies *every* hard
requirement, and it wins decisively on reuse: the entire numeric surface of the
package is inherited by delegation. The single cost — a custom
`vec_proxy`/`vec_restore` — is paid once, in `R/mv-vctrs.R`.

## Chosen design in detail

### Classes

```
tfd_mv = c("tfd_mv", "tf_mv", "tf")   # raw-evaluation components
tfb_mv = c("tfb_mv", "tf_mv", "tf")   # basis components
```

`tf_mv` is the abstract multivariate parent (convention only, like `tf`). The
classes inherit `tf` (so `is_tf()`, `tf_domain()`, S4 `setOldClass()` apply) but
**deliberately not** `tfd`/`tfb`: univariate methods such as `Math.tfd`,
`as.matrix.tf` or `[.tf` assume *scalar* evaluations and would misbehave on a
bundle. `is_tfd()`/`is_tfb()` therefore return `FALSE`; use `is_tf_mv()`,
`is_tfd_mv()`, `is_tfb_mv()`.

### Internal layout (`R/tfd-mv.R`, `new_tf_mv()`)

Built with `vctrs::new_vctr()`:

* `.data = seq_len(n)` — an integer placeholder of length `n` (number of curves).
* attribute `components` — a named list of the `d` univariate `tf` vectors.
* attribute `comp_names` — the component names (`c("x","y",...)`).
* attribute `domain` — the shared `tf_mv` domain.

All per-curve metadata (`arg`, `evaluator`, `basis`, `basis_matrix`, ...) lives
*inside* the component objects, so none of it is duplicated. **Domain
handling**: a `tf_mv` lives on a single time axis. If components arrive with
differing domain attributes (typical when each was built from independent
irregular sampling and the auto-derived range differs in the last digit),
`new_tf_mv()` takes the union as the mv domain and widens each component to
match. Users can supply an explicit `domain` to `tfd_mv()` as long as it
contains every component's observed range; a `domain` that does not is an
error.

### vctrs integration (`R/mv-vctrs.R`)

The proxy is a **data frame with `n` rows and `d` columns**, one column per
component:

```r
vec_proxy.tf_mv(x)  # -> data.frame(x = <tfd>, y = <tfd>, ...)  (n rows)
vec_restore.tf_mv(x, to)  # rebuild the bundle from the (sliced/combined) proxy
```

This is the key idea: because the proxy columns *are* univariate `tf` vectors,
`vec_slice()`, `vec_c()` and casting all fall out of the existing **univariate**
`vctrs` methods applied column-wise. For example, `c(reg_mv, irreg_mv)` combines
each component with the univariate `vec_ptype2.tfd_reg.tfd_irreg`, yielding an
irregular result — no extra code. (Note: `vec_proxy.tf_mv` must take the size
from the components, never from `vec_size(x)`, which would recurse.)

`vec_ptype2`/`vec_cast` are defined for all four `{tfd_mv,tfb_mv}²` pairs and
computed component-wise (so `tfd_mv <-> tfb_mv` reuses `tf_rebase`); they require
equal `d` and equal `comp_names`. `vec_ptype_abbr` → `"tfd_mv"`;
`vec_ptype_full` → `"tfd_mv<d=2>"`. (Like the existing leaf-class methods, the
ptype methods are registered on the *leaf* classes `tfd_mv`/`tfb_mv`, not on the
`tf_mv` parent — vctrs picks them up there.)

### UI and methods (`R/mv-methods.R`)

* Constructors: `tfd_mv()` / `tfb_mv()` accept a list of `tf` vectors, a list of
  matrices (one per component), a 3-d array `[curve, arg, component]`, or a long
  data frame with several `value` columns.
* Accessors: `tf_ncomp()`, `tf_components()`, `tf_component()` / `<-`, and `$`
  sugar (`f$x`). `tf_arg()` adapts to the irregularity structure (see
  "Irregularity cases" below); `tf_evaluations()` returns a list of `n`
  `(n_arg × d)` matrices when components share the per-curve grid, otherwise a
  per-curve named list; `tf_count()` an `n × d` matrix.
* `[`: `f[i]` subsets curves; `f[i, j]` evaluates, returning a 3-d array
  `[curve, arg, component]` (this is issue #18's "array-valued `j`"), or a list
  of per-curve data frames when `matrix = FALSE`; `component=` drops to the
  univariate result; a 2-column matrix index returns one row per
  `(curve, arg)` pair × `d` columns.
* Arithmetic / `Math` / `Summary` / `mean`/`median`/`sd`/`var` / `==` / `!=`:
  all component-wise, delegating to the univariate operators.
* Display: `print`/`format` join the per-component sparklines with `" | "`;
  pillar `format_glimpse` is registered for tibble columns.
* `plot()`: `"facet"` (one panel per component) or `"trajectory"` (for `d = 2`,
  `y(t)` vs `x(t)` — the movement view).
* Interop: `as.matrix()` → `[curve, arg, component]` array; `as.data.frame(.,
  unnest = TRUE)` → long format with one column per component.

### Irregularity cases

Because each output dimension is an independent univariate `tf` vector, the
composition design naturally accommodates four qualitatively different
"shapes" of irregularity. `tf_arg()` and `tf_evaluations()` adapt their return
type so callers can tell which shape they have:

| Case | Components | `tf_arg(f)` | `tf_evaluations(f)[[i]]` |
|------|------------|-------------|--------------------------|
| 0. fully regular (shared grid across components *and* curves) | all `tfd_reg` / `tfb`, identical `arg` | numeric vector | `(n_arg × d)` matrix |
| 1. **per-curve grid shared across components**, varies across curves (e.g. movement data sampled at irregular but synchronized timestamps) | all `tfd_irreg`, identical per-curve arg lists | **list of `n` numeric vectors** (collapsed — not nested) | `(n_arg_i × d)` matrix |
| 2. per-component regular grid, same for all curves (e.g. position on a finer grid than velocity) | all `tfd_reg`, different `arg` vectors | list of `d` numeric vectors | per-curve named list (lengths differ across components) |
| 3. per-(curve, component) grids | all `tfd_irreg`, independent per-curve arg lists | list of `d` lists (one per component) | per-curve named list |

The independence-of-components design comes with one cost: in case 1 the
shared per-curve arg vector is stored once *per component*, so `d`-fold
redundant. That is the price of the composition principle — components must
be self-contained univariate `tf` vectors so the existing numeric machinery
reuses verbatim. The redundancy is purely in storage; the public accessors
(`tf_arg`, `tf_evaluations`) collapse to the non-redundant shape.

Mixed cases (some components regular, some irregular; some on shared grids,
some not) also work — they fall through to the "list of `d` per-component"
shape.

## Worked example

```r
library(tf)
# a 2-d trajectory from two GP draws
traj <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
traj
tf_ncomp(traj)          # 2
traj$x                  # the x-component as a univariate tfd
traj[1:2]               # subset curves
traj[1, c(.2, .5, .8)]  # array [curve, arg, component]
traj + traj             # component-wise arithmetic
mean(traj)              # component-wise pointwise mean (length-1 tfd_mv)

# basis representation, fitted per component:
tb <- tfb_mv(traj, k = 15, verbose = FALSE)
as.tfd_mv(tb)           # back to raw (lossy)

# trajectory plot (movement view):
plot(traj, type = "trajectory")

# works as a tibble column:
tibble::tibble(id = 1:5, path = traj)
```

## Delegated verbs (calculus, smoothing, re-basing, registration)

Because a `tf_mv` is just a bundle of univariate `tf` vectors, the standard
verbs work by mapping over the components:

* `tf_rebase()` — re-expresses each component in the target basis/grid, hence
  also drives `tfd_mv <-> tfb_mv` conversion.
* `tf_derive()`, `tf_smooth()`, `tf_zoom()` — component-wise, returning a
  `tf_mv`.
* `tf_integrate()` — definite integrals return an `n × d` matrix (one
  integral per curve per component); indefinite integrals return a `tf_mv`.

**Registration** is the one verb that is *not* a naive component-wise map: a
vector-valued curve has a single time axis, so all components must share one
warp or they desynchronize. `tf_estimate_warps.tf_mv()` therefore estimates a
**single warp per curve** from a univariate *registration signal* and
`tf_warp.tf_mv()` / `tf_align.tf_mv()` apply that one warp to every component.
`tf_register()` (a plain function) then composes correctly with no changes,
producing a `tf_registration` whose `registered`/`template` are `tf_mv` and
whose warps are univariate.

The registration signal defaults to the **first component** — predictable and
never degenerate. `ref_component` can select another component (by name/index),
`"norm"` for the pointwise Euclidean norm \(\lVert f(t)\rVert\)
(rotation-invariant, but degenerate for constant-modulus signals such as
`(sin, cos)`), or a custom `function(tf_mv) -> tf`. Fully-joint multivariate
elastic registration (e.g. a multivariate SRVF criterion summed across
components inside the optimizer) is left as future work.

## TODO / future work

* **Convenience functions to regularize irregular `tf_mv` args.** Two
  orthogonal "regularization" axes are useful and currently leave the user
  threading raw `tfd()` / `tf_interpolate()` calls per component:
  * *across components within each entry* — when components arrive on
    independent per-curve grids (case 3 above), force every component of
    curve `i` onto a single shared per-curve grid (collapsing case 3 →
    case 1). Natural default: the union, intersection, or a user-supplied
    grid per curve.
  * *across entries within each component* — collapse irregular components
    to a regular shared grid (case 1 → case 0 for that component). Standard
    `tf_interpolate()`-style operation, but a single `tf_mv`-aware verb
    avoids per-component bookkeeping.
* **`tfb_mv` with a *shared* basis across components.** The current
  `tfb_mv()` fits each component independently, so the `d` basis function
  systems can differ (and the per-component `basis_matrix` attributes are
  stored separately). A shared-basis variant would fit one basis on the
  stacked components and store a single `basis_matrix` + `d` coefficient
  vectors per curve — smaller storage, stronger statistical coupling, and
  cleaner mathematics (e.g. derivatives, integrals, inner products done
  uniformly).
* **`tfb_mv` from MFPCA (multivariate functional principal components).**
  Multivariate FPCA produces vector-valued eigenfunctions
  \(\Psi_k: \mathcal{T} \to \mathbb{R}^d\) with shared *scalar* loadings
  per curve: \(f_i(t) \approx \mu(t) + \sum_k s_{ik}\,\Psi_k(t)\). This is
  qualitatively different from "FPC per component": it captures joint
  variation across dimensions in a single coordinate system. Would require
  a new `tfb_mv` subclass storing the multivariate eigenfunctions plus an
  `n × K` score matrix, an MFPCA fitter (or a wrapper around
  `MFPCA::MFPCA()`), and a `vec_cast` path back to `tfd_mv`.
* **A proper vignette with real-data case studies.** This design doc is
  not a substitute for narrative documentation. A `vignettes/multivariate.Rmd`
  walking through (1) construction from the shipped `gait` data (a real
  2-d example: knee + hip angle), (2) bracket/`$`/array semantics, (3)
  `tfb_mv` round-trip, (4) registration on synchronized vs desynchronized
  components, and (5) a movement-data case study would exercise the
  feature end-to-end and surface remaining rough edges. Adds `knitr` /
  `rmarkdown` to `Suggests`.

## Surfaces (future work)

Multivariate *input* (`f: R^p -> R`, e.g. images/surfaces) is a different axis:
there `arg` becomes a multi-column grid / list of grids, which the composition
design does not address. The `tf_mv` name is reserved for multivariate *output*;
a future multivariate-input class (e.g. `tfd_surface`) would slot under `tf`
alongside `tf_mv`. No surface code is shipped yet, to avoid dead scaffolding.

## Files

* `R/tfd-mv.R` — `new_tf_mv()`, `tfd_mv()` constructors, `as.tfd_mv()`.
* `R/tfb-mv.R` — `tfb_mv()` constructors, `as.tfb_mv()`.
* `R/mv-vctrs.R` — proxy/restore, ptype2/cast, ptype abbr/full.
* `R/mv-methods.R` — accessors, `$`, `[`, evaluate, ops/math/summary,
  print/format, plot, converters.
* Tests: `tests/testthat/test-tfd-mv.R`, `test-tfb-mv.R`, `test-mv-vctrs.R`,
  `test-mv-methods.R`, `test-mv-verbs.R`.
