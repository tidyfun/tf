# `tf` — from-the-ground-up design & code review (June 2026)

Scope: the whole package on branch `claude/ground-up-review` (= `claude/jolly-planck-nvXkZ`,
i.e. **including** the multivariate/`tf_mv` + registration + MFPCA PR, ~8.6k added lines vs `main`).
Five parallel deep reviews (core type system; operations layer; `tf_mv` layer; registration/MFPCA;
package-level hygiene), findings deduplicated and re-ranked here. Items marked **[verified]** were
reproduced at the console or confirmed in source during the review.

---

## Executive summary

The package's foundations are good and most of the big design calls were right: a vctrs
list-vctr of per-function evaluations with vector-level metadata; real ptype2/cast matrices with
thought-through loss semantics; class hierarchies (`tf > tfd > tfd_reg/tfd_irreg`,
`tf > tfb > tfb_spline/tfb_fpc`) that earn their keep; refusal of recycling and cross-type
arithmetic; lossy round-trips that warn. The new MFPCA implementation is mathematically correct
(Happ & Greven weighting verified independently; exact full-rank reconstruction to 5.6e-15) and is
tested against real invariants. The `tf_mv` composition design — components in an attribute, a
data-frame proxy for vctrs — is the right representation; the team's own design memo argued it
well, and `vec_c`/`bind_rows`/joins/tibble columns all behave.

The package is nevertheless **not in releasable shape**, for three systemic reasons:

1. **The invariants are enforced by convention, not by code, and they are broken in shipping
   paths.** There is no `validate_tf()`. Three independent reviews each reproduced silent data
   corruption within minutes: `tf_arg<-` destroys all values of an irregular vector; `tfd.list`'s
   regularity check is logically inverted and NA-pads mismatched curves without a word;
   `tf_evaluate` returns values at the wrong positions for duplicated arg values. Every confirmed
   bug lives in a path with zero test coverage — setters, `.default` methods, dots-forwarding,
   irregular n>1 calculus, error branches.

2. **The `tf_mv` inheritance contract is false.** `tf_mv` inherits `"tf"` and the docs promise
   un-overridden verbs do "the right thing component-wise". In reality ~10 exported verbs fail
   uncontrolled on `tf_mv` (deep internal errors or, worse, NA garbage with a base-R warning),
   and several seams are silently wrong (`[<-` broadcasts a univariate function into every
   component; `var(f, g)` ignores `g`; `median()` returns a chimera trajectory stitched from
   different observed curves). The PR should not have been merged as-is — though the fix is
   bounded, not a rewrite.

3. **The registration layer has accumulated heuristic sediment.** Warp "strictification" with
   silent identity-warp fallbacks, a Procrustes loop whose convergence check compares objectives
   against *different templates*, an undocumented workaround for an (unfiled?) upstream fdasrvf
   bug applied at the wrong point in the iteration, and two reproducible warp-corruption bugs
   (domain wider than arg range; NA landmarks). The MFPCA half of the same PR shows what the
   registration half should look like: invariant-tested, with the math written down.

Cross-cutting: the 1,072-line vignette for the flagship new feature was written and then parked in
`attic/` — the mv layer ships with no long-form docs, no NEWS entry, and no README mention. The
plotting code has zero tests (and a literal `ol =` typo for `col =` to show for it). And there is
a plausible **license incompatibility**: GPL-2-only code vendored from softImpute into an
`AGPL (>= 3)` package.

Recommended order of work: §A (correctness, days) → §B (contracts, ~a week) → §C (consolidation,
opportunistic). Detailed findings follow.

---

## A. Confirmed bugs — fix before anything else

| # | What | Where | Status |
|---|------|-------|--------|
| A1 | `tf_arg<-.tfd_irreg` builds `list(arg=, data=)` instead of `list(arg=, value=)`; after `tf_arg(x) <- tf_arg(x)` all evaluations are `NULL`, `is.na(x)` all `TRUE`. Silent total data loss from a no-op. Found independently by three reviewers. | `R/methods.R:192` | **[verified]** |
| A2 | `tfd.list` regularity check inverted: `empty <- lens != 0` is the *non*-empty mask, so `lens[!empty]` compares lengths of the empty entries (vacuously equal). `tfd(list(1:3, 1:5), arg = 1:5)` silently returns a `tfd_reg` with the first curve NA-padded — and those NAs are invisible to `is.na.tf`, which checks only element 1. | `R/tfd-class.R:284-287` | **[verified]** |
| A3 | Duplicated arg values ⇒ wrong values at wrong positions: `zoo_wrapper` sorts+uniquifies, `evaluate_tfd_once` assigns the shorter vector positionally into `ret[!seen]`. `tf_evaluate(x, c(.25, .25, .35))` returns the .35-value at position 2. Reachable from `x[i, c(.25, .25, .35)]`. | `R/evaluate.R:64-79`, `R/approx.R:1-13` | **[verified]** |
| A4 | Antiderivative of irregular `tfd` with n>1 dies: per-curve arg list flattened via `unlist()` before reconstruction → internal "Must be sorted" assertion. | `R/calculus.R:394-407` | **[verified]** |
| A5 | `tf_rebase.tfb.tfd` splices `...` twice (`modifyList` then again in `append`) → "matched by multiple actual arguments" whenever any dot is passed, contradicting its docs. | `R/rebase.R:146-152` | **[verified]** |
| A6 | `new_tfb_spline()` accepts but ignores `arg`; `tf_rebase(tfd, tfb_spline)` therefore fits on the wrong grid (hard mgcv error for mismatched k, or a result that does **not** share a basis with its own rebase target, triggering spurious lossiness warnings on subsequent arithmetic). The fpc path (`rebase.R:119`) does it right. | `R/tfb-spline.R:1-9`, `R/rebase.R:81-109` | **[verified]** |
| A7 | `tfb_spline.default` and `tfb_fpc.default` both crash (`new_vctr()` rejects data frames), and `tfb_fpc.default` calls the *spline* constructor. The documented return-empty-prototype behavior has never worked. | `R/tfb-spline.R:574-595`, `R/tfb-fpc.R:231-246` | **[verified]** |
| A8 | All-NA input to `tfd()` silently returns a **length-0** vector (warning is commented out) — a size-2 input becomes size 0 in a pipeline, e.g. an all-NA group in a grouped mutate. | `R/tfd-class.R:39-56` | **[verified]** |
| A9 | SRVF warps rescaled by `tf_domain()` instead of `range(arg)`: when domain ⊋ arg range (legal for `tfd`), warps come back non-monotone and `tf_register()` dies in `assert_monotonic()` with no hint. The new `srvf_mv_gamma_to_warps` copies the same bug. | `R/register.R:757-760`, `R/register-mv.R:179-188` | **[verified]** |
| A10 | Landmark registration with NA landmarks: default template (`colMeans(..., na.rm = TRUE)`) is monotonicity-checked only when *user-supplied*; crossing column means yield non-monotone warps, silently from `tf_estimate_warps`, cryptic abort from `tf_align`. | `R/register-utils.R:229-231`, `R/register.R:786-791` | **[verified]** |
| A11 | `[<-.tf_mv` broadcasts *any* non-`tf_mv` value (incl. a univariate `tf`) into every component — `g[1] <- tf_rgp(1)` silently sets x- and y-components to the same function, while `g + tf_rgp(1)` correctly errors. | `R/brackets-mv.R:147-153` | **[verified]** |
| A12 | `var.tf_mv(x, y)` carefully delegates `y` per component, but `var.tf` drops `y` on the floor: `var(f, g) == var(f)` for unrelated `g`. Silently wrong, and the mv plumbing advertises a capability that doesn't exist. | `R/ops-mv.R:102-125`, `R/summarize.R:142-144` | **[verified]** |
| A13 | `Math.tfd` discards `...`: `round(x, 1) == round(x)`, `log(x, base = 10) == log(x)`. Silent wrong answers for documented base signatures. Same in the `Math.tfb` round-trip. | `R/math.R:3-29,39` | **[verified]** |
| A14 | `all(x)`/`any(x)` error handler itself crashes: `cli_abort("{.Generic} ...")` is parsed as a malformed cli style. | `R/summarize.R:240-243` | **[verified]** |
| A15 | `plot.tf(points = TRUE)` (default for irregular!): `ol = rgb(0, 0, 0, alpha)` — typo for `col`; alpha ignored, bogus `ol` leaks into `matlines()`. Survives because graphics has **zero** test coverage. | `R/graphics.R:106` | **[verified]** |
| A16 | `summary.tf_registration` headline diagnostic (amplitude-variance reduction) is NA + a leaked base warning for **every multivariate registration** — exactly the flagship methods of the PR. The correct computation already exists in the test helper. | `R/registration-class.R:217-232` | **[verified]** |
| A17 | `sd(xb)` / `var(xb)` return invisibly for `tfb` (vestigial `ret <-` as last expression). | `R/summarize.R:45-50` | **[verified]** |
| A18 | `tf_split(x, splits = <domain boundary>)` → `if (logical(0))` crash; `include = "left"` breaks for a single split via `1:(length(end)-1)`. | `R/split-combine.R:34-39` | **[verified]** |
| A19 | `[.tf_mv` accepts NA indices silently (returns an NA curve) although NA-indexing is a documented hard error for univariate `tf` (`vec_as_location(missing = "error")`). | `R/brackets-mv.R:87-88` vs `R/brackets.R:104-111` | **[verified]** |
| A20 | `tf_integrate` on irregular data under default limits returns silent `NA`s (the in-code TODO at `calculus.R:339` admits it). The flagship calculus verb on the flagship data type is a trap. Default per-curve limits to `range(tf_arg)` for irregular input, or warn. | `R/calculus.R:339-350` | **[verified]** |

The pattern is uniform: every one of A1–A20 lives where tests are thin — replacement functions,
`.default` methods, dots-forwarding, irregular n>1, duplicated args, NA corners, error branches,
graphics. The single highest-leverage fix is an **internal `validate_tf()`** (element lengths ↔ arg
lengths, sortedness, in-domain, attribute completeness per subclass, mv `data ↔ components` sync)
run after every constructor/mutator in the test suite, plus a parametrized matrix
`{tfd_reg, tfd_irreg, tfb_spline, tfb_fpc, tfd_mv, tfb_mv} × {NA / no NA} × {n = 1, n > 1}`
over every verb, plus `expect_error(..., regexp=)` on every `cli_abort` (would have caught A14
mechanically).

---

## B. Design-level findings

### B1. Legal/release blockers that aren't code

- **License [must resolve before next CRAN release].** `R/soft-impute-svd.R` says "copied from
  softImpute … under GPL-2"; CRAN softImpute is **GPL-2 only**, which is incompatible with the
  declared `AGPL (>= 3)`. Same question for the mogsa-derived `fpc_wsvd`
  (`R/tfb-fpc-utils.R:15`; mogsa is GPL-2). Options: permission, reimplementation, Suggests-and-call,
  or relicensing. Authors of vendored code should be `cph` (not just `ctb`) regardless.
- **The mv feature is invisible.** No NEWS section, no README mention, and the 1,072-line vignette
  (`attic/vector-valued-functions.Rmd`) was written, polished over many commits, then moved to
  attic. Restore it to `vignettes/` (precompute slow fdasrvf chunks if runtime is the concern).
  Roughly 30 exports and a third of `R/` are currently undocumented at the "why would I use this"
  level.
- **No fdasrvf version floor.** The mv-SRVF code hard-depends on fdasrvf ≥ 2.4.0 APIs
  (`multivariate_karcher_mean` reorganization) and was developed against a *dev* version
  (2.4.4.9000), but DESCRIPTION says bare `Suggests: fdasrvf`. CRAN-fdasrvf-2.3.x users get a raw
  "object not exported" error. Add `fdasrvf (>= 2.4.0)` and version-aware `check_installed()`.

### B2. The `tf_mv` inheritance contract (the PR's central flaw)

`class = c(subclass, "tf_mv", "tf")` makes every `.tf` method reachable, and
`R/tfd-mv.R:142-151` promises component-wise correctness for "almost every verb". Probed reality:
`summary()`, `fivenum()`, `quantile()`, `tf_depth()`, `tf_where()`, `tf_anywhere()`, `tf_fmean()`,
`tf_crosscov()`, `tf_interpolate()`, `tf_sparsify()`, `tf_jiggle()` all fail with deep internal
errors, and `tf_fwise()` returns NA garbage with only a base-R warning. **Fix:** enumerate every
exported generic with a `.tf`/`.tfd`/`.tfb` method; for each, either implement `.tf_mv` or add a
fail-fast stub ("not defined for vector-valued <tf_mv>"); add a metaprogrammed test that walks the
generic list and asserts work-or-classed-error; rewrite the doc contract to what is true (the
inheritance buys `tf_domain()`/predicates/S4 reuse — *behavior* comes only from explicit methods).

Related semantic decisions that need an owner, not a default:

- **`median.tf_mv` returns a chimera**: per-component depth-medians select *different observed
  curves* per component — no multivariate depth notion endorses the result, and it's undocumented.
  Compute depth on a joint criterion so one curve index wins for all components; offer
  `depth = "pointwise"` as the escape hatch.
- **`tfb_mfpc` invariants undefended**: `mf + 1` aborts with a baffling internal scoring error;
  `mf$x <- other` silently discards the joint spec; `c(mf[1:4], mf[5:8])` of the *same fit* drops
  the spec (slice-keeps/concat-drops is documented only in a comment). Intercept Math/Ops with an
  explicit "demoting" warning; keep the spec in ptype2 when all inputs carry `identical()` specs.
- **`tf_arg.tf_mv` has three return shapes** (numeric / per-curve list / per-component list)
  discriminated downstream by a name-equality heuristic (`tf_mv_curve_grids`,
  `R/accessors-mv.R:205-227`) that misfires when curve names coincide with component names. This
  polymorphism is also *why* inherited verbs that default `arg = tf_arg(f)` explode. Make the
  distinction explicit.
- Construction silently widens component domains to the union under `suppressWarnings()`
  (`R/tfd-mv.R:56-63`) — the round-trip `tfd_mv(list(x = fx))$x == fx` fails on attributes (the
  package's own test dodges it with `ignore_attr = TRUE`). Document; message when widening is
  non-trivial.
- `Summary.tf_mv` (`max(f, u)`) silently broadcasts a univariate `u` across components — the exact
  mixing `vec_arith.tf_mv.default` forbids for `f + u`. One policy, applied everywhere.
- Hand-rolled record type: `new_vctr(seq_len(n))` with real data in an attribute is what
  `vctrs::new_rcrd()` exists to avoid; the dummy-integer payload is a landmine for anything
  touching `vec_data()`/`unclass()` directly. Consider migrating before the class calcifies; at
  minimum write down the `data <-> components` sync invariant in `new_tf_mv()`.

What should **not** be churned: the composition representation itself, the proxy/restore
machinery, the tidyverse interop tests, and the "mv result == univariate result per component"
test oracle. The parallel `-mv.R` file layout is fine; the duplication critique applies narrowly
(brackets argument-dance, print header — extract shared helpers) not broadly.

### B3. Core representation: three conventions for one concept

`attr(x, "arg")` is `list(<grid>)` for `tfd_reg`, the sentinel `numeric(0)` for `tfd_irreg`
(actual args live per-element as `list(arg=, value=)`), and a bare numeric for `tfb`; `tf_arg()`
returns bare / list / bare respectively. Consequences: ~30 `ensure_list(tf_arg(x))` call sites,
`is.list()` branches in `assert_arg` and `[.tf`, different element types between sibling
subclasses (hence two `is.na` methods, different equality proxies, and corruption modes like A1),
and manual class surgery (`as.tfd_irreg.tfd_reg` raw `class<-`). **Recommendation:** unify on
attribute-level `arg` as a list always (length 1 = shared grid, length n = per-element), elements
always bare numeric vectors. That deletes the sentinel, most `ensure_list` calls, and arguably
lets `tfd_reg`/`tfd_irreg` collapse into one class with a cheap predicate. This is the single
biggest structural simplification available in the package, and it directly prevents the A1 class
of bug.

### B4. Closures in attributes

`evaluator` (tfd), `basis`/`scoring_function` (tfb) are stored as closures. Costs, all observed:
equality undecidable (the FIXME at `utils.R:129-138` admits it; numerically identical vectors with
different evaluators compare all-`FALSE` with no hint); serialization bloat (3-element tfb, k=8:
**169 KB for 368 bytes of coefficients**, ~460×); and the deparse-then-`get()`-in-`parent.frame()`
resolution in `new_tfd` fails for evaluators defined in a caller's local scope while supporting a
bizarre string-double-indirection nobody asked for. **Recommendation:** store a canonical spec
(name + args — `basis_label`/`basis_args` already exist as the pattern) and resolve functions
late; accept anonymous functions by storing the function as the spec itself.

### B5. `[.tf` is four APIs in one operator

`x[i]` → tf; `x[i, j]` → matrix; `x[i, j, matrix = FALSE]` → list of data frames;
`x[cbind(i, j)]` → bare numeric (with *interpolation* semantics borrowed from a base idiom that
has none); and `x[i, , matrix = FALSE]` differs from `x[i]` via `missing()` introspection — so
`x[i]` and `x[i, ]` return different types, violating the deepest `[` convention there is.
Generic code that does `x[i, j]` mechanically gets evaluations instead of subsets.
**Recommendation:** keep `x[i]` and `x[i, j]` as interactive sugar; move the matrix/data-frame/
matrix-index forms behind `tf_evaluate()` variants with fixed return types. (`[.tf_mv` then
inherits the shared, validated index-normalization helper — also fixing A19.)

### B6. Binary-op semantics

- Result **domain is the union** while computation happens on the **intersection** of args
  (`R/ops.R:259-262`): `y1 * y2` with domains [0,1] and [0,0.5] claims [0,1] but has data to 0.5;
  later evaluation/integration over the claimed domain yields NAs. Products/sums are defined on
  the intersection; use it.
- Arg alignment is **exact-double matching** everywhere (`intersect`, `match`, `%in%`) while the
  package owns `round_resolution()` machinery it doesn't use here; mathematically-equal grids
  computed differently fall into "no common argument values". Use tolerance matching or document
  the exactness contract loudly.
- `tfb` arithmetic outside +/− (same basis) and ×/÷ (numeric, identity link) silently round-trips
  through `tfd` with `penalized = FALSE`, **discarding smoothing parameters** (in-code TODO admits
  it) and warning per operation — chained expressions trigger O(chain) re-smoothings and a wall of
  warnings. Make the warning once-per-session/option-controlled, document the `sp` loss, implement
  unary minus on coefficients.
- Which operand's names/evaluator/attributes win is decided by `vec_size(x) >= vec_size(y)`
  (`ops.R:281-297, 477-481`) — `a + b` vs `b + a` differ. Prefer first-argument-wins, document it.
- Ops with numeric *vectors* strip names while scalars preserve them (`ops.R:324-327`), with no
  rationale.

### B7. vctrs integration — good bones, two real violations

- `vec_cast.tfd_reg.tfd_reg` keeps `x`'s own `arg`, violating
  `vec_ptype(vec_cast(x, to)) == vec_ptype(to)` — the test suite admits it by skipping attribute 1.
  Either re-evaluate onto the target grid (lossy-with-consent via `maybe_lossy_cast`) or error.
- `warn_tfd_cast()` fires inside `vec_ptype2.*` — type computation must be pure; dplyr computes
  ptypes repeatedly, so joins over mixed-grid columns spray warnings. Move all signaling to casts.
- `get_resolution()` can `cli_abort` *during ptype2* on near-duplicate args — a data-quality error
  thrown while dplyr computes a common type, with no pointer to the offending input. Validate at
  construction instead.
- The S4 shims (`R/tf-s4.R`) declare slots and prototypes that contradict the real objects
  (`family` function vs list; basis signatures; `new("tfb")` would violate every S3 invariant).
  If the goal is "usable as S4 slot", `setOldClass()` suffices; the full `setClass` definitions
  are unmaintained cruft — delete or generate from one source of truth.

### B8. Registration layer

The result-object decision (`tf_registration`) is right — registration produces a bundle, and
attribute-stuffed tfds would shed it on first subset. Execution issues, beyond A9/A10/A16:

- **It's a bare list**: no `vec_size`, no tibble column, no `mutate` — in the package whose pitch
  is functional data *in data frames*. The contents are parallel vectors + constant metadata,
  i.e. exactly `vctrs::new_rcrd` shape. Also asymmetric: shape subclass stores `warps` +
  `inv_warps`, base class only `inv_warps` — the (atticed) vignette consequently writes
  `tf_invert(tf_inv_warps(reg))` four times, paying inversion error each time. Store forward
  warps (or a caching `tf_warps()` accessor) in the base class.
- **Procrustes loop convergence check is invalid** (`R/register.R:540-599`): this iteration's
  objective vs the *refined* template is compared with last iteration's vs the *old* template;
  and for SRVF the outer objective is plain L2 while the inner step minimizes Fisher–Rao — "got
  worse" in L2 is compatible with improvement in the criterion actually optimized. Per-curve NA
  integrals additionally make the objective average over a changing subset. Evaluate both
  candidates against the same template; use an amplitude distance for SRVF.
- **`srvf_mv_equalize_scale()`** patches a claimed upstream fdasrvf bug by renormalizing to mean
  arc length — *after* the template-refinement loop has already converged on the wrongly-scaled
  quantity; arc length is not the SRVF scale quotient (q-norms are); zero-length curves are
  silently skipped; and `tf_scales()` reports a quantity inconsistent with the transformation
  actually applied. Equalize inside the loop in q-space, pin the fdasrvf version exhibiting the
  bug, file the upstream issue and reference it.
- **Silent identity-warp fallback** in `strictify_domain_preserving_warp`
  (`R/register-utils.R:109-116`) — "no registration for this curve", no warning. `cli_warn` with
  the curve index.
- **Taxonomy**: `"srvf_mv"` lives inside `tf_register()`'s method enum (so every univariate method
  carries a duplicated abort block for it) while `tf_register_shape()` is a separate top-level
  function — two dispatch rules for the user to learn. `tol` silently ignored for srvf_mv;
  `max_iter` reinterpreted as Karcher `maxit` with a surprise "must be ≥ 2" error; `ref_component
  = 1L` silently registers a d-dim object by its first component, documented only in
  `?tf_mv_methods`. Unify the enum or split the function; document the defaults where users look.
- Smaller: CC `crit = 2` is Ramsay & Silverman's *minimum-eigenvalue* criterion, misdescribed as
  "first-eigenfunction variance" (the gradient itself checks out — verified); `tf_register`
  synthesizes a mean "template" for landmark registration that the docs say has none;
  `detect_landmarks` misses plateau extrema (strict sign change required) and grows vectors in
  O(n²) R loops; univariate SRVF tests are smoke-only while the mv tests check real invariants —
  copy the mv pattern down.

### B9. MFPCA (mostly praise, three watch-items)

The sqrt-weight stacking / `1/sqrt(w_j)` scaling / score projection are mutually consistent and
reconstruction is exact at full rank for unequal weights too **[verified]**. Watch-items:
(1) `crossprod(xi_w)` assumes mean-zero scores — true for `fpc_wsvd`, *not* guaranteed for the
documented arbitrary-`method` extension point (e.g. `refund::fpca.sc` BLUP scores); center or
assert. (2) `pve` is relative to univariately-*retained* variance (`pve = 0.99` under
`uni_pve = 0.9` means 99% of 90%) — standard but undocumented; one sentence fixes it.
(3) Trapezoid quadrature weights are implemented **four times** in package code
(`tfb-mfpc.R:371`, `tfb-fpc.R:49`, `tfb-fpc-utils.R:52`, `registration-class.R:246`) and a fifth
time in the tests; one helper.

---

## C. API surface, consistency, hygiene

By the numbers: 100 exports, 264 S3method directives (cast/ptype2 matrix complete and symmetric),
~7k test lines in 41 well-organized files, tests pass clean, datasets exemplary at 16 KB.
Naming discipline is high. The exceptions:

1. **Masked base/stats generics** — `sd`, `var`, `rank`, `fivenum` are redefined as generics,
   shadowing stats/base for every session; `xtfrm.tf` silently imposes a *depth-based total order*
   (MHI hard-coded) on sort/order/rank — a major semantic decision documented only in `?tf_order`.
   Prefer `tf_f*` verbs (they exist!) + honest methods on existing generics; have `xtfrm` warn on
   first use.
2. **First-argument name lottery**: `x` (`tf_fwise`, `tf_smooth`, …), `f` (`tf_where`, `tf_zoom`,
   `tf_derive`, …), `object` (`tf_evaluate`, `tf_interpolate`, `tf_rebase`); plus a parameter
   literally named `return` (`R/where.R:65`). Breaking to fix, and only getting more expensive —
   decide before 1.0.
3. **Exported plumbing with junk docs**: `ensure_list`, `unique_id` (description: "See above." —
   there is no above; `@returns` of `unique_id` is a copy-paste lie), `prep_plotting_arg`,
   unprefixed `in_range`/`%inr%`. Unexport or document for real. `coef.tfb` strips names
   (`attributes(object) <- NULL`). `man/tfmethods.Rd` renders `@param object as usual` /
   `@param ... dots` literally.
4. **Predicate bloat**: 14 `is_*` predicates incl. exact duplicates (`is_tfd_reg <- is_reg`).
   And the mv PR silently changed the contract of `is_tf()` (now TRUE for `tf_mv`;
   `is_tf_1d()` added) — downstream code written pre-mv that branches on `is_tf()` is now wrong.
   NEWS + docs, prominently.
5. **Dependencies**: `mvtnorm` imported for one `rmvnorm` call (4 lines of `chol()` replace it);
   `pracma` solely for `savgol` (move to Suggests or inline). README claims "no
   tidyverse-dependencies" while DESCRIPTION wholesale-imports **purrr**. Either soften the claim
   or migrate to base `lapply`/`vapply`.
6. **Docs/check landmines**: `man/landmarks.Rd` is `\keyword{internal}` for an *exported* function
   and lists unexported helpers in `\usage` (R CMD check WARNING); `man/vctrs.Rd` self-renamed to
   `vec_ptype2.tfd_mv.tfd_mv` (pkgdown URL break — pin `@name`); dataset docs wrong on multiple
   facts (gait "datasets package", growth gender levels, none mention the columns are `tfd` —
   the entire point); no `@examples` on `plot.tf_mv`, `fpc_wsvd`, mv converters.
7. **Coverage theater**: `# nocov` wraps the *entire* implementation of all six `tf_approx_*`
   evaluators (`approx.R:2,12`) — the package's central evaluation machinery, where bug A3 lives —
   and the whole savgol/rollmean branch of `tf_smooth`. nocov is for unreachable code. No test
   file at all for: graphics.R, plot-mv.R, smooth.R, approx.R, math.R, soft-impute-svd.R.
   `_snaps/` exists and is empty — zero snapshot tests in a package with this much bespoke
   print/format code.
8. **Duplication backlog** (mechanical, low-risk): NA-restoration implemented 4×
   (`restore_na_entries`, `tfb_na_result` — which itself has a duplicated line at ops.R:356/358 —
   and inline in Math.tfb and 3× in ops.R); `tfd_op_numeric`/`numeric_op_tfd` and the three
   `tfb_op_*` are ~90% copy-paste; eight `cum*` stubs collapsible to one dispatcher;
   `tf_fmax`/`tf_fmin`/`tf_fmedian` are one factory; the `1 * NA * x[1]` make-an-NA idiom appears
   3× (name it `tf_na_like()`); `==.tfb <- eval(`==.tfd`)` is a no-op dressed as a copy; the
   constructor families (`tfd.*`, `tfb_spline.*`, `tfb_fpc.*`) each re-implement canonicalization
   with divergent behavior — route everything through the existing `mat_2_df`/`df_2_df` funnel;
   `new_tfb_spline` is ~190 lines mixing five concerns — extract `resolve_s_args()`,
   `resolve_family()`, `report_pve()`.
9. **Performance landmines**: list-arg `tf_evaluate.tfb` constructs a full mgcv design matrix
   *per curve* (10k curves = 10k `Predict.matrix` calls — batch on the unique-arg union);
   `tf_crosscor` recomputes interpolation+means ~6×; `summarize_tf` densifies irregular vectors
   onto the union grid.
10. **Sundry**: `R/globals.R` ~15/19 entries dead (leftover ggplot geoms now in tidyfun);
    `geom-mv.R` contains differential *geometry* (norm/inner/tangent/arclength), not a ggplot Geom
    — actively confusing in an ecosystem where `geom_spaghetti()` is a real thing; rename
    `geometry-mv.R`. ~21 TODO/FIXMEs in R/, several flagging real semantic doubts
    (`tfd-class.R:266` "this will break for multivariate data!" — has mv made this live?) —
    triage into issues. `cli_alert_warning` used where a real warning condition is needed
    (`split-combine.R:137` — violates the project's own CLAUDE.md rule). "Matèrn" → "Matérn".
    `strict = F` in example code. 55 lines >100 chars with `air.toml` present — run the formatter.

---

## D. Suggested sequencing

1. **Now (correctness, ~2–3 days):** A1–A20. Most are one-to-five-line fixes; each needs the
   regression test that was missing. Add `validate_tf()` and wire it into the test helpers.
2. **Before any release (~1 week):** license resolution (B1); the `tf_mv` contract — fail-fast
   stubs + metaprogrammed coverage test + honest docs (B2); fdasrvf version floor; restore the
   vignette; NEWS/README for mv; fix the landmarks.Rd check WARNING.
3. **Next (design debt, opportunistic but soon — these get more expensive):** unify `arg` storage
   (B3) and evaluator/basis specs (B4); intersection domains + pure ptype2 (B6/B7); registration
   loop objective + scale workaround (B8); `[.tf` de-overloading (B5) and argument-name unification
   (C2) batched into one documented breaking release.
4. **Ongoing:** duplication backlog (C8), snapshot tests for print/format, plot smoke tests,
   dependency diet (C5), kill the nocov shields (C7).

## E. What's right and should be defended against future churn

The list-vctr + metadata-attributes representation; the subclass hierarchies; the loss-aware cast
matrix; refusal of recycling and of cross-type arithmetic; `tf_mv` as composition with a
data-frame proxy (vs tibble-of-columns, `list_of`, or matrix-evaluations — all correctly
rejected in `attic/design/multivariate.md`); the registration *result-object* decision; the
shared-score `tfb_fpc` trick that makes MFPCA print/plot/reconstruct for free; the MFPCA
invariant tests and the "mv == univariate per component" oracle; base-graphics-only plotting
(geoms belong in tidyfun); Fornberg finite differences and the CC analytic gradient (both check
out); dataset sizes; `.Rbuildignore` discipline.
