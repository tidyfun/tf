# Methods registered on vector-valued (`tf_mv`) classes

`tf_mv` classes (`tfd_mv` / `tfb_mv`) inherit from `"tf"` so that
[`tf_domain()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md),
the type predicates
([`is_tf()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md),
[`is_tf_mv()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md),
...) and S4 generic reuse continue to work. **Behaviour** on `tf_mv`
objects, however, is supplied *only* by explicitly registered `.tf_mv`
methods: any generic without one aborts with a classed
`tf_mv_method_unimplemented` condition. This avoids silent fall-through
to the univariate method, which would otherwise produce wrong-shape
results or deep internal errors.

## Details

Some verbs are *permanently* stubbed because they have no well-defined
vector-valued semantics: [`sort()`](https://rdrr.io/r/base/sort.html),
[`rank()`](https://tidyfun.github.io/tf/dev/reference/tf_order.md) and
[`xtfrm()`](https://rdrr.io/r/base/xtfrm.html) (no canonical total order
on \\R^d\\ – use
[`tf_order()`](https://tidyfun.github.io/tf/dev/reference/tf_order.md)
with `by =` instead), and
[`tf_invert()`](https://tidyfun.github.io/tf/dev/reference/tf_invert.md)
(function inversion requires a monotone scalar function, which a
vector-valued `f: R -> R^d` is not – invert a monotone component
instead, e.g. `tf_invert(f$x)`).

Real component-wise semantics (joint vs. per-component, norm-based, ...)
are being designed verb-by-verb in
<https://github.com/tidyfun/tf/issues/255>;
[`tf_crosscov()`](https://tidyfun.github.io/tf/dev/reference/functionwise.md)
/
[`tf_crosscor()`](https://tidyfun.github.io/tf/dev/reference/functionwise.md)
remain blocked pending their joint design
(<https://github.com/tidyfun/tf/issues/274>).
