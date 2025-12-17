# R Coding Style Guide

> A distillation of coding principles and style conventions for writing clean, maintainable R code. Extracted from the `tf` package codebase, emphasizing clarity over cleverness and simplicity over sophistication.

---

## Core Philosophy

### Minimal Dependencies
- Avoid tidyverse in core packages; use only essential, stable dependencies
- Preferred: `checkmate` (validation), `cli` (messaging), `purrr` (functional), `vctrs` (vectors)
- Every dependency must earn its place

### Trust But Verify
- **Validate at boundaries**: Check all inputs at public function entry points
- **Trust internal code**: No validation between internal functions
- **Fail fast**: Catch errors early with clear, actionable messages

### Functional Over Imperative
- Prefer `map()`/`map2()`/`pmap()` over explicit loops
- Write small, composable functions (one responsibility each)
- Immutability by default

### Documentation is First-Class Code
- Every exported function needs complete roxygen2 docs with examples
- Explain *why*, not just *what*

---

## Naming Conventions

### Functions
- **snake_case** for everything (no camelCase)
- **Consistent prefixes** for related functions (`tf_smooth`, `tf_derive`, `tf_integrate`)
- **Verbs for actions**: `evaluate()`, `interpolate()`, `derive()`
- **Nouns for getters**: `tf_arg()`, `tf_domain()`
- Avoid single-letter names except standard math (`x`, `y`, `f`, `i`, `j`)

### Variables
- Descriptive names: `evaluations`, `arg_values`, `domain_limits`
- Avoid abbreviations unless standard (`arg`, `eval`)
- Plural for collections: `evaluations`, `args`, `results`
- Underscores for separation: `min_diff`, `arg_ret`

### S3 Methods
- Pattern: `generic.class` (e.g., `plot.tf`, `smooth.tfd`)
- Default methods call `.NotYetImplemented()` or use vctrs `stop_incompatible_*()`

---

## Code Formatting

### Spacing and Pipes
```r
# YES
result <- x + y
result <- x |> transform() |> finalize()

# NO
result<-x+y
result <- x %>% transform() %>% finalize()
```

- Use **native pipe** `|>` (not magrittr `%>%`)
- Line breaks after pipes, indent continuation
- **Lambda functions**: `\(x)` syntax (not `function(x)`)
- **Indentation**: 2 spaces (not tabs)
- **Line length**: ~80 characters (flexible)

---

## Function Design

### Structure
```r
my_function <- function(data, arg = NULL, verbose = TRUE) {
  # 1. Validate inputs (at top)
  assert_numeric(data)
  if (!is.null(arg)) assert_numeric(arg, len = length(data))

  # 2. Set defaults
  arg <- arg %||% seq_along(data)

  # 3. Main logic
  result <- process(data, arg)

  # 4. Return (implicit or explicit early returns)
  result
}
```

### Best Practices
- **Short and focused**: ~50 lines or less when possible
- **Required args first**, optional args with sensible defaults after
- Use `missing()` when `NULL` is valid input
- Return values invisibly for side-effect functions (plotting, printing)
- Document all arguments and return values

### Pure Functions: NO Hidden Dependencies
**CRITICAL: Functions must receive ALL inputs as explicit arguments.**

```r
# BAD: Accessing global variables
threshold <- 0.05  # Global
filter_significant <- function(data) {
  data |> filter(p_value < threshold)  # Uses global threshold
}

# GOOD: All inputs explicit
filter_significant <- function(data, threshold = 0.05) {
  data |> filter(p_value < threshold)  # threshold is an argument
}

# BAD: Reading from environment
analyze_data <- function(data) {
  config <- readRDS("config.rds")  # Hidden dependency
  process(data, config$settings)
}

# GOOD: Configuration passed as argument
analyze_data <- function(data, config) {
  process(data, config$settings)
}
```

**Why this matters:**
- Functions are **testable** - can call with different inputs
- Functions are **reusable** - not tied to specific global state
- Functions are **predictable** - same inputs always give same outputs
- Code is **debuggable** - can see all dependencies in function signature

**Exceptions:** Side effects must be explicit in function purpose (e.g., `save_plot()`, `write_results()`)

### Use of ...
- For passing args to underlying functions or extending generics
- Not as primary interface; name important arguments
- Always document what `...` is for

---

## S3 Objects

### Constructor Pattern
```r
# Low-level constructor (no validation)
new_my_class <- function(data, metadata) {
  structure(data, metadata = metadata, class = "my_class")
}

# User-facing constructor (with validation)
my_class <- function(data, ...) {
  assert_numeric(data)
  new_my_class(data, process_metadata(...))
}
```

### Method Dispatch
```r
my_generic <- function(x, ...) UseMethod("my_generic")

my_generic.default <- function(x, ...) .NotYetImplemented()

my_generic.my_class <- function(x, ...) {
  # implementation
}
```

### Using vctrs
- Use `vctrs` framework for vector-like S3 classes
- Store data in structure, metadata in attributes
- Implement `vec_ptype2()` and `vec_cast()` for type coercion

---

## Error Handling

### Input Validation with checkmate
```r
assert_numeric(x, any.missing = FALSE, finite = TRUE)
assert_character(names, min.len = 1, unique = TRUE)
assert_choice(method, choices = c("linear", "spline"))
assert_count(order, positive = TRUE)
assert_true(condition)
```

### Custom Assertions
```r
check_my_condition <- function(x) {
  if (!valid(x)) return("Descriptive error message")
  TRUE
}
assert_my_condition <- makeAssertionFunction(check_my_condition)
```

### User Messages with cli
```r
# Errors
cli::cli_abort(c(
  "Problem description",
  "i" = "How to fix it",
  "x" = "What not to do"
))

# Warnings
cli::cli_warn(c(
  "!" = "Something unexpected but we can continue",
  "i" = "Consider doing X to avoid this"
))

# Information
cli::cli_inform("i" = "Using default {.code k = {k_value}}")
```

### cli Formatting
- `{.arg argname}` for function arguments
- `{.fn function}` for function names
- `{.cls class}` for classes
- `{.code code}` for inline code
- `{.val value}` for values
- `{variable}` for interpolation

---

## Documentation (roxygen2)

### Complete Template
```r
#' One-line description (imperative voice)
#'
#' Longer description with details, use cases, caveats.
#'
#' @param x Type and description, mention constraints.
#' @param ... Additional arguments passed to [other_function()].
#' @returns Description of return value (type and structure).
#' @family function_group_name
#' @export
#' @examples
#' # Executable example showing typical usage
#' result <- my_function(1:10)
```

### Best Practices
- **First line**: Imperative ("Calculate", "Transform", "Create")
- **Details**: Use cases, behavior, edge cases
- **Cross-references**: `[function_name()]` for links
- **Families**: Group with `@family` tags
- **Examples**: Real, executable examples
- Internal docs: Comment "why" not "what"

---

## Functional Programming

### Use purrr Consistently
```r
# map() returns list, map_dbl() returns numeric, map_lgl() returns logical
results <- map(data, \(x) compute(x))
means <- map_dbl(data, mean, na.rm = TRUE)
flags <- map_lgl(data, is_valid)

# Multiple inputs
combined <- map2(x, y, \(a, b) a + b)
results <- pmap(list(x, y, z), \(a, b, c) a + b * c)
```

### Error Handling
```r
# Use possibly() for expected failures
safe_compute <- possibly(risky_function, otherwise = NA)
results <- map(data, safe_compute)

# Filter with keep() and discard()
valid <- keep(items, is_valid)
invalid <- discard(items, is_valid)
```

### Extract Complex Logic
```r
# Good: Named helper function
results <- map(data, compute_and_validate)

# Avoid: Complex inline lambda
results <- map(data, \(x) {
  temp <- step1(x)
  validated <- step2(temp)
  step3(validated)
})
```

---

## Data Structures

### Prefer Simple Structures
- **Lists** for heterogeneous data
- **Vectors** for homogeneous data
- **Named** when elements have identity
- **Attributes** for metadata (minimal)

### Common Patterns
```r
# List construction: use list() not c()
my_list <- list(x = 1, y = 2)

# Programmatic naming
setNames(values, names_vector)

# Null coalescing
value <- user_input %||% default_value

# Normalize to list
ensure_list <- function(x) if (is.list(x)) x else list(x)

# Sort unique values
sort_unique <- function(x) sort(unique(x))
```

---

## Control Flow

### Early Returns
```r
my_function <- function(x) {
  if (is.null(x)) return(default_value)
  if (length(x) == 0) return(empty_result)

  # Main logic without deep nesting
  process(x)
}
```

### switch() for Dispatch
```r
result <- switch(
  method,
  linear = linear_method(x),
  spline = spline_method(x),
  locf = locf_method(x),
  cli::cli_abort("Unknown method: {method}")
)
```

---

## Performance

### Vectorization First
```r
# Good: Vectorized
result <- x^2 + 2 * x + 1

# Avoid: Explicit loops
result <- numeric(length(x))
for (i in seq_along(x)) result[i] <- x[i]^2 + 2 * x[i] + 1
```

### When You Must Loop
- Pre-allocate results: `results <- vector("list", n)`
- Use `seq_len(n)` or `seq_along(x)` for indices

### Optimization Strategy
1. Write clear code first
2. Profile before optimizing
3. Optimize proven bottlenecks only
4. Document why optimized code is complex

---

## Testing (testthat v3)

### What to Test
- Every exported function
- Edge cases: empty, NA, boundaries
- Error handling
- Numeric accuracy

### Test Structure
```r
test_that("descriptive name of what is tested", {
  # Arrange
  input <- setup_test_data()

  # Act
  result <- my_function(input)

  # Assert
  expect_equal(result, expected)
  expect_type(result, "double")
  expect_error(my_function(bad_input), "error pattern")
})
```

### Test Quality
- Deterministic (use `set.seed()` for random data)
- Fast (slow tests don't get run)
- Independent (no shared state)

---

## Anti-Patterns

### Don't Over-Engineer
- ❌ Abstractions for single use
- ❌ "Future-proofing" features
- ❌ Unasked-for functionality
- ✅ Simple, clear solution to stated problem

### Don't Modify Inputs
```r
# Bad: Modifying in place
f <- function(x) { x$new <- 1; x }

# Good: Explicit copy
f <- function(x) { ret <- x; ret$new <- 1; ret }
```

### Don't Swallow Errors
```r
# Bad: Silent failure
result <- tryCatch(risky(x), error = \(e) NULL)

# Good: Explicit handling
result <- tryCatch(risky(x), error = \(e) {
  cli::cli_warn("Failed: {e$message}")
  fallback_value
})
```

### Don't Use Magic Numbers
```r
# Bad
if (p < 0.05) significant <- TRUE

# Good
ALPHA <- 0.05
if (p < ALPHA) significant <- TRUE
```

---

## Package Workflow

### File Organization
- One concept per file (`smooth.R`, `interpolate.R`)
- Helper functions at top, exported at bottom
- Related S3 methods grouped together

### Development Cycle
1. Modify code
2. `devtools::document()` (update roxygen)
3. `devtools::load_all()` (test interactively)
4. `devtools::test()` (run tests)
5. `devtools::check()` (before committing)

### Git and PRs
- Atomic commits (one logical change)
- Clear commit messages
- PRs target `dev` branch
- Test before committing

---

## Quick Checklist

Before committing:

- [ ] Complete roxygen2 docs with examples
- [ ] snake_case naming with consistent prefixes
- [ ] Input validation with checkmate at boundaries
- [ ] User messages use cli formatting
- [ ] Errors include fix suggestions
- [ ] Native pipe `|>` (not `%>%`)
- [ ] Lambda syntax `\(x)` (not `function(x)`)
- [ ] Descriptive variable names
- [ ] **Functions are pure: all inputs as arguments (no globals)**
- [ ] Functions < 50 lines when possible
- [ ] No new unnecessary dependencies
- [ ] Tests for new functionality
- [ ] DRY principle followed
- [ ] `devtools::check()` passes

---

## Key Principles

**Write code that is:**
- Clear over clever
- Simple over sophisticated
- Explicit over implicit
- Documented over "self-explanatory"
- Tested over "works on my machine"

**Write for humans first, computers second.**

Your future self will thank you.
