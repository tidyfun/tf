---
name: r-coding-style-guide
description: Comprehensive R coding standards for package development with AI coding agents. Covers naming conventions (snake_case with prefixes), pure functions with no hidden dependencies, S3 objects, roxygen2 documentation, checkmate validation, cli messaging, purrr functional patterns, testthat testing, and package workflow. Emphasizes clarity over cleverness and testable, reusable code.
---

# R Coding Style Guide for Package Development

## When to Use This Skill

Apply these guidelines when:
- Developing R packages (not scripts or analysis code)
- Writing code for AI coding agents to maintain
- Need formal documentation with roxygen2
- Implementing S3 object systems
- Working with minimal dependencies (no tidyverse)

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
**Always use snake_case** (no camelCase, no dots except S3 methods)

```r
# GOOD
tf_smooth_data <- function(x) { ... }
calculate_metrics <- function(data) { ... }

# BAD
tfSmoothData <- function(x) { ... }
calculate.metrics <- function(data) { ... }
```

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

### Spacing and Operators

```r
# CORRECT
result <- x + y
result <- x |> transform() |> finalize()

# WRONG
result<-x+y
result <- x %>% transform() %>% finalize()  # Don't use magrittr pipe
```

### Pipes and Functions
- Use **native pipe** `|>` (not magrittr `%>%`)
- Line breaks after pipes, indent continuation
- **Lambda functions**: `\(x)` syntax (not `function(x)`)

```r
# Good
data |>
  filter(valid) |>
  map(\(x) process(x)) |>
  summarize()
```

### Style Rules
- **Indentation**: 2 spaces (not tabs)
- **Line length**: ~80 characters (flexible)

---

## Function Design

### Standard Structure

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

### Pure Functions: CRITICAL RULE

**NO HIDDEN DEPENDENCIES: Functions must receive ALL inputs as explicit arguments.**

```r
# BAD: Accessing global variables
threshold <- 0.05  # Global
filter_significant <- function(data) {
  data |> filter(p_value < threshold)  # Uses global threshold ❌
}

# GOOD: All inputs explicit
filter_significant <- function(data, threshold = 0.05) {
  data |> filter(p_value < threshold)  # threshold is an argument ✓
}

# BAD: Reading from environment
analyze_data <- function(data) {
  config <- readRDS("config.rds")  # Hidden dependency ❌
  process(data, config$settings)
}

# GOOD: Configuration passed as argument
analyze_data <- function(data, config) {
  process(data, config$settings)  # Explicit ✓
}
```

**Why this matters:**
- **Testable**: Can call with different inputs
- **Reusable**: Not tied to specific global state
- **Predictable**: Same inputs → same outputs
- **Debuggable**: All dependencies visible in signature

**Exceptions:** Side effects must be explicit in function purpose (e.g., `save_plot()`, `write_results()`)

### Best Practices
- **Short and focused**: ~50 lines or less when possible
- **Required args first**, optional args with sensible defaults after
- Use `missing()` when `NULL` is valid input
- Return values invisibly for side-effect functions (plotting, printing)
- Document all arguments and return values

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

## Error Handling and Messages

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
# Errors - be specific and actionable
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

### cli Formatting Syntax
- `{.arg argname}` for function arguments
- `{.fn function}` for function names
- `{.cls class}` for classes
- `{.code code}` for inline code
- `{.val value}` for values
- `{variable}` for interpolation

---

## Documentation with roxygen2

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

### Documentation Rules
- **First line**: Imperative voice ("Calculate", "Transform", "Create")
- **Details**: Use cases, behavior, edge cases
- **Cross-references**: `[function_name()]` for links
- **Families**: Group with `@family` tags
- **Examples**: Real, executable examples that work
- **Internal docs**: Comment "why" not "what"

---

## Functional Programming Patterns

### Use purrr Consistently

```r
# map() returns list, map_*() returns typed vector
results <- map(data, \(x) compute(x))
means <- map_dbl(data, mean, na.rm = TRUE)
flags <- map_lgl(data, is_valid)

# Multiple inputs
combined <- map2(x, y, \(a, b) a + b)
results <- pmap(list(x, y, z), \(a, b, c) a + b * c)
```

### Error Handling in Functional Code

```r
# possibly() for expected failures
safe_compute <- possibly(risky_function, otherwise = NA)
results <- map(data, safe_compute)

# keep() and discard() for filtering
valid <- keep(items, is_valid)
invalid <- discard(items, is_valid)
```

### Extract Complex Logic

```r
# GOOD: Named helper function
results <- map(data, compute_and_validate)

# AVOID: Complex inline lambda
results <- map(data, \(x) {
  temp <- step1(x)
  validated <- step2(temp)
  step3(validated)
})
```

---

## Testing with testthat v3

### What to Test
- Every exported function
- Edge cases: empty inputs, NA values, boundaries
- Error handling (errors throw correctly)
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
- **Deterministic**: Use `set.seed()` for random data
- **Fast**: Slow tests don't get run
- **Independent**: No shared state between tests

---

## Anti-Patterns to Avoid

### Don't Over-Engineer
```r
# BAD: Premature abstraction
create_configurable_processor <- function(data, config, strategy) { ... }

# GOOD: Solve the actual problem
process_data <- function(data) {
  data |> filter(!is.na(value)) |> transform()
}
```

### Don't Modify Inputs

```r
# BAD: Modifying in place
add_col <- function(df) { df$new <- 1; df }

# GOOD: Explicit copy
add_col <- function(df) { ret <- df; ret$new <- 1; ret }
```

### Don't Swallow Errors

```r
# BAD: Silent failure
result <- tryCatch(risky(x), error = \(e) NULL)

# GOOD: Log and handle explicitly
result <- tryCatch(risky(x), error = \(e) {
  cli::cli_warn("Failed: {e$message}")
  fallback_value
})
```

### Don't Use Magic Numbers

```r
# BAD
if (p < 0.05) significant <- TRUE

# GOOD
ALPHA <- 0.05
if (p < ALPHA) significant <- TRUE
```

---

## Package Development Workflow

### File Organization
- One concept per file (`smooth.R`, `interpolate.R`)
- Helper functions at top, exported at bottom
- Related S3 methods grouped together

### Development Cycle
1. Modify code
2. `devtools::document()` - update roxygen
3. `devtools::load_all()` - test interactively
4. `devtools::test()` - run tests
5. `devtools::check()` - before committing

### Git Best Practices
- Atomic commits (one logical change)
- Clear commit messages
- PRs target `dev` branch
- Test before committing

---

## Quick Checklist

Before committing code:

- [ ] Complete roxygen2 docs with examples
- [ ] snake_case naming with consistent prefixes
- [ ] **Functions are pure: all inputs as arguments (no globals)**
- [ ] Input validation with checkmate at boundaries
- [ ] User messages use cli formatting
- [ ] Errors include fix suggestions
- [ ] Native pipe `|>` (not `%>%`)
- [ ] Lambda syntax `\(x)` (not `function(x)`)
- [ ] Descriptive variable names
- [ ] Functions < 50 lines when possible
- [ ] No new unnecessary dependencies
- [ ] Tests for new functionality
- [ ] DRY principle followed
- [ ] `devtools::check()` passes

---

## Key Principles

**Write code that is:**
- **Clear over clever** - readability trumps brevity
- **Simple over sophisticated** - solve the problem, don't over-engineer
- **Explicit over implicit** - make intentions obvious
- **Documented over "self-explanatory"** - future you needs help
- **Tested over "works on my machine"** - prove it works

**Write for humans first, computers second.**
