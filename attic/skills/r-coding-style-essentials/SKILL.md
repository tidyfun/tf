---
name: r-coding-style-essentials
description: Essential R coding standards for scripts and data analysis projects. Covers naming (snake_case), pure functions with no global dependencies, tidyverse-friendly patterns (dplyr, purrr), basic validation with stopifnot, informative error messages, functional programming, and code organization. For general projects, not package development.
---

# R Coding Style Essentials

## When to Use This Skill

Apply when writing R scripts for data analysis, general projects (not packages), using tidyverse, prototyping, or exploratory analysis.

**Not for**: Package development (use r-coding-style-guide instead)

## Core Principles

- **Clear over clever** - readability trumps brevity
- **Simple over sophisticated** - solve the problem, don't over-engineer
- **Explicit over implicit** - make intentions obvious
- **Functional over imperative** - prefer `map()` over loops when clearer

---

## Naming Conventions

### Functions: snake_case always

```r
# GOOD
calculate_mean <- function(x) { mean(x, na.rm = TRUE) }
filter_outliers <- function(data, threshold = 3) { ... }

# BAD
calculateMean <- function(x) { ... }
filter.outliers <- function(data) { ... }
```

- **Verbs for actions**: `calculate_mean()`, `filter_data()`, `plot_results()`
- **Nouns for getters**: `get_data()`, `extract_values()`
- **Consistent prefixes**: `plot_scatter()`, `plot_timeseries()`

### Variables
- **Descriptive**: `patient_ages`, `sales_by_region`, `model_predictions`
- **Plural for collections**: `results`, `measurements`
- **Underscores**: `max_value`, `start_date`
- Avoid single letters except `i`, `j`, `x`, `y` in math contexts

### Constants: UPPER_SNAKE_CASE

```r
MAX_ITERATIONS <- 1000
DEFAULT_ALPHA <- 0.05
```

---

## Code Formatting

### Spacing and Assignment

```r
# CORRECT
result <- x + y
func(arg1, arg2, arg3)

# WRONG
result<-x+y
result = x + y  # Use <- not =
```

### Pipes

```r
# Good - one operation per line
result <- raw_data |>
  filter(age > 18) |>
  mutate(log_income = log(income)) |>
  group_by(region) |>
  summarize(mean = mean(log_income))
```

Use native pipe `|>` (R ≥ 4.1) or magrittr `%>%`

### Lambda Functions

```r
# Modern R (≥ 4.1)
map(data, \(x) x^2 + 1)

# Older R
map(data, function(x) process(x))
```

### Style
- **2 spaces** indentation
- **~80 chars** per line
- Break long calls after commas

---

## Function Design

### Structure

```r
my_function <- function(data, threshold = 0.05, verbose = FALSE) {
  # 1. Validate inputs
  stopifnot(is.data.frame(data), threshold > 0)

  # 2. Handle edge cases early
  if (nrow(data) == 0) return(NULL)

  # 3. Main logic
  result <- process_data(data, threshold)

  result
}
```

### Pure Functions: CRITICAL

**NO HIDDEN DEPENDENCIES: All inputs must be explicit arguments.**

```r
# BAD: Global variable
THRESHOLD <- 0.05
filter_data <- function(df) {
  df |> filter(p_value < THRESHOLD)  # ❌
}

# GOOD: Explicit argument
filter_data <- function(df, threshold = 0.05) {
  df |> filter(p_value < threshold)  # ✓
}

# BAD: Reading from environment
process <- function(data) {
  settings <- readRDS("settings.rds")  # ❌
  transform(data, settings)
}

# GOOD: Pass explicitly
process <- function(data, settings) {
  transform(data, settings)  # ✓
}
```

**Why**: Testable, reusable, predictable. Same inputs → same outputs.

### Best Practices
- **Short**: ~50 lines
- **One responsibility**
- **Required args first**, optional with defaults
- **Extract**: If copy-paste 3x, make function

---

## Control Flow

### Early Returns

```r
calculate_score <- function(x, y) {
  if (is.null(x)) return(0)
  if (is.null(y)) return(0)
  if (length(x) == 0) return(0)

  compute_complex_score(x, y)
}
```

### Conditionals

```r
# Explicit if-else
if (condition) {
  do_something()
} else {
  do_something_else()
}

# Vectorized
result <- ifelse(x > 0, "positive", "negative")

# Multiple conditions (tidyverse)
category <- case_when(
  score >= 90 ~ "A",
  score >= 80 ~ "B",
  TRUE ~ "F"
)

# switch() for discrete options
result <- switch(
  method,
  "mean" = mean(x, na.rm = TRUE),
  "median" = median(x, na.rm = TRUE),
  stop("Unknown method: ", method)
)
```

---

## Functional Programming

### purrr Basics

```r
# map() returns list, map_*() typed vector
results <- map(data, \(x) process(x))
numbers <- map_dbl(data, compute_value)
flags <- map_lgl(data, is_valid)

# Multiple inputs
combined <- map2(x, y, \(a, b) a + b)
results <- pmap(list(x, y, z), \(a, b, c) a * b + c)

# Filtering
valid <- keep(items, is_valid)
invalid <- discard(items, is_valid)

# Error handling
safe_mean <- possibly(mean, otherwise = NA)
means <- map_dbl(data_list, safe_mean)
```

---

## Working with Data (tidyverse)

### dplyr Pipelines

```r
result <- raw_data |>
  filter(!is.na(value)) |>
  mutate(
    log_value = log(value),
    scaled = scale(value)
  ) |>
  group_by(category) |>
  summarize(n = n(), mean = mean(value)) |>
  arrange(desc(mean))
```

### across() for Multiple Columns

```r
# By type
df |> mutate(across(where(is.numeric), scale))

# By pattern
df |> mutate(across(starts_with("date_"), as.Date))

# Summarize multiple
df |> summarize(across(where(is.numeric), list(mean = mean, sd = sd)))
```

### Selecting

```r
select(df, id, value)              # By name
select(df, starts_with("date_"))   # By pattern
select(df, where(is.numeric))      # By type
```

---

## Error Handling

### Validation

```r
# stopifnot() with messages (R ≥ 4.0)
calculate_ratio <- function(num, denom) {
  stopifnot(
    "num must be numeric" = is.numeric(num),
    "denom cannot be zero" = all(denom != 0)
  )
  num / denom
}

# Manual checks
if (!is.numeric(x)) {
  stop("x must be numeric, got ", class(x))
}
```

### Informative Messages

```r
# BAD
if (x < 0) stop("Invalid x")

# GOOD
if (x < 0) {
  stop("x must be non-negative, got x = ", x,
       "\nTry using abs(x) or filtering negatives")
}
```

### Try-Catch

```r
result <- tryCatch(
  risky_computation(data),
  error = function(e) {
    message("Failed: ", e$message)
    NULL
  }
)

# Or use possibly()
safe_compute <- possibly(risky_computation, otherwise = NULL)
```

---

## Common Patterns

```r
# Null coalescing
value <- user_input %||% default_value

# Default arguments
plot_data <- function(data, x, y, title = NULL) {
  title <- title %||% paste(y, "vs", x)
  # ...
}

# Named lists for returns
compute_stats <- function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    n = sum(!is.na(x))
  )
}

# Factors with forcats
library(forcats)
category <- fct_infreq(category)  # Order by frequency
category <- fct_relevel(category, "high", "medium", "low")
```

---

## Performance

```r
# GOOD: Vectorized
result <- x^2 + 2 * x + 1

# AVOID: Loop for simple ops
for (i in seq_along(x)) result[i] <- x[i]^2

# Pre-allocate when looping
results <- vector("list", n)
for (i in seq_len(n)) {
  results[[i]] <- compute(i)
}

# data.table for large data
library(data.table)
dt <- as.data.table(df)
result <- dt[, .(mean = mean(value)), by = category]
```

---

## Anti-Patterns

### Don't Over-Engineer

```r
# BAD
create_processor <- function(data, config, strategy) { ... }

# GOOD
process_data <- function(data) {
  data |> filter(!is.na(value)) |> mutate(scaled = scale(value))
}
```

### Don't Use Magic Numbers

```r
# BAD
significant <- filter(results, p < 0.05)

# GOOD
ALPHA <- 0.05
significant <- filter(results, p < ALPHA)
```

### Don't Repeat Yourself

```r
# BAD
summary_a <- data_a |> filter(valid) |> summarize(m = mean(val))
summary_b <- data_b |> filter(valid) |> summarize(m = mean(val))

# GOOD
calc_summary <- function(d) d |> filter(valid) |> summarize(m = mean(val))
summaries <- map(list(data_a, data_b), calc_summary)
```

### Don't Modify Inputs Silently

```r
# BAD
add_column <- function(df) { df$new <- 1; df }

# GOOD
add_column <- function(df) { df |> mutate(new = row_number()) }
```

### Don't Swallow Errors

```r
# BAD
try(risky(x), silent = TRUE)

# GOOD
tryCatch(risky(x), error = function(e) {
  message("Failed: ", e$message)
  NA
})
```

---

## Code Organization

### Script Structure

```r
# 1. Header, libraries, constants
library(tidyverse)
DATA_PATH <- "data/customers.csv"

# 2. Functions
calculate_churn <- function(data, days) { ... }

# 3. Load, analyze, output
customers <- read_csv(DATA_PATH)
results <- calculate_churn(customers, 90)
write_csv(results, "output/churn.csv")
```

Use section headers: `# Data Loading ####`

Split files by task, source shared: `source("shared_functions.R")`

---

## Comments

```r
# GOOD: Explain WHY
# Log transform to normalize right-skewed distribution
data <- mutate(data, log_val = log(value + 1))

# BAD: State obvious
m <- mean(x)  # Calculate mean

# GOOD: Document assumptions and choices
# Assumes data sorted by date
rolling <- zoo::rollmean(values, k = 7)
```

---

## Quick Checklist

- [ ] Clear variable names
- [ ] Functions focused (one thing each)
- [ ] **Functions pure: all inputs as args (no globals)**
- [ ] No magic numbers
- [ ] Basic validation
- [ ] Pipes readable
- [ ] No repetition (DRY)
- [ ] Comments explain why
- [ ] Consistent naming
- [ ] Works in fresh session

---

## Summary

**Good R code is:**

1. **Readable** - future you understands it
2. **Consistent** - same patterns throughout
3. **Simple** - solves problem without complexity
4. **Functional** - composable pieces
5. **Defensive** - validates inputs, handles errors

**Code is read more than written. Optimize for reading.**
