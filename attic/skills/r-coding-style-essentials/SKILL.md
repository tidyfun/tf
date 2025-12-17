---
name: r-coding-style-essentials
description: Essential R coding standards for scripts and data analysis projects. Covers naming (snake_case), pure functions with no global dependencies, tidyverse-friendly patterns (dplyr, purrr), basic validation with stopifnot, informative error messages, functional programming, and code organization. For general projects, not package development.
---

# R Coding Style Essentials

## When to Use This Skill

Apply these guidelines when:
- Writing R scripts for data analysis
- Creating general R projects (not packages)
- Using tidyverse for data wrangling
- Prototyping or exploratory analysis
- Working with smaller codebases

**Not for**: Package development (use r-coding-style-guide instead)

## Core Principles

- **Clear over clever** - readability trumps brevity
- **Simple over sophisticated** - solve the problem, don't over-engineer
- **Explicit over implicit** - make intentions obvious
- **Functional over imperative** - prefer `map()` over loops when clearer

**Write for humans first, computers second.**

---

## Naming Conventions

### Functions
**Always use snake_case** (no camelCase, no dots)

```r
# GOOD
calculate_mean <- function(x) { mean(x, na.rm = TRUE) }
filter_outliers <- function(data, sd_threshold = 3) { ... }
plot_results <- function(results) { ... }

# BAD
calculateMean <- function(x) { ... }
filter.outliers <- function(data, threshold) { ... }
```

- **Verbs for actions**: `calculate_mean()`, `filter_data()`, `plot_results()`
- **Nouns for getters**: `get_data()`, `extract_values()`
- **Consistent prefixes** for related functions: `plot_scatter()`, `plot_timeseries()`

### Variables
- **Descriptive names**: `patient_ages`, `sales_by_region`, `model_predictions`
- **Plural for collections**: `results`, `measurements`, `predictions`
- **Underscores for word separation**: `max_value`, `start_date`, `error_rate`
- Avoid single letters except `i`, `j`, `x`, `y` in standard math contexts

### Constants
**Use UPPER_SNAKE_CASE**

```r
MAX_ITERATIONS <- 1000
DEFAULT_ALPHA <- 0.05
DATA_PATH <- "data/customers.csv"
```

---

## Code Formatting

### Spacing and Assignment

```r
# CORRECT
result <- x + y
func(arg1, arg2, arg3)
list(a = 1, b = 2)

# WRONG
result<-x+y
result = x + y  # Use <- for assignment, not =
func(arg1,arg2,arg3)
```

### Pipes

Use **native pipe** `|>` (R ≥ 4.1) or **magrittr** `%>%` for older R

```r
# Good - one operation per line
result <- raw_data |>
  filter(age > 18) |>
  mutate(log_income = log(income)) |>
  group_by(region) |>
  summarize(mean_income = mean(log_income))

# Also OK for short chains
count <- data |> filter(valid) |> nrow()
```

### Lambda Functions

```r
# Modern R (≥ 4.1)
map(data, \(x) x^2 + 1)
map2(x, y, \(a, b) a * b)

# Older R or when more readable
map(data, function(x) process(x))
```

### Style Rules
- **2 spaces** for indentation (not tabs)
- **~80 characters** per line (flexible)
- Break long function calls after commas

---

## Function Design

### Standard Structure

```r
my_function <- function(data, threshold = 0.05, verbose = FALSE) {
  # 1. Validate inputs
  stopifnot(is.data.frame(data), threshold > 0)

  # 2. Handle edge cases early
  if (nrow(data) == 0) return(NULL)

  # 3. Main logic
  result <- process_data(data, threshold)

  # 4. Return
  result
}
```

### Pure Functions: CRITICAL RULE

**NO HIDDEN DEPENDENCIES: All inputs must be explicit function arguments.**

```r
# BAD: Using global variable
THRESHOLD <- 0.05  # Global constant
filter_data <- function(df) {
  df |> filter(p_value < THRESHOLD)  # Reads from global ❌
}

# GOOD: Threshold as argument
filter_data <- function(df, threshold = 0.05) {
  df |> filter(p_value < threshold)  # Explicit argument ✓
}

# BAD: Reading from environment
process_data <- function(data) {
  settings <- readRDS("settings.rds")  # Hidden file dependency ❌
  transform(data, settings)
}

# GOOD: Pass settings explicitly
process_data <- function(data, settings) {
  transform(data, settings)  # Explicit argument ✓
}
```

**Why:** Functions should be testable, reusable, and predictable. Same inputs → same outputs.

### Best Practices
- **Short**: ~50 lines or less
- **One responsibility** per function
- **Required args first**, optional with defaults after
- **Sensible defaults** that work for most cases
- **Extract functions**: If you copy-paste 3 times, make it a function

---

## Control Flow

### Early Returns

Reduce nesting by returning early:

```r
calculate_score <- function(x, y) {
  if (is.null(x)) return(0)
  if (is.null(y)) return(0)
  if (length(x) == 0) return(0)

  compute_complex_score(x, y)  # Main logic without deep nesting
}
```

### Conditionals

```r
# Explicit if-else for clarity
if (condition) {
  do_something()
} else {
  do_something_else()
}

# Vectorized operations
result <- ifelse(x > 0, "positive", "negative")

# Multiple conditions (tidyverse)
category <- case_when(
  score >= 90 ~ "A",
  score >= 80 ~ "B",
  score >= 70 ~ "C",
  TRUE ~ "F"
)
```

### switch() for Discrete Options

```r
result <- switch(
  method,
  "mean" = mean(x, na.rm = TRUE),
  "median" = median(x, na.rm = TRUE),
  "mode" = calculate_mode(x),
  stop("Unknown method: ", method)
)
```

---

## Functional Programming with purrr

### Basic Usage

```r
# map() returns list, map_*() returns typed vector
results <- map(data, \(x) process(x))
numbers <- map_dbl(data, \(x) compute_value(x))
flags <- map_lgl(data, is_valid)

# Multiple inputs
combined <- map2(x, y, \(a, b) a + b)
results <- pmap(list(x, y, z), \(a, b, c) a * b + c)

# Filtering
valid <- keep(items, is_valid)
invalid <- discard(items, is_valid)
```

### Error Handling in map

```r
# possibly() returns default on error
safe_mean <- possibly(mean, otherwise = NA)
means <- map_dbl(data_list, safe_mean)

# safely() catches errors and returns result + error
safe_log <- safely(log)
results <- map(values, safe_log)
```

---

## Working with Data (tidyverse)

### dplyr Pipelines

```r
result <- raw_data |>
  filter(!is.na(value)) |>
  mutate(
    log_value = log(value),
    value_scaled = scale(value)
  ) |>
  group_by(category) |>
  summarize(
    n = n(),
    mean = mean(value),
    sd = sd(value)
  ) |>
  arrange(desc(mean))
```

### across() for Multiple Columns

```r
# Apply function to columns by type
df |> mutate(across(where(is.numeric), scale))

# Apply to columns by name pattern
df |> mutate(across(starts_with("date_"), as.Date))

# Summarize multiple columns
df |> summarize(across(where(is.numeric), list(mean = mean, sd = sd)))
```

### Selecting Columns

```r
select(df, id, value, category)           # By name
select(df, starts_with("date_"))          # By pattern
select(df, ends_with("_score"))           # By suffix
select(df, contains("temp"))              # By substring
select(df, where(is.numeric))             # By type
```

---

## Error Handling and Validation

### Input Validation

```r
# stopifnot() with descriptive messages (R ≥ 4.0)
calculate_ratio <- function(numerator, denominator) {
  stopifnot(
    "numerator must be numeric" = is.numeric(numerator),
    "denominator must be numeric" = is.numeric(denominator),
    "denominator cannot be zero" = all(denominator != 0)
  )
  numerator / denominator
}

# Manual checks with informative messages
if (!is.numeric(x)) {
  stop("x must be numeric, got ", class(x))
}

if (any(x < 0)) {
  warning("Negative values found in x, setting to 0")
  x[x < 0] <- 0
}
```

### Informative Error Messages

```r
# BAD: Cryptic
if (x < 0) stop("Invalid x")

# GOOD: Descriptive and actionable
if (x < 0) {
  stop(
    "x must be non-negative, got x = ", x, "\n",
    "Try using abs(x) or filtering negative values"
  )
}
```

### Try-Catch for Expected Failures

```r
result <- tryCatch(
  risky_computation(data),
  error = function(e) {
    message("Computation failed: ", e$message)
    NULL
  }
)

# Or use possibly() for simpler cases
safe_compute <- possibly(risky_computation, otherwise = NULL)
result <- safe_compute(data)
```

---

## Common Patterns

### Null Coalescing

```r
# Use %||% (from rlang/purrr)
value <- user_input %||% default_value

# Equivalent to
value <- if (is.null(user_input)) default_value else user_input
```

### Default Arguments Based on Other Args

```r
plot_data <- function(data, x_col, y_col, title = NULL) {
  # Set default title based on column names
  title <- title %||% paste(y_col, "vs", x_col)
  # ... plotting code
}
```

### Named Lists for Multiple Returns

```r
compute_stats <- function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    n = sum(!is.na(x))
  )
}

stats <- compute_stats(data)
stats$mean  # Access by name
```

### Working with Factors

```r
# Create factors with explicit levels
category <- factor(
  raw_values,
  levels = c("low", "medium", "high"),
  ordered = TRUE
)

# Use forcats (tidyverse)
library(forcats)
category <- fct_infreq(category)  # Order by frequency
category <- fct_relevel(category, "high", "medium", "low")
category <- fct_lump(category, n = 5)  # Keep top 5, lump rest as "Other"
```

---

## Performance

### Vectorization

```r
# GOOD: Vectorized operations
result <- x^2 + 2 * x + 1
scaled <- (x - mean(x)) / sd(x)

# AVOID: Explicit loops for simple operations
result <- numeric(length(x))
for (i in seq_along(x)) {
  result[i] <- x[i]^2 + 2 * x[i] + 1
}
```

### Pre-allocation

When you must loop, pre-allocate:

```r
n <- 10000
results <- vector("list", n)  # Pre-allocate
for (i in seq_len(n)) {
  results[[i]] <- compute(i)
}

# Don't grow objects in loops (slow!)
results <- list()
for (i in seq_len(n)) {
  results[[i]] <- compute(i)  # BAD: grows each iteration
}
```

### data.table for Large Data

```r
library(data.table)
dt <- as.data.table(large_df)
result <- dt[, .(mean_val = mean(value)), by = category]
```

---

## Anti-Patterns to Avoid

### Don't Over-Engineer

```r
# BAD: Premature abstraction
create_abstract_processor <- function(data, pipeline, validators) { ... }

# GOOD: Simple solution
process_data <- function(data) {
  data |>
    filter(!is.na(value)) |>
    mutate(value_scaled = scale(value))
}
```

### Don't Use Magic Numbers

```r
# BAD
significant_results <- filter(results, p_value < 0.05)
adults <- filter(patients, age >= 18)

# GOOD
ALPHA <- 0.05
ADULT_AGE <- 18
significant_results <- filter(results, p_value < ALPHA)
adults <- filter(patients, age >= ADULT_AGE)
```

### Don't Repeat Yourself (DRY)

```r
# BAD: Repetitive code
summary_a <- data_a |> filter(valid) |> summarize(mean = mean(value))
summary_b <- data_b |> filter(valid) |> summarize(mean = mean(value))
summary_c <- data_c |> filter(valid) |> summarize(mean = mean(value))

# GOOD: Extract to function
calculate_summary <- function(data) {
  data |> filter(valid) |> summarize(mean = mean(value))
}
summaries <- map(list(data_a, data_b, data_c), calculate_summary)
```

### Don't Modify Inputs Silently

```r
# BAD: Modifies input unexpectedly
add_column <- function(df) {
  df$new_col <- 1:nrow(df)
  df
}

# GOOD: Clear that you're returning modified copy
add_column <- function(df) {
  df |> mutate(new_col = row_number())
}
```

### Don't Swallow Errors

```r
# BAD: Silent failures
process_all <- function(items) {
  map(items, \(x) try(process(x), silent = TRUE))
}

# GOOD: Log failures
process_all <- function(items) {
  map(items, \(x) {
    tryCatch(
      process(x),
      error = function(e) {
        message("Failed to process item: ", e$message)
        NA
      }
    )
  })
}
```

---

## Code Organization

### Script Structure

```r
# 1. Header comment
# Analysis of customer churn rates
# Author: Your Name
# Date: 2024-01-15

# 2. Load libraries
library(tidyverse)
library(lubridate)

# 3. Set constants
DATA_PATH <- "data/customers.csv"
OUTPUT_PATH <- "output/churn_analysis.pdf"
CHURN_THRESHOLD <- 90  # days

# 4. Define functions
calculate_churn_rate <- function(data, threshold) {
  # ...
}

# 5. Load data
customers <- read_csv(DATA_PATH)

# 6. Main analysis
churn_rates <- calculate_churn_rate(customers, CHURN_THRESHOLD)

# 7. Output results
ggsave(OUTPUT_PATH, plot_results(churn_rates))
```

### Section Headers (RStudio)

Use `####` for collapsible sections:

```r
# Data Loading ####
data <- read_csv("data.csv")

# Data Cleaning ####
data_clean <- data |> filter(...)

# Analysis ####
results <- analyze(data_clean)

# Visualization ####
plots <- create_plots(results)
```

### When to Split Into Multiple Files

- **One file per major task** (import.R, clean.R, analyze.R, visualize.R)
- **Source shared functions** from separate file

```r
# shared_functions.R
calculate_metrics <- function(data) { ... }

# main_analysis.R
source("shared_functions.R")
results <- calculate_metrics(data)
```

---

## Comments

### When to Comment

```r
# GOOD: Explain WHY, not WHAT
# Use log transformation to normalize right-skewed distribution
data <- data |> mutate(value_log = log(value + 1))

# BAD: Stating the obvious
# Calculate the mean
mean_value <- mean(data$value)

# GOOD: Document assumptions
# Assumes data is already sorted by date
rolling_avg <- zoo::rollmean(values, k = 7, align = "right")

# GOOD: Explain non-obvious choices
# Add 1 to avoid log(0) for zero values
data <- data |> mutate(value_log = log(value + 1))
```

---

## Quick Checklist

Before running your script:

- [ ] Clear variable names (no `x`, `temp`, `data2` unless very local)
- [ ] Functions are focused (each does one thing well)
- [ ] **Functions are pure: all inputs as arguments (no globals)**
- [ ] No magic numbers (constants are named)
- [ ] Basic input validation (at least `stopifnot()`)
- [ ] Pipes are readable (one operation per line when piping)
- [ ] No repetition (extracted repeated code to functions)
- [ ] Comments explain why, not what
- [ ] Consistent naming throughout
- [ ] Works in fresh R session (all dependencies sourced)

---

## Summary

**Good R code is:**

1. **Readable** - someone else (or future you) can understand it
2. **Consistent** - follows the same patterns throughout
3. **Simple** - solves the problem without unnecessary complexity
4. **Functional** - breaks work into composable pieces
5. **Defensive** - validates inputs and handles errors gracefully

**Remember:** Code is read many more times than it is written. Optimize for reading, not writing.
