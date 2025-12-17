# R Coding Style Essentials

> Core coding style for R scripts and data analysis projects. Write clear, maintainable code.

---

## Core Principles

- **Clear over clever** - readability trumps brevity
- **Simple over sophisticated** - solve the problem, don't over-engineer
- **Explicit over implicit** - make intentions obvious
- **Functional over imperative** - prefer `map()` over loops when clearer

**Write for humans first, computers second.**

---

## Naming

### Functions
- **snake_case** for everything (no camelCase, no dots)
- **Verbs for actions**: `calculate_mean()`, `filter_data()`, `plot_results()`
- **Nouns for getters**: `get_data()`, `extract_values()`
- **Consistent prefixes**: `plot_scatter()`, `plot_timeseries()`, `plot_heatmap()`

### Variables
- **Descriptive names**: `patient_ages`, `sales_by_region`, `model_predictions`
- **Plural for collections**: `results`, `measurements`, `predictions`
- **Underscores**: `max_value`, `start_date`, `error_rate`
- Avoid single letters except `i`, `j`, `x`, `y` in standard math contexts

### Constants
- **UPPER_SNAKE_CASE**: `MAX_ITERATIONS <- 1000`, `DEFAULT_ALPHA <- 0.05`

---

## Formatting

### Spacing and Assignment
```r
# YES
result <- x + y
func(arg1, arg2, arg3)

# NO
result<-x+y
result = x + y  # Use <- not =
```

### Pipes
- Use **native pipe** `|>` (R ≥ 4.1) or **magrittr** `%>%`
- Line break after each pipe
```r
result <- raw_data |>
  filter(age > 18) |>
  mutate(log_income = log(income)) |>
  group_by(region) |>
  summarize(mean_income = mean(log_income))
```

### Lambda Functions
- **`\(x)` syntax** in R ≥ 4.1, `function(x)` otherwise
```r
map(data, \(x) x^2 + 1)
map2(x, y, \(a, b) a * b)
```

### Line Length and Indentation
- **2 spaces** for indentation
- **~80 characters** per line (flexible)
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

  # 4. Return
  result
}
```

### Best Practices
- **Short**: ~50 lines or less
- **One responsibility** per function
- **Required args first**, optional with defaults after
- **Sensible defaults** that work for most cases
- **Extract functions**: If you copy-paste 3 times, make it a function

### Pure Functions: NO Hidden Dependencies
**CRITICAL: All inputs must be explicit function arguments.**

```r
# BAD: Using global variable
THRESHOLD <- 0.05
filter_data <- function(df) {
  df |> filter(p_value < THRESHOLD)  # Hidden dependency
}

# GOOD: Threshold as argument
filter_data <- function(df, threshold = 0.05) {
  df |> filter(p_value < threshold)
}

# BAD: Reading from environment
process <- function(data) {
  settings <- readRDS("settings.rds")  # Hidden file dependency
  transform(data, settings)
}

# GOOD: Pass settings explicitly
process <- function(data, settings) {
  transform(data, settings)
}
```

**Why:** Functions should be testable, reusable, and predictable. Same inputs → same outputs.

---

## Control Flow

### Early Returns
```r
calculate_score <- function(x, y) {
  if (is.null(x)) return(0)
  if (is.null(y)) return(0)
  if (length(x) == 0) return(0)

  compute_complex_score(x, y)  # No deep nesting
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
```

### switch() for Discrete Options
```r
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
# map() returns list, map_*() returns typed vector
results <- map(data, \(x) process(x))
numbers <- map_dbl(data, \(x) compute(x))
flags <- map_lgl(data, is_valid)

# Multiple inputs
combined <- map2(x, y, \(a, b) a + b)
results <- pmap(list(x, y, z), \(a, b, c) a * b + c)

# Filter
valid <- keep(items, is_valid)
invalid <- discard(items, is_valid)
```

### Error Handling
```r
# possibly() returns default on error
safe_mean <- possibly(mean, otherwise = NA)
means <- map_dbl(data_list, safe_mean)

# safely() catches errors
safe_log <- safely(log)
results <- map(values, safe_log)
```

---

## Working with Data

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
# Apply to columns by type
df |> mutate(across(where(is.numeric), scale))

# Apply to columns by name pattern
df |> mutate(across(starts_with("date_"), as.Date))
```

### Selecting Columns
```r
select(df, id, value, category)           # By name
select(df, starts_with("date_"))          # By pattern
select(df, where(is.numeric))             # By type
```

---

## Error Handling

### Input Validation
```r
# stopifnot() with messages
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
# Bad
if (x < 0) stop("Invalid x")

# Good
if (x < 0) {
  stop(
    "x must be non-negative, got x = ", x,
    "\nTry using abs(x) or filtering negative values"
  )
}
```

### Try-Catch
```r
result <- tryCatch(
  risky_computation(data),
  error = function(e) {
    message("Computation failed: ", e$message)
    NULL
  }
)
```

---

## Common Patterns

### Null Coalescing
```r
value <- user_input %||% default_value

# Equivalent to
value <- if (is.null(user_input)) default_value else user_input
```

### Default Arguments
```r
plot_data <- function(data, x_col, y_col, title = NULL) {
  title <- title %||% paste(y_col, "vs", x_col)
  # ... plotting
}
```

### Named Lists for Multiple Returns
```r
compute_stats <- function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}

stats <- compute_stats(data)
```

### Working with Factors
```r
# Explicit levels
category <- factor(x, levels = c("low", "medium", "high"))

# forcats helpers
library(forcats)
category <- fct_infreq(category)  # Order by frequency
category <- fct_relevel(category, "high", "medium", "low")
```

---

## Performance

### Vectorization
```r
# Good: Vectorized
result <- x^2 + 2 * x + 1

# Avoid: Loop for simple operations
for (i in seq_along(x)) result[i] <- x[i]^2 + 2 * x[i] + 1
```

### Pre-allocation
```r
# Pre-allocate when you must loop
n <- 10000
results <- vector("list", n)
for (i in seq_len(n)) {
  results[[i]] <- compute(i)
}
```

### data.table for Large Data
```r
library(data.table)
dt <- as.data.table(df)
result <- dt[, .(mean = mean(value)), by = category]
```

---

## Anti-Patterns

### Don't Over-Engineer
```r
# Bad: Premature abstraction
process_with_config <- function(data, config) { ... }

# Good: Solve the actual problem
process_data <- function(data) {
  data |> filter(!is.na(value)) |> mutate(value = scale(value))
}
```

### Don't Use Magic Numbers
```r
# Bad
significant <- filter(results, p_value < 0.05)

# Good
ALPHA <- 0.05
significant <- filter(results, p_value < ALPHA)
```

### Don't Repeat Yourself
```r
# Bad: Repetition
summary_a <- data_a |> filter(valid) |> summarize(mean = mean(val))
summary_b <- data_b |> filter(valid) |> summarize(mean = mean(val))

# Good: Extract function
calc_summary <- function(d) d |> filter(valid) |> summarize(mean = mean(val))
summaries <- map(list(data_a, data_b), calc_summary)
```

### Don't Modify Inputs Silently
```r
# Bad: Unexpected modification
add_column <- function(df) {
  df$new <- 1:nrow(df)
  df
}

# Good: Clear mutation
add_column <- function(df) {
  df |> mutate(new = row_number())
}
```

### Don't Swallow Errors
```r
# Bad: Silent failure
result <- try(risky(x), silent = TRUE)

# Good: Log failures
result <- tryCatch(
  risky(x),
  error = function(e) {
    message("Failed: ", e$message)
    NA
  }
)
```

---

## Code Organization

### Script Structure
```r
# 1. Header
# Analysis: Customer churn
# Author: Name
# Date: 2024-01-15

# 2. Libraries
library(tidyverse)

# 3. Constants
DATA_PATH <- "data/customers.csv"
CHURN_DAYS <- 90

# 4. Functions
calculate_churn <- function(data, days) { ... }

# 5. Load data
customers <- read_csv(DATA_PATH)

# 6. Analysis
results <- calculate_churn(customers, CHURN_DAYS)

# 7. Output
write_csv(results, "output/churn.csv")
```

### Section Headers (RStudio)
```r
# Data Loading ####
data <- read_csv("data.csv")

# Cleaning ####
clean <- data |> filter(...)

# Analysis ####
results <- analyze(clean)
```

### Multiple Files
- One file per major task (import, clean, analyze, plot)
- Source shared functions from separate file
```r
source("shared_functions.R")
```

---

## Comments

### When to Comment
```r
# Good: Explain WHY
# Log transform to normalize right-skewed distribution
data <- mutate(data, value_log = log(value + 1))

# Bad: State the obvious
# Calculate mean
m <- mean(x)

# Good: Document assumptions
# Assumes data already sorted by date
rolling <- zoo::rollmean(values, k = 7)

# Good: Explain non-obvious choices
# Add 1 to avoid log(0)
log_vals <- log(values + 1)
```

---

## Quick Checklist

Before running:

- [ ] Clear variable names (no `x`, `temp`, `data2`)
- [ ] Functions are focused (one thing each)
- [ ] **Functions are pure: all inputs as arguments (no globals)**
- [ ] No magic numbers (constants named)
- [ ] Basic input validation
- [ ] Pipes are readable (one op per line)
- [ ] No repetition (DRY)
- [ ] Comments explain why, not what
- [ ] Consistent naming throughout
- [ ] Tested in fresh R session

---

## Summary

**Good R code is:**

1. **Readable** - future you can understand it
2. **Consistent** - same patterns throughout
3. **Simple** - solves problem without unnecessary complexity
4. **Functional** - composable pieces
5. **Defensive** - validates inputs, handles errors

**Remember:** Code is read many more times than written. Optimize for reading.
