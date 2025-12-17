# R Coding Style Essentials

> Core coding style guidelines for R scripts and general projects. Focused on writing clear, maintainable code for data analysis and scripting work.

---

## Core Principles

**Write code that is:**
- **Clear over clever** - readability trumps brevity
- **Simple over sophisticated** - solve the problem, don't over-engineer
- **Explicit over implicit** - make your intentions obvious
- **Functional over imperative** - prefer `map()` over loops when it's clearer

**Write for humans first, computers second.**

---

## Naming Conventions

### Functions
- **snake_case** for everything (no camelCase, no dots)
- **Verbs for actions**: `calculate_mean()`, `filter_data()`, `plot_results()`
- **Nouns for getters**: `get_data()`, `extract_values()`
- **Consistent prefixes** for related functions: `plot_scatter()`, `plot_timeseries()`, `plot_heatmap()`

```r
# Good
calculate_summary_stats <- function(data) { ... }
filter_outliers <- function(x, threshold) { ... }

# Avoid
calcSummaryStats <- function(data) { ... }
filter.outliers <- function(x, threshold) { ... }
```

### Variables
- **Descriptive names**: `patient_ages`, `sales_by_region`, `model_predictions`
- **Avoid abbreviations** unless very standard: `df` for data frame, `i`/`j` for indices
- **Plural for collections**: `results`, `measurements`, `predictions`
- **Underscores to separate words**: `max_value`, `start_date`, `error_rate`

```r
# Good
patient_ages <- c(45, 52, 38, 61)
sales_by_region <- summarize(data, ...)

# Avoid single letters (except standard cases)
x <- c(45, 52, 38, 61)  # What is x?
sbr <- summarize(data, ...)  # Cryptic
```

### Constants
- **UPPER_SNAKE_CASE** for true constants
```r
MAX_ITERATIONS <- 1000
DEFAULT_ALPHA <- 0.05
DATA_PATH <- "data/raw/input.csv"
```

---

## Code Formatting

### Spacing
```r
# YES: Spaces around operators, after commas
result <- x + y
func(arg1, arg2, arg3)
list(a = 1, b = 2)

# NO: Cramped
result<-x+y
func(arg1,arg2,arg3)
```

### Pipes
- Use **native pipe** `|>` for R ≥ 4.1, or **magrittr** `%>%` for older R
- Line break after each pipe, indent continuation
```r
# Good
result <- raw_data |>
  filter(age > 18) |>
  mutate(log_income = log(income)) |>
  group_by(region) |>
  summarize(mean_income = mean(log_income))

# Also OK for short chains
result <- data |> filter(valid) |> nrow()
```

### Lambda Functions
- Use **`\(x)` syntax** in R ≥ 4.1
- Fall back to `function(x)` in older R or for clarity
```r
# Modern R (≥ 4.1)
map(data, \(x) x^2 + 1)
map2(x, y, \(a, b) a * b + a)

# Older R or when more readable
map(data, function(x) {
  transformed <- complex_transform(x)
  validate_and_return(transformed)
})
```

### Indentation and Line Length
- **2 spaces** for indentation (not tabs)
- Aim for **~80 characters** per line (flexible)
- Break long function calls after commas
```r
# Good line breaking
long_result <- complex_function(
  first_argument = value1,
  second_argument = value2,
  third_argument = value3,
  additional_options = list(opt1 = TRUE, opt2 = FALSE)
)
```

### Assignment
- Use **`<-`** for assignment (not `=`)
- Exception: `=` for function arguments
```r
# Good
result <- calculate(data)
plot(x, y, col = "blue")

# Avoid
result = calculate(data)
```

---

## Function Design

### Structure
```r
my_function <- function(data, threshold = 0.05, verbose = FALSE) {
  # 1. Input validation (quick checks at top)
  stopifnot(is.data.frame(data))
  stopifnot(threshold > 0 && threshold < 1)

  # 2. Early returns for edge cases
  if (nrow(data) == 0) {
    if (verbose) message("Empty data, returning NULL")
    return(NULL)
  }

  # 3. Main logic
  result <- process_data(data, threshold)

  # 4. Return
  result
}
```

### Best Practices
- **Short and focused**: Aim for functions that fit on one screen (~50 lines)
- **One responsibility**: Each function does one thing well
- **Required args first**, optional args with defaults after
- **Sensible defaults**: Choose values that work for most cases
- **Clear return values**: Don't return different types based on conditions

### When to Extract a Function
- **Rule of three**: If you copy-paste code 3 times, make it a function
- **Conceptual clarity**: Even if used once, extract if it clarifies intent
- **Testing**: Functions are easier to test than inline code

---

## Control Flow

### Early Returns
Reduce nesting by returning early:
```r
# Good
calculate_score <- function(x, y) {
  if (is.null(x)) return(0)
  if (is.null(y)) return(0)
  if (length(x) == 0) return(0)

  # Main logic without deep nesting
  score <- compute_complex_score(x, y)
  score
}

# Avoid deep nesting
calculate_score <- function(x, y) {
  if (!is.null(x)) {
    if (!is.null(y)) {
      if (length(x) > 0) {
        score <- compute_complex_score(x, y)
        return(score)
      }
    }
  }
  return(0)
}
```

### Conditionals
```r
# Use explicit if-else for clarity
if (condition) {
  do_something()
} else {
  do_something_else()
}

# ifelse() for vectorized operations
result <- ifelse(x > 0, "positive", "negative")

# case_when() for multiple conditions (tidyverse)
category <- case_when(
  score >= 90 ~ "A",
  score >= 80 ~ "B",
  score >= 70 ~ "C",
  TRUE ~ "F"
)
```

### switch() for Discrete Options
```r
method_result <- switch(
  method,
  "mean" = mean(x, na.rm = TRUE),
  "median" = median(x, na.rm = TRUE),
  "mode" = calculate_mode(x),
  stop("Unknown method: ", method)
)
```

---

## Functional Programming with purrr/apply

### When to Use map() vs for Loops
```r
# Use map() when transforming each element
results <- map(data_list, \(x) process(x))
values <- map_dbl(data_list, \(x) compute_value(x))

# Use for loops when:
# - You need the index explicitly
# - Early termination is important
# - Side effects are primary goal
for (i in seq_along(items)) {
  if (should_stop(items[[i]])) break
  process_with_side_effects(items[[i]], i)
}
```

### purrr Basics
```r
# map() returns list
results <- map(1:10, \(x) x^2)

# map_*() returns typed vectors
numbers <- map_dbl(data, \(x) as.numeric(x))
flags <- map_lgl(data, \(x) is_valid(x))
names <- map_chr(data, \(x) get_name(x))

# map2() for two inputs
combined <- map2(x_values, y_values, \(x, y) x + y)

# pmap() for multiple inputs
results <- pmap(
  list(x, y, z),
  \(x, y, z) x * y + z
)

# Keep/discard for filtering
valid_data <- keep(data_list, is_valid)
errors <- discard(results, is_successful)
```

### Error Handling in map
```r
# safely() catches errors
safe_log <- safely(log)
results <- map(values, safe_log)

# possibly() returns default on error
safe_mean <- possibly(mean, otherwise = NA)
means <- map_dbl(data_list, safe_mean)
```

---

## Working with Data

### Data Frames
```r
# Use tibbles for better printing (tidyverse)
library(tibble)
df <- tibble(
  id = 1:100,
  value = rnorm(100),
  category = sample(letters[1:5], 100, replace = TRUE)
)

# Or base R data.frame
df <- data.frame(
  id = 1:100,
  value = rnorm(100),
  category = sample(letters[1:5], 100, replace = TRUE),
  stringsAsFactors = FALSE  # Important for older R
)
```

### dplyr Patterns
```r
# Clear data pipelines
result <- raw_data |>
  filter(!is.na(value)) |>
  mutate(
    log_value = log(value),
    value_scaled = scale(value)
  ) |>
  group_by(category) |>
  summarize(
    n = n(),
    mean_value = mean(value),
    sd_value = sd(value)
  ) |>
  arrange(desc(mean_value))

# Use across() for multiple columns
df |>
  mutate(across(where(is.numeric), scale)) |>
  mutate(across(starts_with("date_"), as.Date))
```

### Selecting Columns
```r
# By name
select(df, id, value, category)

# By pattern
select(df, starts_with("date_"))
select(df, ends_with("_score"))
select(df, contains("temp"))

# By type
select(df, where(is.numeric))
select(df, where(is.character))
```

---

## Error Handling and Validation

### Input Checks
```r
# Quick checks with stopifnot()
calculate_ratio <- function(numerator, denominator) {
  stopifnot(
    "numerator must be numeric" = is.numeric(numerator),
    "denominator must be numeric" = is.numeric(denominator),
    "denominator cannot be zero" = all(denominator != 0)
  )
  numerator / denominator
}

# More flexible checks
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
# Bad: Cryptic
if (x < 0) stop("Invalid x")

# Good: Descriptive and actionable
if (x < 0) {
  stop(
    "x must be non-negative, got x = ", x, "\n",
    "Try using abs(x) or filtering negative values"
  )
}

# With glue for interpolation (tidyverse)
if (nrow(data) == 0) {
  stop(glue::glue(
    "Data frame is empty after filtering.\n",
    "Original had {n_original} rows, filter removed all rows."
  ))
}
```

### Try-Catch for Expected Failures
```r
# Handle expected errors gracefully
result <- tryCatch(
  {
    risky_computation(data)
  },
  error = function(e) {
    message("Computation failed: ", e$message)
    return(NULL)
  }
)

# Or more simply with possibly()
safe_compute <- possibly(risky_computation, otherwise = NULL)
result <- safe_compute(data)
```

---

## Common Patterns and Idioms

### Null Coalescing
```r
# Use %||% (rlang/purrr) or manual check
value <- user_input %||% default_value

# Equivalent to:
value <- if (is.null(user_input)) default_value else user_input
```

### Default Arguments Based on Other Args
```r
plot_data <- function(data, x_col, y_col, title = NULL) {
  # Set default title based on column names
  if (is.null(title)) {
    title <- paste(y_col, "vs", x_col)
  }
  # ... plotting code
}
```

### Named Lists
```r
# Return multiple values as named list
compute_stats <- function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    n = sum(!is.na(x))
  )
}

# Use with
stats <- compute_stats(data)
stats$mean
stats$sd
```

### Working with Factors
```r
# Create factors with explicit levels
category <- factor(
  raw_values,
  levels = c("low", "medium", "high"),
  ordered = TRUE
)

# Or use forcats (tidyverse)
library(forcats)
category <- fct_relevel(category, "high", "medium", "low")
category <- fct_infreq(category)  # Order by frequency
```

---

## Performance Tips

### Vectorization
```r
# Good: Vectorized
result <- x^2 + 2 * x + 1

# Avoid: Explicit loops for simple operations
result <- numeric(length(x))
for (i in seq_along(x)) {
  result[i] <- x[i]^2 + 2 * x[i] + 1
}
```

### Pre-allocation
```r
# When you must loop, pre-allocate
n <- 10000
results <- vector("list", n)  # Pre-allocate
for (i in seq_len(n)) {
  results[[i]] <- compute(i)
}

# Don't grow objects in loops
results <- list()
for (i in seq_len(n)) {
  results[[i]] <- compute(i)  # BAD: grows each iteration
}
```

### data.table for Large Data
```r
# For very large datasets, consider data.table
library(data.table)
dt <- as.data.table(df)
result <- dt[, .(mean_val = mean(value)), by = category]
```

---

## Anti-Patterns to Avoid

### Don't Over-Engineer
```r
# Bad: Premature abstraction
create_abstract_data_processor <- function(
  data,
  processing_pipeline,
  validation_rules,
  error_handling_strategy
) { ... }

# Good: Simple solution
process_data <- function(data) {
  data |>
    filter(!is.na(value)) |>
    mutate(value_scaled = scale(value))
}
```

### Don't Use Magic Numbers
```r
# Bad
significant_results <- filter(results, p_value < 0.05)
adult_patients <- filter(patients, age >= 18)

# Good
ALPHA <- 0.05
ADULT_AGE <- 18
significant_results <- filter(results, p_value < ALPHA)
adult_patients <- filter(patients, age >= ADULT_AGE)
```

### Don't Repeat Yourself (DRY)
```r
# Bad: Repetitive
summary_a <- data_a |> filter(valid) |> summarize(mean = mean(value))
summary_b <- data_b |> filter(valid) |> summarize(mean = mean(value))
summary_c <- data_c |> filter(valid) |> summarize(mean = mean(value))

# Good: Extract to function
calculate_summary <- function(data) {
  data |> filter(valid) |> summarize(mean = mean(value))
}
summary_a <- calculate_summary(data_a)
summary_b <- calculate_summary(data_b)
summary_c <- calculate_summary(data_c)

# Even better: Use map
summaries <- map(list(data_a, data_b, data_c), calculate_summary)
```

### Don't Modify Inputs Silently
```r
# Bad: Modifies input unexpectedly
add_column <- function(df) {
  df$new_col <- 1:nrow(df)
  df
}

# Good: Clear that you're returning modified copy
add_column <- function(df) {
  df |> mutate(new_col = row_number())
}

# Or make it explicit
add_column_inplace <- function(df) {
  df$new_col <- 1:nrow(df)
  df
}
```

### Don't Swallow Errors Silently
```r
# Bad: Silent failures
process_all <- function(items) {
  map(items, \(x) try(process(x), silent = TRUE))
}

# Good: Log failures
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

### When to Split Into Multiple Files
- **One file per major task** (import, clean, analyze, visualize)
- **Source shared functions** from separate file
```r
# shared_functions.R
calculate_metrics <- function(data) { ... }

# analysis.R
source("shared_functions.R")
results <- calculate_metrics(data)
```

---

## Comments and Documentation

### When to Comment
```r
# Good: Explain WHY, not WHAT
# Use log transformation to normalize right-skewed distribution
data <- data |> mutate(value_log = log(value + 1))

# Bad: Stating the obvious
# Calculate the mean
mean_value <- mean(data$value)

# Good: Explain non-obvious choices
# Add 1 to avoid log(0) for zero values
data <- data |> mutate(value_log = log(value + 1))

# Good: Document assumptions
# Assumes data is already sorted by date
rolling_avg <- zoo::rollmean(values, k = 7, align = "right")
```

### Section Headers
```r
# Large scripts benefit from sections
# Use RStudio's section comments: ####

# Data Loading ####
data <- read_csv("data.csv")

# Data Cleaning ####
data_clean <- data |> filter(...)

# Analysis ####
results <- analyze(data_clean)

# Visualization ####
plots <- create_plots(results)
```

---

## Quick Checklist

Before running your script:

- [ ] **Clear variable names** - no `x`, `temp`, `data2` unless very local
- [ ] **Functions are focused** - each does one thing well
- [ ] **No magic numbers** - constants are named
- [ ] **Input validation** - at least basic checks with `stopifnot()`
- [ ] **Pipes are readable** - one operation per line when piping
- [ ] **No repetition** - extracted repeated code to functions
- [ ] **Comments explain why** - not what the code does
- [ ] **Consistent naming** - all snake_case or all your chosen style
- [ ] **Works on fresh R session** - sourced all dependencies

---

## Summary

**Good R code is:**
1. **Readable** - someone else (or future you) can understand it
2. **Consistent** - follows the same patterns throughout
3. **Simple** - solves the problem without unnecessary complexity
4. **Functional** - breaks work into composable pieces
5. **Defensive** - validates inputs and handles errors gracefully

**Remember:** Code is read many more times than it is written. Optimize for reading.
