# Test Instructions for NA Handling Fix

## Running Tests Locally

Unfortunately, R is not installed in this environment. To run the full test suite, please use one of the following methods:

### Method 1: R Console (Recommended)

```r
# Load the package
devtools::load_all()

# Run all tests
devtools::test()

# Or run just the new NA consistency tests
testthat::test_file("tests/testthat/test-na-consistency.R")
```

### Method 2: Command Line

```bash
# From the package root directory
Rscript -e "devtools::test()"

# Or run R CMD check
R CMD check .
```

### Method 3: GitHub Actions

The tests will automatically run via GitHub Actions when you push to the repository or create a pull request to the `dev` branch.

## Expected Test Results

### New Tests (test-na-consistency.R)

The new test file includes 20+ test cases covering:

1. **Concatenation operations**: All should pass with NULL entries for NAs
2. **Subindexing/assignment**: All should create NULL entries consistently
3. **Arithmetic operations**: NA propagation should create NULL entries
4. **Class transformations**: NAs should remain NULL through conversions
5. **Edge cases**: Constructors, printing, formatting all handle NAs correctly

### Existing Tests

All existing tests should continue to pass. The changes made are:

- `new_tfd()` now creates NULL entries for NAs (instead of list(arg=domain[1], value=NA))
- Operations creating all-NA results convert to NULL entries
- Print functions handle all-NA objects without errors

### Known Issues (if any)

Based on existing comments in test-concatenation.R:
- `c(NA, x)` may not work due to a vctrs issue - this is a known limitation
- Our tests handle this with `suppressWarnings()` and `try()` blocks

## Verifying the Fix

To manually verify the fix works, run these examples:

```r
library(tf)

# Test 1: c(tf_rgp(3), NA) should work without errors
set.seed(123)
x <- tf_rgp(3)
str(c(x, NA))
# Expected: 4th entry should be NULL

# Test 2: tf_rgp(3) - NA_real_ should create NULL entries
set.seed(123)
x <- tf_rgp(3)
result <- x - NA_real_
str(result)
print(result)
# Expected: All entries NULL, no interpolation errors

# Test 3: Setting all to NA should display [NA, NA] range
set.seed(123)
x <- tf_rgp(3)
x[1:3] <- NA
print(x)
# Expected: Range shows [NA, NA] not [Inf, -Inf]
```

## CI/CD

The package uses GitHub Actions for continuous integration:
- `.github/workflows/quick-check.yaml` - Runs on push/PR to dev branch
- `.github/workflows/full-check.yaml` - Comprehensive R CMD check
- `.github/workflows/test-coverage.yaml` - Code coverage reporting

All workflows will automatically test the new changes.
