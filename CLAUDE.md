# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Structure Overview

This repository contains code for `tf`, a package defining functional data classes and methods with minimal dependencies.
Another package `tidyfun` is an extension package providing tidyverse integration for functional data analysis.

## Key components:

- `tf`: Defines `tf`, `tfd`, and `tfb` classes for functional data representation
- Core operations: arithmetic, summarizing, evaluating, differentiating/integrating 
- data integration: data wrangling with functional data columns

## Development Workflow

1. For new features or bug fixes:
   - Develop an implementation plan 
   - Create a feature branch from 'dev'
   - Write unit tests for proposed features/bug fixes
   - Implement changes and test them
   - Document with roxygen2
   - Submit PR against 'dev' branch

2. When modifying existing functions:
   - Update roxygen documentation
   - Update/add unit tests
   - Check for downstream effects on dependent functions

3. For new functions:
   - Develop an implementation plan first   
   - Follow naming conventions (tf_* prefix)
   - Add comprehensive tests
   - Include roxygen documentation with examples


## Build/Test Commands

- Full package check: `Rscript -e "devtools::check()"`
- In R session:
  - Update documentation: `devtools::document()`
  - Re-load package: `devtools::load_all()`
  - Run all tests: `devtools::test()`
  - Run single test file: `testthat::test_file("tests/testthat/test-filename.R")`
  - Code coverage: `covr::package_coverage()`
- In the terminal, wrap the commands above like this: `Rscript -e "devtools::document(); devtools::load_all(); <COMMAND>"`

## Code Style Guidelines

- Function naming: Use snake_case with `tf_` prefix for exported functions
- Method implementation: Use `ClassName.method` pattern for S3 methods
- Documentation: Use roxygen2 with markdown formatting (@param, @return)
- Imports: Declare dependencies in DESCRIPTION file; avoid tidyverse; avoid adding dependencies to additional packages.
- Input checks & error handling: Use checkmate-assertions and the custom assertions from assertions.R. 
  If none are applicable, use cli::cli_abort() or cli::cli_warning() for errors and warnings, respectively. 
- Object creation: Follow vctrs framework for S3 classes
- Testing: Write unit tests with testthat v3 (expect_equal, expect_error)
- PRs: Make pull requests against the 'dev' branch

Always reload the package with `devtools::load_all()` before testing changes.

Only perform the minimal set of code changes that are sufficient to achieve the desired result. 
Never change the user interface of exported functions unless it is absolutely necessary to achieve the desired result.

