Markdown
## Resubmission

This is a resubmission of the ItemRest package (version 0.2.4). 

I have addressed the feedback from users regarding the flexibility of the item removal criteria. The following updates have been implemented:

* Added a `min_loading` parameter to the `itemrest()` function, allowing users to define their own threshold for identifying low-loading items (default is set to 0.30).
* Added a `loading_diff` parameter to allow for flexible cross-loading identification.
* Updated the default threshold for cross-loading differences to 0.10, as requested by users.
* Included support for Howard (2016) heuristics by allowing the `loading_diff` argument to be set to `"howard"`.
* Updated the `identify_problem_items()` and `test_removals()` internal functions to support these new parameters.
* All documentation, examples, and the package vignette have been updated to reflect these changes.

## Test results

The package passes `R CMD check --as-cran` with 0 errors and 0 warnings.

Tested on:
* Local Windows 11 installation, R 4.3.1
* win-builder (devel and release)

Thank you for your time and consideration.
