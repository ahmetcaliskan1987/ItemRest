## Update

This is an update for the ItemRest package (version 0.2.5). 

This version fixes a minor but important reporting bug discovered in the previous version (0.2.4) regarding the display of factor loading ranges.

* Fixed: The `loading_range` calculation in the summary table was incorrectly using absolute values. It has been updated to use raw loading values to correctly preserve and display negative signs, as required by psychometric reporting standards.
* No changes were made to the core logic or the automated selection algorithm.

## Test results

The package passes `R CMD check --as-cran` with 0 errors and 0 warnings.
There is 1 NOTE regarding "future file timestamps" due to a local system clock synchronization issue, which does not affect package functionality.

Tested on:
* Local Windows 11 installation, R 4.3.1
* win-builder (devel and release)

Thank you for your time and consideration.
