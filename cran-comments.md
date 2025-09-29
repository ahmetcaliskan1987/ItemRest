# CRAN comments for ItemRest 0.2.2

## Resubmission
This resubmission addresses CRAN pretest NOTEs.

* Excluded the `.github` folder from the build by adding `^\\.github$` to `.Rbuildignore`.
* Fixed a spelling issue in DESCRIPTION (“reproducible” instead of “replicable”).
* Updated version and NEWS.md accordingly.

## Test environments
* Local: macOS 14.x, R 4.4.x
* win-builder: R-devel (Windows)
* rhub: Ubuntu 22.04 (R-release), Fedora (R-devel), Windows (R-release)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
None (new package).

## Additional notes
All examples run quickly and do not require external services or network access.

