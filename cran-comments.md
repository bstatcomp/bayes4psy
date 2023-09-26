# Revisions

## CRAN submission 26. 9. 2023

RStan optimizations and updates.

## CRAN submission 16. 3. 2023

Optimized building of RStan models.

## CRAN submission 27. 9. 2021

Additional fixes required for newer versions of the RStan package.

## CRAN submission 10. 9. 2021

Support for newer versions of the RStan package.

## CRAN submission 14. 4. 2021

Further vignettes optimization. The package now uses a stable rstan version.

## CRAN submission 7. 12. 2020

Vignettes are now optimized to enable faster package compilation.

## CRAN submission 20. 11. 2020

Revised tests to avoid stability issues on Solaris.

## CRAN submission 19. 2. 2020

Reran all unit tests manually and corrected all checks.

## CRAN submission 17. 2. 2020

Fixxed errors denoted in the check results report.

## CRAN submission 23. 12. 2019

Everything was OK.

## CRAN submission 19. 6. 2019

1. CRAN: Omit the redundant part "An R Package for" in your title. Please do not start your Description with "This package", package name or similar.
Response: Done.

2. CRAN: Please add small executable examples in your Rd-files.
Response: Added function level examples to exported model fitting functions (b_bootstrap, b_color, b_linear, b_reaction_time, b_success_rate and b_ttest). Added class level examples to classes that represent fitted models (color_class, linear_class, reacion_time_class, success_rate_class, ttest_class), adding to particular functions here does not make sense because these functions cannot be used in a vacuum.

3. CRAN: We are missing some authors and copyright holder in the authors list: author Rasmus Baath, author John Kruschke, Trustees of Columbia University.
Response: Authors mentioned above are now added into the Authors@R field.

## Test environments

* local OS X Mojave 10.14.5, R 3.5.4
* local Windows 10, R 3.5.4
* win-builder (R-devel)
* Ubuntu Linux 16.04 LTS (rhub)
* Fedora Linux, R-devel (rhub)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTES:

* checking installed package size ... NOTE
    installed size is  6.1Mb
    sub-directories of 1Mb or more:
      libs   5.2Mb

* hecking for GNU extensions in Makefiles ... NOTE
    GNU make is a SystemRequirements.

## Downstream dependencies

There are currently no downstream dependencies for this package.
