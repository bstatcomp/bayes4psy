# bayes4psy—User Friendly Bayesian Data Analysis for Psychology

<!-- badges: start -->
[![R-CMD-check](https://github.com/bstatcomp/bayes4psy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bstatcomp/bayes4psy/actions/workflows/R-CMD-check.yaml)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/bayes4psy?color=blue)](https://CRAN.R-project.org/package=bayes4psy)[![Downloads](http://cranlogs.r-pkg.org/badges/bayes4psy?color=blue)](https://CRAN.R-project.org/package=bayes4psy)
<!-- badges: end -->

## Authors

Jure Demšar, Grega Repovš and Erik Štrumbelj

* Faculty of Computer and Information Science, University of Ljubljana
* MBLab, Department of Psychology, Faculty of Arts, University of Ljubljana

## The package

This is an R package intended for Bayesian statistical analysis in the field of psychology. Psychology is one of the fields where [the replication crisis](https://en.wikipedia.org/wiki/Replication_crisis) is the most prominent. Scientist believe that one of the main reason for this might be non-transparent and inappropriate use of frequentist statistics. We believe that using fully transparent Bayesian methods provided in this package could greatly alleviate the replication crisis in psychology.

We also prepared several examples where we used the **bayes4psy** package to perform Bayesian data analysis for scientific publications on actual data gathered by psychological tests. These examples can be found on our [GitHub repository](https://github.com/bstatcomp/bayes4psy_tools). This repository also includes short test scenarios (these scenarios execute all developed functionalities on dummy data) intended for debugging the package.

This readme document provides only a brief overview of the package, a detailed description of all implemented functions is provided in package's help pages.

## Models

Bayesian models in the **bayes4psy** package are written with the [Stan](https://mc-stan.org) language. Stan functions required for working with these models are accessed via the **RStan** package. There are currently five models in the package:

* a Bayesian t-test (Bayesian alternative to the classic t-test, see John Kruschke -- Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan),
* a reaction time model (a hierarchical exponentially modified Gaussian model),
* a success rate model (a hierarchical Bernoulli-Beta model),
* a linear model (a hierarchical linear regression),
* a color model (normal/Von Mises model).

## Model fitting

The fitted model objects returned by the **bayes4psy** package are called _b\_results_ objects. These objects contain three components, _extract_ contains values of fitted parameters, _fit_ is the original _stanfit_ object and _data_ is the input data used for fitting. To fit a model to the data one has to first prepare the input data and then call an appropriate fitting function:

* b_ttest (the Bayesian t-test),
* b_reaction_time (the reaction time model),
* b_success_rate (the success rate model,
* b_linear (the linear model),
* b_color (the color model).

If no priors are provided as parameters for fitting functions, then flat (improper) priors are put on all parameters. See provided examples and tests to see how one can specify their own priors.

## Analyzing fits

To enable users without extensive programming knowledge to perform professional level Bayesian data analysis we developed a number of custom methods. Below is a short description of functions common to all models, for descriptions of functions specific to certain models consult the package's help pages.

* __`summary`__ prints a summary of the fit.
* __`print`__, __`show`__ prints a more detailed summary of the fit (same as RStan's _print_ function).
* __`plot_fit`__ visualize the quality of the fitted model against the input data.
* __`plot_trace`__ construct a trace plot for relevant parameters of the fitted model.
* __`get_parameters`__ extracts the parameters of the fitted model.
* __`get_subject_parameters`__ get parameters for each subject (useful only for hierarchical models).
* __`plot_means`__ visualize means for a single or multiple fitted models.
* __`compare_means`__ compare means between two or more fitted models.
* __`plot_means_difference`__ visualize the difference of means between two or more fitted models.
* __`plot_distributions`__ visualize distributions underlying fitted models, can be used to visualize one or more fitted models.
* __`compare_distributions`__ draw and compare samples from distributions underlying fitted models, can be used to compare two or more fitted models.
* __`plot_distributions_difference`__ visualize the difference in distributions underlying fitted models, can be used to compare two or more fitted models.

## Resources

* [bayes4psy_tools](https://github.com/bstatcomp/bayes4psy_tools) repository with real life examples that use the package to perform scientific paper grade Bayesian data analysis.
* [Open an issue](https://github.com/bstatcomp/bayes4psy/issues) GitHub issues for bug reports, feature requests.
* [Stan](https://mc-stan.org/) the Stan homepage.

## Development Version

You can install the development version from GitHub. To do so you first have to install the **RStan** package and C++ toolchain ([instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)). Once **RStan** and the toolchain are installed, you can install **bayes4psy** by using the **devtools** package in R:

```r
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("bstatcomp/bayes4psy")
```

## Funding

The research behind this software was partially funded by the Slovenian Research Agency (ARRS) through grants L1-7542 (Advancement of computationally intensive methods for efficient modern general-purpose statistical analysis and inference), P3-0338 (Physiological mechanisms of neurological disorders and diseases), J3-9264 (Decomposing cognition: working memory mechanism and representations), P5-0410 (Digitalization as driving force for sustainability of individuals, organizations, and society), and P5-0110 (Psychological and neuroscientific aspects of cognition).
