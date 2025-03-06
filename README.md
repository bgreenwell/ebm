# ebm

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ebm)](https://CRAN.R-project.org/package=ebm)
[![R-CMD-check](https://github.com/bgreenwell/ebm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bgreenwell/ebm/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A [reticulate](https://rstudio.github.io/reticulate/)-powered interface to the Python [InterpretML](https://interpret.ml/) framework for fitting [explainable boosting machines](https://doi.org/10.48550/arXiv.1909.09223) (EBMs). EBMs are a modern type of generalized additive model that use tree-based, cyclic gradient boosting with automatic interaction detection. They are often as accurate as state-of-the-art blackbox models while remaining completely interpretable.


## Installation

You can install the **ebm** package from either CRAN (stable) or GitHub (development):
```r
# Install the latest stable version from CRAN:
install.packages("ebm")

# Install the latest development version from GitHub:
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("bgreenwell/ebm")
```

# Usage

For a thorough overview of using the **ebm** package, see [this article](https://bgreenwell.github.io/ebm/articles/RJwrapper.pdf).
