
# ospsuite.plots: Graphics and tables for graphics for OSP

<!-- badges: start -->

  [![Latest release download count badge](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.Plots/latest/total?label=%E2%AD%B3%20Downloads%20latest%20release)](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/releases/latest)
  [![Total downloads count badge](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.Plots/total?label=%E2%AD%B3%20Downloads%20total)](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/releases)

  [![build](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.Plots/main-workflow.yaml?logo=github&logoColor=white&label=Build)](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/actions/workflows/main-workflow.yaml)
  [![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.Plots/branch/main/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.Plots)
  [![Lint Test](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.Plots/lint.yaml?logo=githubactions&logoColor=white&label=lint)](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/actions/workflows/lint.yaml)

<!-- badges: end -->


The `{ospsuite.plots}` package provides a framework to create
figures and dependent tables which are used by R packages in the Open Systems
Pharmacology ecosystem:

-   [`{ospsuite}`](https://www.open-systems-pharmacology.org/OSPSuite-R/)
-   [`{ospsuite.reportingframework}`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/)

This is the beta release of the  `{ospsuite.plots}`. We welcome your feedback as we refine its features and performance.

## Installation

`{ospsuite.plots}` and its Open Systems Pharmacology dependencies are published
on the [OSP R-universe](https://open-systems-pharmacology.r-universe.dev).
Installing the released version needs nothing but base R, and resolves
`{ospsuite.utils}` for you:

``` r
install.packages(
  "ospsuite.plots",
  repos = c(OSP = "https://open-systems-pharmacology.r-universe.dev", getOption("repos"))
)
```

To install the development version from GitHub instead, use
[pak](https://pak.r-lib.org):

``` r
# install.packages("pak")
pak::pak("Open-Systems-Pharmacology/OSPSuite.Plots")
```

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases,
issue trackers, chat rooms, mailing lists etc.) is expected to follow the Open
Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing code, please be familiar with the [coding
standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

OSPSuite.Plots Library is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
