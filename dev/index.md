# ospsuite.plots: Graphics and tables for graphics for OSP

The
[ospsuite.plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/)
package provides a framework to create figures and dependent tables
which are used by R packages in the Open Systems Pharmacology ecosystem:

- [`{ospsuite}`](https://www.open-systems-pharmacology.org/OSPSuite-R/)
- [`{ospsuite.reportingframework}`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/)

This is the beta release of the
[ospsuite.plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/).
We welcome your feedback as we refine its features and performance.

## Installation

Install the released version from the [OSP
R-universe](https://open-systems-pharmacology.r-universe.dev):

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
options(repos = c(
  OSP = "https://open-systems-pharmacology.r-universe.dev",
  getOption("repos")
))

pak::pak("Open-Systems-Pharmacology/OSPSuite.Plots")
```

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc.) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing code, please be familiar with the [coding
standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

OSPSuite.Plots Library is released under the [GPLv2
License](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/LICENSE).

All trademarks within this document belong to their legitimate owners.
