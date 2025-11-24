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

You can install the development version of
[ospsuite.plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/)
from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/ospsuite.plots")
```

[ospsuite.plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/)
requires following packages to be installed:

From CRAN:

- [ggplot2](https://cran.r-project.org/package=ggplot2/index.html)
- [ggh4x](https://cran.r-project.org/package=ggh4x/index.html)
- [cowplot](https://cran.r-project.org/package=cowplot/index.html)
- [data.table](https://cran.r-project.org/package=data.table/index.html)
- [ggnewscale](https://cran.r-project.org/package=ggnewscale/index.html)
- [checkmate](https://cran.r-project.org/package=checkmate/index.html)
- [dplyr](https://cran.r-project.org/package=checkmate/index.html)
- [magrittr](https://cran.r-project.org/package=magrittr/index.html)
- [fitdistrplus](https://cran.r-project.org/package=magrittr/index.html)
- [tidyr](https://cran.r-project.org/package=tidyr/index.html)
- [fs](https://cran.r-project.org/package=fs/index.html)

Must be downloaded manually:

- [ospsuite.utils](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/download/v1.3.17/ospsuite.utils_1.3.17.zip)

To install manually, follow these instructions:

``` r
# Install `{ospsuite.utils}` from local file 
# (`pathTo_ospsuite.utils.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_ospsuite.utils.zip, repos = NULL)


# Install dependencies (e.g. ggplot2) which are on CRAN
install.packages('ggplot2')
install.packages('ggh4x')
install.packages('data.table')
install.packages('ggnewscale')
install.packages('checkmate')
install.packages('dplyr')
install.packages('magrittr')
install.packages('fitdistrplus')
install.packages('tidyr')
install.packages('fs')
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
