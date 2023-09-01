install.packages(c('testthat','rmarkdown','spelling','ggplot2','ggh4x','data.table','ggnewscale','checkmate','scales','dplyr','tidyr'), repos = 'http://cran.us.r-project.org', type='win.binary')
download.file('https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-rutils/artifacts/ospsuite.utils.zip?pr=false', destfile = 'ospsuite.utils.zip', mode='wb')
install.packages('ospsuite.utils.zip', repos = NULL, type = 'binary')
unlink('ospsuite.utils.zip')
