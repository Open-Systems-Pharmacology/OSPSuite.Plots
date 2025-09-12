install.packages(c('testthat','rmarkdown','spelling','ggplot2','ggh4x','data.table','ggnewscale','checkmate','scales','dplyr','tidyr'), repos = 'http://cran.us.r-project.org', type='win.binary')
download.file('https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/download/v1.7.0/ospsuite.utils_1.7.0.zip', destfile = 'ospsuite.utils.zip', mode='wb')
install.packages('ospsuite.utils.zip', repos = NULL, type = 'binary')
unlink('ospsuite.utils.zip')
