
package_list = c('knitr',
                 'readr','feather',
                 'MASS','zoo',
                 'ggplot2','ggthemes','mapdata','gridExtra','quantreg','hexbin',
                 'kableExtra',
                 'cowplot', 'ggpubr',
                 'magick',
                 'rworldmap','ggmap',
                 'scales','reshape',
                 'chron','lubridate','timeDate',
                 'plyr','dplyr',
                 'parallel','doParallel',
                 'tictoc',
                 'styler',
                 'bookdown',
                 'biwavelet', 'forecast',
                 'tidyverse','magrittr'
                 #'HMM',
                 # 'plotly',
                 #'klaR', 'cluster',
                 #'Rcpp',
                 #'e1071', 'parallelSVM', 'randomForest',
)

pckgs = installed.packages()

for(package in package_list){
  cat(package,"\n")
  pckgs = installed.packages()
  need.to.install = (!(package %in% pckgs[,1]))
  if(need.to.install){cat('installing package ',package,'\n');install.packages(package,dependencies = TRUE)}
  library(package = package, character.only = TRUE)
}

rm(package_list, pckgs,need.to.install,package)


