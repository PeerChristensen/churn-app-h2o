# # code to install packages in the docker file
# 
# # #The following two commands remove any previously installed H2O packages for R.
#  if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# #
#  # Next, we download packages that H2O depends on.
#  pkgs <- c("RCurl","jsonlite")
#  for (pkg in pkgs) {
#    if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
#  }
# 
# # Now we download, install and initialize the H2O package for R.
# #install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zahradnik/1/R")
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zermelo/3/R")

install.packages('shiny', repos='http://cran.rstudio.com/')
install.packages('shinydashboard', repos='http://cran.rstudio.com/')
install.packages('dashboardthemes', repos='http://cran.rstudio.com/')
install.packages("devtools")
devtools::install_github('andrewsali/shinycssloaders')
install.packages('shinyWidgets', repos='http://cran.rstudio.com/')
install.packages('yardstick', repos='http://cran.rstudio.com/')
install.packages('DT', repos='http://cran.rstudio.com/')
install.packages('gsubfn', repos='http://cran.rstudio.com/')
