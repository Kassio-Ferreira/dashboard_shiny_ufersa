# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("readxl", "tidyr", "dplyr", "readxl", "ggplot2",
                "reshape2", "shiny", "shinydashboard", "DT", "ggthemes",
                "scales", "plotly")


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
