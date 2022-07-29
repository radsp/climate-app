
# Libraries --------------------------------------------------------------------


install.packages("devtools")
require(devtools)
install_version("bslib", version = "0.3.1", repos = "http://cran.us.r-project.org")



if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, knitr, lubridate, readr, readxl, tidyr, tools, forcats, ggplot2,
               shiny, shinyjs, shinythemes, shinyWidgets, shinyBS, leaflet, shinybusy)


library(civis)
library(bslib)

# Source ------------------------------------------------------------------------

# source("util.R")
source("app_ui.R")
source("app_server.R")


# App ----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)