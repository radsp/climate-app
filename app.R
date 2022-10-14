
# Libraries --------------------------------------------------------------------


# install.packages("devtools")
# require(devtools)
# install_version("bslib", version = "0.3.1", repos = "http://cran.us.r-project.org")

require(devtools)

# 
if (!require("pacman")) install.packages("pacman")

pacman::p_load(plyr, tidyverse, ggplot2, leaflet, plotly, 
               shiny, shinyjs, shinythemes, shinyWidgets, shinyBS, shinybusy,
               shinydashboard, shinycssloaders)
# install.packages("scales")

library(civis)
# library(bslib)
library(scales)

# Source ------------------------------------------------------------------------

# source("util.R")
# source("mod-fns.R")

source("global.R")
source("fun.R")
source("home.R")
source("trackers.R")
source("downloads.R")
source("app_ui.R")
source("app_server.R")



# App ----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
