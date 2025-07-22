# =====================================================
# DASHBOARD ANALISIS SOVI - UJIAN STATISTIKA TERAPAN
# Politeknik Statistika STIS - VERSI LENGKAP & DIPERBAIKI
# =====================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(leaflet)
library(leaflet.extras)
library(viridis)
library(car)
library(nortest)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(zip)
library(moments)
library(e1071)
library(stats)
library(sf)
library(cluster)
library(factoextra)

# Source UI and Server
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
