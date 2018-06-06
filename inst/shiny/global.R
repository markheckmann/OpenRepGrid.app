
#### Shiny global variables and helpers ####

library(shiny)
library(rgl)
library(ggplot2)
library(stringr)
library(shinythemes)
library(shinyBS)
library(OpenRepGrid, quietly=TRUE)


#### ----------------------- SETTINGS --------------------####

options(width=150)
options(rgl.useNULL=TRUE) 


#### ----------------------- CONSTANTS -------------------####

colors <- colors()         # all available color names in R
colors.all <- c("black", "darkred", "darkblue", "darkgreen", colors)


#### -------------------- REACTIVE VALUES ----------------####

values <- reactiveValues()
values$e.names <- "No grid selected"
values$c.names <- "No grid selected"
values$current_grid <- NULL
values$e.names <- NA
values$c.names <- NA
values$constructs_clusterboot <- NULL
values$elements_clusterboot <- NULL
 


#### UI variables defined here for convenience ####
# Current gridsampler version (displayed in header)
pkg_version <- 0.1 # paste0("OpenRepGrid.app v", packageVersion("OpenRepGrid.app"))

# # Column descriptions
# desc_col1 <- helpText("")
# desc_col2 <- helpText("")
# desc_col3 <- helpText("")

#### Tooltips ####
# See tooltip options: http://www.w3schools.com/bootstrap/bootstrap_ref_js_tooltip.asp
tooltip_opts <- list(delay = list(show = 1000, hide = 100))



