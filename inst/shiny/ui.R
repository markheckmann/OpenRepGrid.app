
# Shiny UI definition


#### ______________________ ####
#### INTRO TOUR #### 

# using boostrap-tour library (http://bootstraptour.com/) 
#
# Add a tour to GUI to explain the panels and basis steps.
# The tour is defined in www/tour.js

# header <- list(tags$script(src = "bootstrap-tour-0.10.3/js/bootstrap-tour.min.js"),
#                tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
#                tags$script("var a = $('#ourNavbar  a[data-value=\"Tour\"]');
#                                     a.attr('data-toggle', 'noclass');
#                                     a.click(function() {
#                                       tour.restart();
#                                       console.log('Tour started');
#                                     });") )
# 
# footer <- tags$script(src = "tour.js")  # add tour




#### ______________________ ####
#### SIDEBAR PANELS ####


#### __ Settings ####


cond.panel.settings <- 
  conditionalPanel(condition="input.level1=='load_grid'",
                   h3("Select a grid"),
                   HTML("<hr>"),
                   h4("Upload a grid file"),
                   uiOutput("upload_dialog"),
                   HTML("Either upload your own grid file or select a sample grid below. 
                        Your grid file must come in 
                        <a href='http://docu.openrepgrid.org/loading.html#txt-files' 
                        target='_blank'>this format</a>."),
                   HTML("<hr>"),
                   h4("Select a sample grid"),
                   selectInput("samplegrid", "", 
                               choices= c("Select a grid"="none", 
                                        c("Boeker (1996)"="boeker", 
                                          "Fransella et al. (2003)"="fbb2003",
                                          "Mackay (1992)"="mackay1992") ),
                               selectize = FALSE),
                   uiOutput("samplegrid_info")
  )


#### __ Bertin ####

cond.panel.bertin <- 
  conditionalPanel(condition="input.level1=='bertin_standard'",
                   h3("Bertin Settings", id="bertin_settings"),
                   tags$div(checkboxInput("bertin_standard_showvalues", "Show values", value = TRUE),
                            id="bertin_standard_showvalues_wrapper_div", style="display:inline-block;"),
                   numericInput("bertin_standard_cex_all", "Textsize", 1.1, min = .1, max = 2,
                                step = .05),
                   span(numericInput("bertin_standard_xlim_1", "Space constructs left", .2, min = 0, max = 1,
                                     step = .02),
                        numericInput("bertin_standard_xlim_2", "Space constructs right", .2, min = 0, max = 1,
                                     step = .02)),
                   numericInput("bertin_standard_ylim", "Size element region", .6, min = 0, max = 1,
                                step = .05),
                   span(selectInput("bertin_standard_color_left", "Color left pole", 
                                    choices=colors.all, selected="white", selectize=FALSE),
                        selectInput("bertin_standard_color_right", "Color right pole", 
                                    choices=colors.all, selected="darkblue", selectize=FALSE)
                   )        
  )




#### ______________________ ####
#### MAIN PANELS ####


#### __ Settings ####

level1.panel.settings <- 
  tabPanel(title = "Load grid", 
           # complete ui generated on server
           uiOutput("load_grid"), 
           value="load_grid")

  
#### __ Bertin ####

level1.panel.bertin <- 
  tabPanel("Bertin", 
           # collapsable info box 
           # requires js.js and styles.css from /www to be read in.
           # done in main panel 
           htmlOutput("bertin_info"),  
           # bertin plot 
           plotOutput("bertin", width="600px", height="600px"),  
           value="bertin_standard")




#### ______________________ ####
#### IU DEFINITION ####


shinyUI(pageWithSidebar(
  
  headerPanel("OpenRepGrid.app"),
  
  
  #### __ Sidebar panel ####
  
  sidebarPanel( 
    
    ## set of conditional panels ##
    cond.panel.settings,
    cond.panel.bertin,
    
    
    ## toggle tooltips button ##
    HTML("<hr>"),
    tags$div(checkboxInput(inputId = "chk_toggle_tooltips", 
                           label = "Info for controls"), 
             id="chk_toggle_tooltip_wrapper_div", 
             style="float: left;"),
    HTML("<br>")
  ),
  
  
  #### __ Main panel ####
  
  mainPanel(
  
    # additional js scripts in folder /www
    # (that's where shiny looks for static files)
    tags$head(
      tags$script(src="js.js", type="text/javascript"),            # Custom JS script for collapsing
      tags$link(href="style.css", rel="stylesheet")        
      #tags$script(src="js/tooltips_defs.js", type="text/javascript")
    ),
    
    h3( textOutput("caption") ),
    tabsetPanel(
        level1.panel.settings,
        level1.panel.bertin,
        id="level1"
    )
  )
  
))


