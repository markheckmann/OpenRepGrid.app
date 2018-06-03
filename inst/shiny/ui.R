
#### Shiny UI definition ####

#### Intro Tour using boostrap-tour library (http://bootstraptour.com/)  ####

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

#### End Intro Tour components, begin shinyUI  ####




#### CONDITIONAL PANELS ####


#### |-- Settings ####


cond.panel.settings <- 
  conditionalPanel(condition="input.level1=='load_grid'",
                   h3("Select a grid"),
                   HTML("<hr>"),
                   h4("Upload a grid file"),
                   uiOutput("upload_dialog"),
                   HTML("To get started you can either select a sample grid
                        from the literature below or upload your own grid file.
                        Your grid file has to be in 
                        <a href='http://docu.openrepgrid.org/loading.html#txt-files' 
                        target='_blank'>this format</a> to be read in."),
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



#### UI DEFINITION ####


shinyUI(pageWithSidebar(
  
  headerPanel("OpenRepGrid.app"),
  
  
  #### |-- Sidebar panel ####
  
  sidebarPanel( 
    
    ## Settings ##
    cond.panel.settings,
    
    ## toggle tooltips ##
    HTML("<hr>"),
    tags$div(checkboxInput(inputId = "chk_toggle_tooltips", 
                           label = "Info for controls"), 
             id="chk_toggle_tooltip_wrapper_div", 
             style="float: left;"),
    HTML("<br>")
  ),
  
  
  #### |-- Main panel ####
  
  mainPanel(
  
    h3(textOutput("caption")),
    tabsetPanel(
      tabPanel(title = "Load grid", 
               uiOutput("load_grid"), 
               value="load_grid"),
      id="level1")
  )
))


