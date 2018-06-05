
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


#### __ Biplot ####


#### ____ 2D  ####

# choices in left panel
choices.center <- c("No centering"=0,
                    "Row mean centering (construct)"=1,
                    "Column mean centering (elements)"=2,
                    "Double-centering (construct and element means)"=3,
                    "Midpoint centering of rows (constructs)"=4)
choices.normalize <- c(none=0, rows=1, columns=2)
biplot.element.selector.12 <- checkboxGroupInput("biplot_element_selector_12", 
                                                 "", isolate(values$e.names),
                                                 selected = isolate(values$e.names))
biplot.construct.selector.12 <- checkboxGroupInput("biplot_construct_selector_12", 
                                                   "", isolate(values$c.names),
                                                   selected = isolate(values$c.names))


# build left panel
cond.panel.biplots.dim.12 <-
  conditionalPanel(condition="input.level1=='level1_biplot_standard' && input.level2_biplots=='level2_biplots_dim_12'",
                   h3("Biplots Settings", id="biplot_settings"),
                   tabsetPanel(
                     #tabPanel("Info", get_html_docu("biplot2d")),
                     tabPanel("Elements", 
                              actionButton("biplot_12_toggle_elements", "Toggle On/off"),
                              actionButton("biplot_12_update_button_elements", "Update Biplot"),
                              HTML("<hr>"),
                              biplot.element.selector.12
                     ),
                     tabPanel("Constructs", 
                              actionButton("biplot_12_toggle_constructs", "Toggle On/off"),
                              actionButton("biplot_12_update_button_constructs", "Update Biplot"),
                              HTML("<hr>"),
                              biplot.construct.selector.12
                     ),
                     tabPanel("Transforms",
                              h5("Transformations"),
                              selectInput("biplot_12_center", "Centering", 
                                          choices=choices.center, selected=choices.center[2],
                                          selectize=FALSE),
                              selectInput("biplot_12_normalize", "Normalize", 
                                          choices=choices.normalize, selected=choices.normalize[1],
                                          selectize=FALSE),
                              numericInput("biplot_12_g", "g", 0, 0, 1, .1)
                     ),
                     tabPanel("Axes",
                              HTML("<hr>"), 
                              h5("Axes"),      
                              numericInput("biplot_12_dim_1", "PC on y-axis", 1, min = 1, max = 100,
                                           step = 1),
                              numericInput("biplot_12_dim_2", "PC on x-axis", 2, min = 1, max = 100,
                                           step = 1),
                              p("Flip axis"),
                              div(
                                checkboxInput("biplot_12_flipaxes_1", "x-axis", FALSE),
                                checkboxInput("biplot_12_flipaxes_2", "y-axis", FALSE))
                     ),
                     tabPanel("Size",
                              h5("Label size"),
                              numericInput("biplot_12_c_label_cex", "Constructs", 1.2, 0, 2, .1),
                              numericInput("biplot_12_e_label_cex", "Elements", 1.2, 0, 2, .1),
                              h5("Symbol size"),       
                              numericInput("biplot_12_c_point_cex", "Constructs", 1.2, 0, 2, .1),      
                              numericInput("biplot_12_e_point_cex", "Elements", 1.2, 0, 2, .1),
                              h5("Plot size in pixel"),       
                              sliderInput("biplot_12_plotsize", "", 600, min=300, max=1200, step=100)            
                     ),
                     tabPanel("Margins",          # in pixel, need to be converted to inches
                              h5("Margins for construct labels"),
                              sliderInput("biplot_12_mai_bottom", "Bottom", 100, min=0, max=300, step=25) ,
                              sliderInput("biplot_12_mai_left", "Left", 100, min=0, max=300, step=25),            
                              sliderInput("biplot_12_mai_top", "Top", 100, min=0, max=300, step=25),            
                              sliderInput("biplot_12_mai_right", "Right", 100, min=0, max=300, step=25)            
                     ),
                     tabPanel("Colors",
                              h5("Element colors"),
                              selectInput("biplot_12_e_point_col", "Symbol", colors.all, "black", selectize=FALSE),
                              selectInput("biplot_12_e_label_col", "Label", colors.all, "black", selectize=FALSE),
                              h5("Constructs colors"),
                              selectInput("biplot_12_c_point_col", "Symbol", colors.all, "black", selectize=FALSE),
                              selectInput("biplot_12_c_label_col", "Label", colors.all, "black", selectize=FALSE) 
                     ),
                     id="todo")               
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
           br(), br(), br(),  # vertical space under bertin figure
           value="bertin_standard")



#### __ Biplot ####

#### ____ 2D  ####

level1.panel.biplots <- 
  tabPanel("Biplots", 
           tabsetPanel(
             tabPanel("2D", 
                      htmlOutput("biplot_info"),
                      plotOutput("biplot2d_12", width="auto", height="auto"), 
                      value="level2_biplots_dim_12"),
             # tabPanel("3D", 
             #          reactiveWebGL(outputId = "webGL"), 
             #          value="level2_biplots_3d"),
             # tabPanel("3D2", 
             #          rgl::rglwidgetOutput("rglPlot"),
             #          value="level2_biplots_3d2"),
             id="level2_biplots"),
           value="level1_biplot_standard")



#### ______________________ ####
#### IU DEFINITION ####


shinyUI(pageWithSidebar(
  
  headerPanel("OpenRepGrid.app"),
  
  
  #### __ Sidebar panel ####
  
  sidebarPanel( 
    
    ## Settings ##
    cond.panel.settings,
    ## Bertin ##
    cond.panel.bertin,
    ## Biplots ##
    cond.panel.biplots.dim.12,
    
    
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
      tags$script(src="js.js", type="text/javascript"),  # Custom JS script for collapsing
      tags$link(href="style.css", rel="stylesheet")        
      #tags$script(src="js/tooltips_defs.js", type="text/javascript")
    ),
    
    h3( textOutput("caption") ),
    tabsetPanel(
        level1.panel.settings,
        level1.panel.bertin,
        level1.panel.biplots,
        id="level1"
    )
  )
  
))


