
source("utils.R")
source("global.R")
source("samplegrid-infotext.R")    # info texts for sample grids


## Shiny server file 
shinyServer(function(input, output, session) {

  
  get_file <- reactive({
    values$current_grid
  })
  
  #### ______________________ ####
  #### OBSERVERS #### 
  

  # extract grid passed via get variable during upload
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$grid)) {
      str <- stringr::str_replace_all(query$grid, "\\\\n", "\n")   #replace "\\n" by "\n"
      f <- paste0(tempfile(), ".txt")
      writeLines(str, f)
      values$current_grid <- importTxt(f)
    }
  })
  
  
  # upload a grid file
  observe({
    infile <- input$gridfile      
    if (!is.null(infile)) {
      values$current_grid <- importTxt(infile$datapath)     # update current grid
      
      # set back samplegrid value and display when grid is uploaded
      updateSelectInput(session, inputId="samplegrid", choices=NULL)   
    }
  })
  
  
  # select a sample grid
  observe({
    samplegrid <- input$samplegrid
    if (samplegrid != "none") 
      values$current_grid <- eval(parse(text=samplegrid))   # update current grid
  })
  
  
  
  # current grid gets changed
  observe({
    g <- values$current_grid
    if (!is.null(g)) {
      values$e.names <- getElementNames(g)
      values$c.names <- getConstructNames2(g) 
    #  values$constructs_clusterboot <- NULL    # reset clusterboot results
    }
    # update all element and construct selectors
    updateCheckboxGroupInput(session, "biplot_element_selector_12", 
                             label="",  choices=values$e.names, 
                             selected=values$e.names)
    updateCheckboxGroupInput(session, "biplot_construct_selector_12", 
                             label="",  choices=values$c.names, 
                             selected=values$c.names) 
    # updateSelectInput(session, "indexes_implicative_dilemma_ideal", 
    #                   label="Ideal element",  choices=values$e.names, 
    #                   selected=values$e.names[1]) 
    # updateSelectInput(session, "indexes_implicative_dilemma_self", 
    #                   label="Self element",  choices=values$e.names, 
    #                   selected=values$e.names[2]) 
  })
  
  # update element selection
  observe({
    input$biplot_12_toggle_elements   # make reactive
    e.names <- isolate(values$e.names)
    if (is.null(isolate(input$biplot_element_selector_12)))
      select <- e.names
    else
      select <- NULL
    updateCheckboxGroupInput(session, "biplot_element_selector_12", label="", 
                             choices=e.names, selected=select)
  }, suspended=FALSE)
  
  observe({
    input$biplot_12_toggle_constructs   # make reactive
    c.names <- isolate(values$c.names)
    if (is.null(isolate(input$biplot_construct_selector_12)))
      select <- c.names
    else
      select <- NULL
    updateCheckboxGroupInput(session, "biplot_construct_selector_12", label="", 
                             choices=c.names, selected=select)
  }, suspended=FALSE)
  
  
  
  
  
  #### ______________________ ####
  #### RENDERERS #### 
  
  
  
  #### __ Load grids ####
  
  # text in main panel on grid loading page
  output$load_grid <- renderUI({
    list(h3("Welcome to OpenRepGrid.app"),
         HTML("<p><i>OpenRepGrid.app</i> is an 
              online grid analysis software. It allows to carry out several kinds of
              analysis for repertory grid data. The software is based on the 
              <a href='http://docu.openrepgrid.org' target='_blank'>OpenRepGrid</a> 
              R package which runs in the background to produce the output.</p>"),
         HTML("<p>In the top panel you will find the available (analysis) features. 
              Once you select a feature you will see the available settings for 
              the feature in the panel on the left.</p>"),
         HTML("<hr><p><font color='red'><b>Caveat:</b></font> Not all browser support all application features properly. 
              The Google Chrome browser seems to work in most cases.
              </p>")
    )
  })  
  
  
  ## rendered UI elemenst for conditional panels
  
  # info text for sample grid depends on selected grid
  output$samplegrid_info <- renderUI({ 
    HTML(samplegrid_info[input$samplegrid])
  })
  
  # upload dialog sample grid
  output$upload_dialog <- renderUI({ 
    fileInput("gridfile", "",
              accept=c("text", "text/plain"))
  })
  
  
  
  #### __ Bertin ####
  
  # info text on top aboce bertin plot
  output$bertin_info <- renderUI({
    HTML( inject_info_on_top_of_ui_pages("bertin", "www/info/bertin.html") )  
  })
  
  # produce bertin plot
  output$bertin <- renderPlot({
    cex <- 1.1
    x <- get_file()
    if (!is.null(x))
      bertin(x, 
             cex.elements=input$bertin_standard_cex_all,    # font size elements 
             cex.text=input$bertin_standard_cex_all,        # font size cells
             cex.constructs=input$bertin_standard_cex_all,  # font size constructs
             colors=c(input$bertin_standard_color_left,     # colors of left and right pole
                      input$bertin_standard_color_right),
             showvalues=input$bertin_standard_showvalues,   # show ratings?
             xlim=c(input$bertin_standard_xlim_1,           # left and right margins
                    1 - input$bertin_standard_xlim_2),
             ylim = c(0, input$bertin_standard_ylim))       # top margin for elements
  })
  

  
  #### __ Biplot ####
  
  #### ____ 2D  ####
  
  
  # add infor text at top over biplot
  output$biplot_info <- renderUI({
    HTML( inject_info_on_top_of_ui_pages("biplot", "www/info/biplot.html"))  
  })
  
  
  # produce biplot
  output$biplot2d_12 <- renderPlot(
  {
    input$biplot_12_update_button_elements      # trigger rendering by button
    input$biplot_12_update_button_constructs    # trigger rendering by button
    x <- get_file() 
    e.sel <- isolate(input$biplot_element_selector_12)   # get selected element names
    e.names <-isolate(values$e.names)           # get elements names
    e.i <- which(e.names %in% e.sel)            # get indexes of selected elements
    
    c.sel <- isolate(input$biplot_construct_selector_12)  # get selected construct names
    c.names <-isolate(values$c.names)           # get construct labels
    c.i <- which(c.names %in% c.sel)            # get indexes of selected construct
    
    dim <- c(input$biplot_12_dim_1,             # dimensions of bipot to display
             input$biplot_12_dim_2)  
    flipaxes <- c(input$biplot_12_flipaxes_1,   # flip x or y axis?
                  input$biplot_12_flipaxes_2) 
    # catch case when selected dimensions are the same
    # which does not make a proper plot
    if (dim[1] == dim[2]) {
      plot.new()
      text(.5, .5, "dimensions can not be identical", cex=1.2)
    } else if (!is.null(x)) {
    # produce 2D biplot
      biplot2d(x, 
               g=input$biplot_12_g, 
               dim=dim, 
               center=input$biplot_12_center, 
               normalize=input$biplot_12_normalize,
               flipaxes=flipaxes, 
               e.points.show=e.i, 
               e.labels.show=e.i,
               c.labels.show=c.i, 
               c.points.show =c.i,
               e.label.cex=input$biplot_12_e_label_cex, 
               c.label.cex=input$biplot_12_c_label_cex,
               e.point.cex=input$biplot_12_e_point_cex,
               c.point.cex=input$biplot_12_c_point_cex,
               e.point.col=input$biplot_12_e_point_col,
               e.label.col=input$biplot_12_e_label_col,
               c.point.col=input$biplot_12_c_point_col,
               c.label.col=input$biplot_12_c_label_col,
               mai=c(input$biplot_12_mai_bottom,
                     input$biplot_12_mai_left,
                     input$biplot_12_mai_top,
                     input$biplot_12_mai_right) / 72,
               var.cex=1)
    }
  }, 
  width=function() input$biplot_12_plotsize,   # width and height can be changed interactively
  height=function() input$biplot_12_plotsize)
  
  
  #### ____ 3D ####

  # render 3D rgl widget
  output$biplot_3d <- rgl::renderRglwidget(
  {
    options(rgl.usuNULL = TRUE)
    save <- options(rgl.inShiny = TRUE)
    on.exit(options(save))
    
    #values$trigger_3d
    input$biplot3d_size   # trigger rendering on size change

    x <- get_file()
    if (!is.null(x)) 
    {
      try(rgl.close())
      biplot3d(x, c.text.col = "#0000ff")  # TODO: strange colors
      view3d(theta = 0, phi = 0, zoom = .8)
      scene <- rgl::scene3d()
      rgl::rglwidget(scene)
    }
  })
  
  
  # render UI in server using several inputs
  # TODO: add button to update plot by user like in 2d plot
  #
  output$bp3d <- renderUI(
  {
   # values$trigger_3d <- values$trigger_3d + 1
    size <- input$biplot3d_size  # trgger size change
    size_px <- paste0(size,"px")
    rgl::rglwidgetOutput("biplot_3d",
                         width = size_px, #"600px",
                         height = size_px) #"600px")
  })
  

  
  #### __ Constructs ####
  
  
  #### ____ Correlation ####
  
  # simple correlations
  output$construct_correlation <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      constructCor(x, 
                   method = input$constructs_correlation_method,   # pearons, spearman, kendall
                   trim = input$constructs_correlation_trim)    
    }    
  })
  
  
  # RMS correlation
  output$construct_correlation_rms <- renderPrint({
    x <- get_file()
    if (!is.null(x) & input$constructs_correlation_rms) {
      constructRmsCor(x,  
                      method = input$constructs_correlation_method, # pearons, spearman, kendall
                      trim = input$constructs_correlation_trim)      
    }    
  })
  
  
})


