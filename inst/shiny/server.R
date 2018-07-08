
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
      values$e.names <- elements(g)
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
    updateSelectInput(session, "indexes_implicative_dilemma_ideal",
                      label="Ideal element",  choices=values$e.names,
                      selected=values$e.names[1])
    updateSelectInput(session, "indexes_implicative_dilemma_self",
                      label="Self element",  choices=values$e.names,
                      selected=values$e.names[2])
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
  
  
  
  #### __ Start  ####
  
  # text in main panel on grid loading page
  output$start <- renderUI({
    list(br(),
         h3("About OpenRepGrid.app"),
         HTML("<p><i>OpenRepGrid.app</i> is an
              online grid analysis software. It allows to carry out several kinds of
              analysis for repertory grid data. The software is based on the
              <a href='http://docu.openrepgrid.org' target='_blank'>OpenRepGrid</a>
              R package which runs in the background to produce the output.</p>"),
         HTML("<p>In the top panel you find the available (analysis) feature tabs.
              When you select a tab you will see the available settings for
              the feature or analysis in the panel on the left.</p>"),
         br(),
         h3("Technical Requirements"),
         HTML("Not all browser support all application features properly.
               The Google Chrome, Microsoft Edge and Firefox browsers seem to work in most cases.
               </p>"),
         br(),
         h3("Tips to get started"),
         HTML("<p>On several tabs you will find a small 'info' button in the upper right corner. 
               Click it to see additional infotmation on a feature.</p>"),
         br(),
         h3("Citation"),
         HTML("<p>If you use the software, please cite it in your publication as follows:</p>
               <p>Heckmann, M. (2014). <i>OpenRepGrid.app: A web based frontend to the OpenRepGrid R 
               package for the analysis of repertory grids.</i>
               ZENODO. doi:10.5281/zenodo.8492
              </p>")
    )
  })
  
  
  ## rendered UI elements for conditional panels
  
  # info text for sample grid depends on selected grid
  output$samplegrid_info <- renderUI({ 
    HTML(samplegrid_info[input$samplegrid])
  })
  
  # upload dialog sample grid
  output$upload_dialog <- renderUI({ 
    fileInput("gridfile", "",
              accept=c("text", "text/plain"))
  })
  
  
  
  #### __ Settings ####
  

  #### ____ Modifiable table ####
  
  # http://stla.github.io/stlapblog/posts/shiny_editTable.html
  # update handsometable when grid changes 
  #
  output$hot <- renderRHandsontable(
  {
    req(values$current_grid)         
    cat("\nUpdate handomsetable")
    g_df <- get_grid_dataframe(values$current_grid)
    colnames(g_df) <- str_wrap(colnames(g_df), width = 10)  # break element names to fit in column
    values$current_grid_df <- g_df
    
    if (!is.null(values$current_grid_df))
      rhandsontable(isolate(values$current_grid_df), stretchH = "all", useTypes = F)
  })

  # when values in handsometable change, update current_grid_df object
  # and in turn the current_grid object
  observe({
    if (!is.null(input$hot)) {
      df = hot_to_r(input$hot)    # get input table data
      cat("\nGet handsometable data and update grid object")
      # values$current_grid_df <- DF
      update_current_grid_from_df(df)
    }
  })
  
  
  
  #### __ Bertin ####
  
  
  ## highlight rows and constructs ##
  #
  # there are arrow buttons to steer the row and column
  # which gets highlighted

  # rows = constructs
  observeEvent(input$btn_up, {
    values$cr <- ifelse(values$cr <= 1, 1, values$cr - 1)
  })
  
  observeEvent(input$btn_down, {
    values$cr <- ifelse(values$cr >= nrow(values$current_grid), 
                        nrow(values$current_grid), 
                        values$cr + 1)
  })
  
  
  # columns = elements
  observeEvent(input$btn_left, {
    values$cc <- ifelse(values$cc <= 1, 1, values$cc - 1)
    print(values$cc)
  })
  
  observeEvent(input$btn_right, {
    values$cc <- ifelse(values$cc >= ncol(values$current_grid), 
                        ncol(values$current_grid), 
                        values$cc + 1)
    print(values$cc)
  })

    
  ## grid actions (move, delete, swap) ##

  # move construct upwards
  observeEvent(input$btn_move_up, {
    req(values$cr > 1 & values$cr <= nrow(values$current_grid))
    cr <- isolate(values$cr)
    grid <- isolate(values$current_grid)
    print("Move up")
    values$cr <- cr - 1
    values$current_grid <- up(grid, cr)
  })
  
  
  # move construct downwards
  observeEvent(input$btn_move_down, {
    req(values$cr > 0 & values$cr < nrow(values$current_grid))
    cr <- isolate(values$cr)
    grid <- isolate(values$current_grid)
    values$cr <- cr + 1
    values$current_grid <- down(grid, cr)
  })
  
  
  # move element left
  observeEvent(input$btn_move_left, {
    req(values$cc > 1 & values$cc <= ncol(values$current_grid))
    cc <- isolate(values$cc)
    grid <- isolate(values$current_grid)
    values$cc <- cc - 1
    values$current_grid <- left(grid, cc)
  })
  
  
  # move element right
  observeEvent(input$btn_move_right, {
    req(values$cc > 0 & values$cc < ncol(values$current_grid))
    cc <- isolate(values$cc)
    grid <- isolate(values$current_grid)
    values$cc <- cc + 1
    values$current_grid <- right(grid, cc)
  })
  
  # swap poles
  observeEvent(input$btn_swap_poles, {
    req(values$cr > 0 & values$cr <= nrow(values$current_grid))
    cr <- isolate(values$cr)
    grid <- isolate(values$current_grid)
    values$current_grid <- swapPoles(grid, cr)
  })
  
  
  # delete construct
  observeEvent(input$btn_delete_construct, {
    grid <- isolate(values$current_grid)
    nr <- nrow(grid)
    req(values$cr >= 1 & values$cr <= nr)
    cr <- isolate(values$cr)
    values$cr <- ifelse(cr == nr, cr - 1, cr)
    values$current_grid <- grid[-cr, ]
  })
  
  
  # delete element
  observeEvent(input$btn_delete_element, {
    grid <- isolate(values$current_grid)
    ne <- ncol(grid)
    req(values$cc >= 1 & values$cc <= ne)
    cc <- isolate(values$cc)
    values$cc <- ifelse(cc == ne, cc - 1, cc)
    values$current_grid <- grid[ , -cc]
  })
  
  
  # info text on top above start screen
  output$start_info <- renderUI({
    HTML( inject_info_on_top_of_ui_pages("bertin", "www/info/start.html") )  
  })
  
  
  # # info text on top aboce bertin plot
  # output$bertin_info <- renderUI({
  #   HTML( inject_info_on_top_of_ui_pages("bertin", "www/info/bertin.html") )  
  # })

  
  # info text on top aboce bertin plot
  output$bertin_info <- renderUI({
    HTML( inject_info_on_top_of_ui_pages("bertin", "www/info/bertin.html") )
  })
  
  
  # produce bertin plot
  output$bertin <- renderPlot({
    cex <- 1.1
    x <- get_file()
    
    # check if bertin plot is shown with row/column highlighting
    if (input$tabets_bertin == "panel_bertin_modify") {
      cc = values$cc
      cr = values$cr
    } else {
      cc = 0
      cr = 0
    }
    
    # draw bertin if grid object exists
    if (!is.null(x)) {
      bertin(x, 
             cex.elements=input$bertin_standard_cex_all,    # font size elements 
             cex.text=input$bertin_standard_cex_all,        # font size cells
             cex.constructs=input$bertin_standard_cex_all,  # font size constructs
             colors=c(input$bertin_standard_color_left,     # colors of left and right pole
                      input$bertin_standard_color_right),
             showvalues=input$bertin_standard_showvalues,   # show ratings?
             xlim=c(input$bertin_standard_xlim_1,           # left and right margins
                    1 - input$bertin_standard_xlim_2),
             ylim = c(0, 1- input$bertin_standard_ylim),    # top margin for elements 1 = all space, 0 = no space
             cc = cc,   # highlight column index
             cr = cr)   # highlight row index
    }
  })
  

  
  #### __ Biplot ####
  
  
  #### ____ 2D  ####
  
  
  # element selection checkboxes
  output$biplot.element.selector.12 <- renderUI({
    checkboxGroupInput("biplot_element_selector_12", 
                       "", isolate(values$e.names),
                       selected = isolate(values$e.names))
  })
   
   
  # construct selection checkboxes
  output$biplot.construct.selector.12 <- renderUI({
     checkboxGroupInput("biplot_construct_selector_12", 
                         "", isolate(values$c.names),
                         selected = isolate(values$c.names))
  })
  
  
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
               #g=input$biplot_12_g, 
               g = 0,
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
      biplot3d(x, 
               c.text.col = "#0000ff", 
               c.cex = .7,
               e.cex = .7)  
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
  
  
  #### ____ Distance ####
  
  output$construct_distance <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      d <- distance(x, along=1,
                    dmethod=input$constructs_distance_dmethod, 
                    trim=input$constructs_distance_trim)    
      print(d, digits=input$constructs_distance_digits)
    }     
  })
  
  
  #### ____ Cluster ####
  
  output$construct_cluster <- renderPlot({
    x <- get_file()
    if (!is.null(x))
      cluster(x, 1, 
              dmethod=input$constructs_cluster_dmethod,  # distance measure
              cmethod=input$constructs_cluster_cmethod,  # cluster method
              type = input$constructs_cluster_type, 
              align=input$constructs_cluster_align,
              lab.cex=1.2, 
              mar=c(4, 2, 3, 20), 
              cex.main=1.2)
  })
  
  
  #### ____ Cluster Boot ####
  
  # wird einmal automatisch aufgerufen, bei initialisierung
  observe({
    i <- input$constructs_clusterboot_update_button
    #cat("constructs_clusterboot_update_button:", i)
    x <- isolate(get_file())
    s <- NULL
    if (!is.null(x) & i > 0)
      s <- clusterBoot(x, along=1,
                       align=input$constructs_clusterboot_align,
                       nboot=isolate(input$constructs_clusterboot_nboot),
                       dmethod=isolate(input$constructs_clusterboot_dmethod),
                       cmethod=isolate(input$constructs_clusterboot_cmethod)) 
    values$constructs_clusterboot <- s
  })
  
  output$construct_clusterboot <- renderPlot({
    s <- values$constructs_clusterboot
    if (!is.null(s)) {
      plot(s)
      if (input$constructs_clusterboot_drawrects){
        pvclust::pvrect(s, max.only=input$constructs_clusterboot_maxonly,
                        alpha=input$constructs_clusterboot_alpha)
      }       
    } else {
      plot.new()
      text(.5, .5, "Analysis not yet prompted", cex=1.3)
    }
  })
  
  
  #### ____ PCA ####
  
  output$construct_pca <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      pca <- constructPca(x, nfactors = input$constructs_pca_nfactors, 
                          rotate = input$constructs_pca_rotate,
                          method = input$constructs_pca_correlation,
                          trim = input$constructs_pca_trim)  
      print(pca, digits=input$constructs_pca_digits,
            cutoff=input$constructs_pca_cutoff)
    }
  })
  
  
  #### ____ Somers' d ####
  
  output$construct_somers <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      d <- constructD(x, dependent=input$constructs_somers_dependent, 
                      trim=input$constructs_somers_trim)    
      print(d, digits=input$constructs_somers_digits)
    }     
  })
  
  
  
  
  #### __ Elements ####
  
  
  #### ____ Correlation ####
  
  output$elements_correlation <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      elementCor(x, rc=input$elements_correlation_rc, 
                 method=input$elements_correlation_method, 
                 trim=input$elements_correlation_trim)    
    }    
  })
  
  
  output$elements_correlation_rms <- renderPrint({
    x <- get_file()
    if (!is.null(x) & input$elements_correlation_rms) {
      elementRmsCor(x,
                    rc=input$elements_correlation_rc,
                    method=input$elements_correlation_method, 
                    trim=input$elements_correlation_trim)      
    }    
  })
  
  
  #### ____ Distance ####
  
  output$elements_distance <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      d <- distance(x, along=2,
                    dmethod=input$elements_distance_dmethod, 
                    trim=input$elements_distance_trim)    
      print(d, digits=input$elements_distance_digits)
    }     
  })
  
  
  #### ____ Cluster ####
  
  output$elements_cluster <- renderPlot({
    x <- get_file()
    if (!is.null(x))
      cluster(x, along=2, 
              dmethod=input$elements_cluster_dmethod,
              cmethod=input$elements_cluster_cmethod,
              type = input$elements_cluster_type, 
              lab.cex=1.2, mar=c(4, 2, 3, 20), cex.main=1.2)
  })
  
  
  #### ____ Cluster (bootstrapped) ####
  
  # wird einmal automatisch aufgerufen, bei initialisierung
  observe({
    input$elements_clusterboot_update_button
    x <- isolate(get_file())
    s <- NULL
    if (!is.null(x))
      s <- clusterBoot(x, along=2,
                       nboot=isolate(input$elements_clusterboot_nboot),
                       dmethod=isolate(input$elements_clusterboot_dmethod),
                       cmethod=isolate(input$elements_clusterboot_cmethod)) 
    values$elements_clusterboot <- s
  })
  
  
  output$elements_clusterboot <- renderPlot({
    s <- values$elements_clusterboot
    if (!is.null(s)) {
      plot(s)
      if (input$elements_clusterboot_drawrects){
        pvclust::pvrect(s, 
                        max.only=input$elements_clusterboot_maxonly,
                        alpha=input$elements_clusterboot_alpha)
      }       
    } else {
      plot.new()
      text(.5, .5, "Analysis not yet prompted", cex=1.3)
    }
  })
  
  
  #### __ Indexes ####
  
  #### ____ PVAFF ####
  
  output$indexes_pvaff <- renderPrint({
    x <- get_file()
    if (!is.null(x))
      indexPvaff(x)
  })
  
  
  #### ____ Implicative Dilemma ####
  
  output$indexes_implicative_dilemma <- renderPrint({
    x <- get_file()
    self <- which(values$e.names %in% input$indexes_implicative_dilemma_self)
    ideal <- which(values$e.names %in% input$indexes_implicative_dilemma_ideal)
    if (!is.null(x))
      indexDilemma(x, self=self, ideal=ideal, 
                   r.min=input$indexes_implicative_dilemmas_rmin)
  })
  
  output$indexes_implicative_dilemma_plot <- renderPlot({
    x <- get_file()
    self <- which(values$e.names %in% input$indexes_implicative_dilemma_self)
    ideal <- which(values$e.names %in% input$indexes_implicative_dilemma_ideal)
    if (!is.null(x) & input$indexes_implicative_dilemmas_show)
      indexDilemma(x, self=self, ideal=ideal, 
                   show=TRUE, output=0)
  })
  
  #### ____ Intensity ####
  
  output$indexes_intensity <- renderPrint({
    x <- get_file()
    if (!is.null(x))
      indexIntensity(x)
  })
  
  
  
})


