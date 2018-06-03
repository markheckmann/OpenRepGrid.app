
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
  
  
  
  #### ______________________ ####
  #### RENDERERS #### 
  
  
  
  #### __Load grids ####
  
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
  
  
  
  #### __Bertin ####
  
  output$bertin_info <- renderUI({
    HTML( inject_info_on_top_of_ui_pages("bertin", "www/info/bertin.html") )  
  })
  
  output$bertin <- renderPlot({
    cex <- 1.1
    x <- get_file()
    if (!is.null(x))
      bertin(x, 
             cex.elements=input$bertin_standard_cex_all, 
             cex.text=input$bertin_standard_cex_all,
             cex.constructs=input$bertin_standard_cex_all,
             colors=c(input$bertin_standard_color_left, input$bertin_standard_color_right),
             showvalues=input$bertin_standard_showvalues,
             xlim=c(input$bertin_standard_xlim_1, 1 - input$bertin_standard_xlim_2),
             ylim = c(0, input$bertin_standard_ylim))
  })
  


  
})


