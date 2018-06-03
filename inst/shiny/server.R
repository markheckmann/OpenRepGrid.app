
source("global.R")
source("samplegrid-infotext.R")    # info texts for sample grids


#### Shiny server file ####
shinyServer(function(input, output, session) {

  
  get_file <- reactive({
    values$current_grid
  })
  
  
  #### ---------------------- OBSERVERS ----------------------#### 
  
  
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
  
  
  
  #### ------------------ RENDERERS -----------------####
  
  #### |-- Load grids ####
  
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
         # HTML("To get started you can either select one of the already available sample grids
         #      from the literature or upload your own grid file. Your grid file has to be in 
         #      <a href='http://docu.openrepgrid.org/loading.html#txt-files' 
         #      target='_blank'>this format</a> to be read in."),
         HTML("<hr><p><font color='red'><b>Caveat:</b></font> Not all browser support all application features properly. 
              The Google Chrome browser seems to work in most cases.
              </p>")
    )
  })  
  

  
  #### ------------------- UI ELEMENTS  -----------------####
  
  # for Conditional Panels
  
  output$samplegrid_info <- renderUI({ 
    HTML(samplegrid_info[input$samplegrid])
  })
  
  output$upload_dialog <- renderUI({ 
    #input$samplegrid
    fileInput("gridfile", "",
              accept=c("text", "text/plain"))
  })
  
  
})


