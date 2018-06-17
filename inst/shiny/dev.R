library(shiny)
library(OpenRepGrid)

values <- reactiveValues()
values$cc <- 1   # initial cursor column
values$cr <- 1   # initial cursor row
values$current_grid <- boeker
# values$highlight_row_col <- FALSE

cond.panel.bertin <- 
  conditionalPanel(condition="input.level1=='bertin'",
                   tabsetPanel(
                     tabPanel("Normal", value = "panel_bertin"),
                     tabPanel("Change", value = "panel_bertin_modify",
                              # move cursor
                              tags$h4("Move cursor"),
                              actionButton("btn_up", "", icon("arrow-up", "fa-2x"), width = "99%"),
                              tags$br(),
                              actionButton("btn_left", "", icon("arrow-left", "fa-2x"), width = "49%"),
                              actionButton("btn_right", "", icon("arrow-right", "fa-2x"), width = "49%"),
                              tags$br(),
                              actionButton("btn_down", "", icon("arrow-down", "fa-2x"), width = "99%"),
                              # move constructs and elements
                              tags$h4("Move constructs and elements"),
                              actionButton("btn_move_up", "", icon("angle-double-up", "fa-2x"), width = "99%"),
                              tags$br(),
                              actionButton("btn_move_left", "", icon("angle-double-left", "fa-2x"), width = "49%"),
                              actionButton("btn_move_right", "", icon("angle-double-right", "fa-2x"), width = "49%"),
                              tags$br(),
                              actionButton("btn_move_down", "", icon("angle-double-down", "fa-2x"), width = "99%"),
                              # modify grid
                              tags$hr(),
                              tags$h4("Modify grid"),
                              actionButton("btn_swap_poles", " Swap poles", icon("exchange", "fa-2x"), width = "99%"),
                              actionButton("btn_delete_construct", " Delete construct", icon("minus-square", "fa-2x"), width = "99%"),
                              actionButton("btn_delete_element", " Delete element", icon("minus-square", "fa-2x"), width = "99%")
                              ),
                    id="tabs")
)                 


shinyApp(
  
  ui = fluidPage(
    sidebarPanel(
      cond.panel.bertin
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bertin", plotOutput("grid"), value = "bertin"),
      id = "level1")
      
    )
  ),
  
  server = function(input, output) 
  {
    
    observeEvent(input$tabs, {
      print(input$tabs)
    })
    
    # # highlight rows and constructs
    # observeEvent(input$btn_highlight_row_col, {
    #   values$highlight_row_col <- !values$highlight_row_col
    # })
    
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
    
    ## grid actions
    
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
      print("Move down")
      values$cr <- cr + 1
      values$current_grid <- down(grid, cr)
    })
    
    
    # move element left
    observeEvent(input$btn_move_left, {
      req(values$cc > 1 & values$cc <= ncol(values$current_grid))
      cc <- isolate(values$cc)
      grid <- isolate(values$current_grid)
      print("Move left")
      values$cc <- cc - 1
      values$current_grid <- left(grid, cc)
    })
    
    # move element right
    observeEvent(input$btn_move_right, {
      req(values$cc > 0 & values$cc < ncol(values$current_grid))
      cc <- isolate(values$cc)
      grid <- isolate(values$current_grid)
      print("Move right")
      values$cc <- cc + 1
      values$current_grid <- right(grid, cc)
    })
    
    # swap poles
    observeEvent(input$btn_swap_poles, {
      req(values$cr > 0 & values$cr <= nrow(values$current_grid))
      cr <- isolate(values$cr)
      grid <- isolate(values$current_grid)
      print("swap poles")
      values$current_grid <- swapPoles(grid, cr)
    })
    
    # delete construct
    observeEvent(input$btn_delete_construct, {
      grid <- isolate(values$current_grid)
      nr <- nrow(grid)
      req(values$cr >= 1 & values$cr <= nr)
      cr <- isolate(values$cr)
      print("Delete construct")
      values$cr <- ifelse(cr == nr, cr - 1, cr)
      values$current_grid <- grid[-cr, ]
    })
    
    # delete element
    observeEvent(input$btn_delete_element, {
      grid <- isolate(values$current_grid)
      ne <- ncol(grid)
      req(values$cc >= 1 & values$cc <= ne)
      cc <- isolate(values$cc)
      print("Delete element")
      values$cc <- ifelse(cc == ne, cc - 1, cc)
      values$current_grid <- grid[ , -cc]
    })
    
    
    # generate plot only if highlighting is on
    output$grid <- renderPlot({
      # check if bertin plot is shown with row/column highlighting
      if (input$tabs == "panel_bertin_modify") {
        bertin(values$current_grid, cc = values$cc, cr = values$cr)
      } else {
        bertin(values$current_grid, cc = 0, cr = 0)    
      }
    })
    
  }
)




