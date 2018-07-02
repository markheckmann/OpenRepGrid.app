library(shiny)
library(rgl)
library(OpenRepGrid)

open3d(useNULL = TRUE)
#ids <- plot3d(rnorm(100), rnorm(100), rnorm(100))[1]
ii <- 1:4
d <- data.frame(x=ii, y=ii^2, z=log(ii), label=LETTERS[ii])


ui <- (fluidPage(
  sidebarPanel(
    checkboxInput("chk1", label = "1", value = FALSE),  
    checkboxInput("chk2", label = "2", value = FALSE),  
    playwidgetOutput("control")
  ),
  mainPanel(
    rglwidgetOutput("wdg")  
  )
))


server <- function(input, output, session) 
{
  options(rgl.useNULL = TRUE)
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  observe({
    
    sphereid <<- with(subset(iris, Species == "setosa"), 
                      spheres3d(Sepal.Length, Sepal.Width, Petal.Length, 
                                col=as.numeric(Species),
                                radius = 0.211))
    with(subset(iris, Species == "versicolor"), 
         spheres3d(Sepal.Length, Sepal.Width, Petal.Length, 
                   col=as.numeric(Species),
                   radius = 0.211))
    with(subset(iris, Species == "virginica"), 
         spheres3d(Sepal.Length, Sepal.Width, Petal.Length, 
                   col=as.numeric(Species),
                   radius = 0.211))
    aspect3d(1,1,1)
    axesid <<- decorate3d()
    subid <<- currentSubscene3d()
    
    open3d()
    ids <- list()
    for (i in 1:nrow(d)) {
      ids[[i]] <- as.numeric(points3d(d[i,]))
    }
    names(ids) <- d$label
    ids <<- unlist(ids)
    scene <<- scene3d()
    rgl.close()
    
    # ids <<- biplot3d(boeker, c.text.col = "#ff00ff")
    # scene <<- scene3d()
    # rgl.close()
  })
  
  output$wdg <- renderRglwidget({
    rglwidget(scene, controllers = c("control"))
  })
  
  output$control <- renderPlaywidget({
    toggleWidget("wdg", respondTo = "chk2", ids = ids[3:4])
    toggleWidget("wdg", respondTo = "chk1", ids = ids[1:2])
  })
}

shinyApp(ui = ui, server = server)



