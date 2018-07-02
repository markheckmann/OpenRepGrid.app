library(rgl)

open3d()
xyz <- matrix(rnorm(300), ncol = 3)
id <- plot3d(xyz, col = "red", type = "s")["data"]
par3d(mouseMode = "selecting")
share <- rglShared(id)

# This puts the selector below the widget.
rglwidget(shared = share, width = 300, height = 300) %>% rglMouse()

# This puts the selector above the widget.
rglMouse() %>% rglwidget(shared = share, width = 300, height = 300, controllers = .) 



library(rgl)
x <- plot3d(rnorm(10), rnorm(10), rnorm(10))
rglwidget(elementId = "theplot")
elementId2Prefix("theplot")
toggleButton(x["data"], prefix = "theplot")

ii <- 1:4
d <- data.frame(x=ii, y=ii^2, z=log(ii), label=LETTERS[ii])

open3d()
points_ids <- list()
for (i in 1:nrow(d)) {
  points_ids[[i]] <- as.numeric(points3d(d[i,]))
}
names(points_ids) <- d$label


s <- scene3d()

theplot <- plot3d(rnorm(100), rnorm(100), rnorm(100), col = "red")

widget <- rglwidget(height = 300, width = 300) 

toggleWidget(sceneId = as.numeric(s), points_ids[[2]], label = "Points")


rgl.clear()



library(rgl)
plotids <- with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                             type="s", col=as.numeric(Species)))
rglwidget(elementId = "plot3drgl")
toggleWidget(sceneId = "plot3drgl", ids = plotids["data"], label = "Data")
