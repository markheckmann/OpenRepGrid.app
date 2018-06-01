
#' \pkg{OpenRepGrid.gui} - Graphical User Interface to Analyze Repertory Grid Data
#'
#' @references
#' \itemize{
#'  \item Green, B. (2004). Personal construct psychology and content analysis. Personal Construct Theory & Practice, 1(3), 82-91.
#'  \item Jankowicz, D. (2004). The easy guide to repertory grids. Chichester, England: John Wiley & Sons.
#'  }
#' @keywords package repgrid
#' @name OpenRepGrid.gui-package
#' @docType package
NULL



#' Run OpenRepGrid.gui app
#'
#' This function starts the OpenRepGrid GUI app.
#' 
#' @param display.mode \code{auto} by default, can also be \code{showcase}.
#' See \link[shiny]{runApp}.
#' @param launch.browser Boolean, set \code{TRUE} to open the app in the browser.
#' See \link[shiny]{runApp}.
#' 
#' @export
#' @import shiny
#' @import shinythemes
#' @import shinyBS
#' 
#' @examples
#' \dontrun{
#' OpenRepGrid.gui()
#' }
#' 
OpenRepGrid.gui <- function(display.mode = "auto",
                            launch.browser = getOption("shiny.launch.browser", interactive()))
{
  appDir <- system.file("shiny", package = "OpenRepGrid.gui")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `OpenRepGrid.gui`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}
