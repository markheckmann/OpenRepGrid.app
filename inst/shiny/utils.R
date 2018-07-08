# utility function

eval_text <- function(x) {
  eval(parse(text=x))
}



# produce collapsible info boxes at top of every page
#
# An anchor and a div (with the content) is generated with the ids
# "toggle-id-info" and "id-info". The js function toggleAndChangeText is passed
# the div's id and will do the collapsing and change of the anchor text.
#
inject_info_on_top_of_ui_pages <- function(id, infofile="") 
{
  if ( !file.exists(infofile) ) {
    cat("infofile does not exist")
    return("")
  }   
  part.1 <- 
  '<div>
    <p class="alignright"> 
      <a id="toggle-__id__-info" class="toggle-info" href="javascript:toggleAndChangeText(\'__id__-info\');"> Info &#9660</a> 
    </p>
  </div>
  <div style="clear: both;"></div>
  <div id="__id__-info" class="mycoll">'
  part.1 <- stringr::str_replace_all(part.1, "__id__", id)
  part.2 <- paste0(readLines(infofile), collapse="\n")
  part.3 <- '\n</div>'
  paste(part.1, part.2, part.3, collapse="\n")
}


# Take a grid object and return a dataframe
# A dataframe is needed for the DT datatble which 
# allows modifying values in shiny
# 
get_grid_dataframe <- function(x)
{
  # check if is repgrid object
  if (!inherits(x, "repgrid"))
      stop("'x' muste be a repgrid object.", call. = FALSE)
  
  e <- elements(x)
  c <- constructs(x)
  r <- getRatingLayer(x, names=FALSE)
  
  r <- as.data.frame(r)
  
  # construe matrix
  d <- data.frame(leftpole = c$leftpole, 
                  r, 
                  rightpole = c$rightpole, stringsAsFactors = FALSE)
  colnames(d) <- c("left pole",
                   e,
                   "right pole")
  d
}



# take the dataframe returned from modifiable table
# and update the current grid object if anything in the table 
# has changed.
# x: new data from hot table
#
update_current_grid_from_df <- function(x)
{
  # get current grid object  
  g <- isolate(values$current_grid)
  
  # if any values have changed then update current grid
  if ( !all(x == get_grid_dataframe(g), na.rm = T) ) {
    cat("\nUpdate current grid with modifiable table data")
  
  # get components: ratings, poles 
  right <- ncol(x)            # right poles
  leftpoles(g) <- x[[1]]      # replace left poles with data from hot table
  rightpoles(g) <- x[[right]] # replace right poles with data from hot table
  
  # update ratings
  r <- x[, c(-1, -right), drop=FALSE]
  g[,] <- as.matrix(r)
 
  # replace current grid
  values$current_grid <- g
  }
}












