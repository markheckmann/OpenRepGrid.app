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

