# utility function

eval_text <- function(x) {
  eval(parse(text=x))
}
