convert_to_jstree <- function(x, name = NULL) {
  if (!is.list(x)) {
    return(list(
      text = if (is.null(name)) as.character(x) else name,
      icon = "glyphicon glyphicon-leaf",
      data = x
    ))
  }
  
  child_names <- names(x)
  
  children_list <- lapply(seq_along(x), function(i) {
    convert_to_jstree(x[[i]], name = child_names[i])
  })
  
  if (is.null(name)) {
    return(children_list)
  }
  
  return(list(
    text = name,
    opened = TRUE,
    children = children_list
  ))
}
