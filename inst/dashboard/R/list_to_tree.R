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

paths_to_jstree <- function(paths) {
  new_names <- strsplit(names(paths), "/")
  paths <- lapply(seq_along(new_names), \(i) {
    replicate(length(new_names[[i]]), paths[[i]], simplify = FALSE) |>
      stats::setNames(new_names[[i]])
  })
  build_nodes <- function(path_list) {
    first_elements <- sapply(path_list, function(x) names(x)[1])
    unique_elements <- unique(first_elements)

    lapply(unique_elements, function(elem) {
      sub_paths <- lapply(path_list[first_elements == elem], function(x) x[-1])
      sub_paths <- sub_paths[sapply(sub_paths, length) > 0]

      node <- list(text = elem)

      if (length(sub_paths) > 0) {
        node$children <- build_nodes(sub_paths)
        node$type     <- "folder"
        node$state    <- list(opened = TRUE)
      } else {
        nm <- lapply(path_list, names) |> unlist()
        node$type <- "file"
        node$data <- list(is_file = TRUE,
                          extra = path_list[nm == elem][[1]][[1]])
      }
      return(node)
    })
  }
  build_nodes(paths)
}
