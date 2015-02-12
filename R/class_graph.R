#' class object
#' 
#' @slot id an identifier for the object, character, normally name:type:parent:package
#' @slot name the name of the class or slot
#' @slot type is it a "slot" of another class or a "class" itself
#' @slot parent what is the parent class
#' @slot package what package does the object belong to
#' @export
setClass("class_object",
         slots = list(id = "character",
                      name = "character",
                      type = "character",
                      parent = "character",
                      package = "character"))

#' class graph of package
#' 
#' @param package the directory or package name in current environment
#' @param depth how deep to go for classes \emph{outside} the package (default = 0)
#' 
#' @import devtools methods graph
#' @export
#' @return graph of classes
class_graph <- function(package = ".", depth = 0){
  base_classes <- c("list", "character", "numeric", "double", "integer", "ANY")
  package_env <- get_package_env(package)
  
  if (grepl("^package", package)){
    package_name <- substring(package, 9)
  } else {
    package_name <- package_env$.packageName
  }
  
  package_classes <- methods::getClasses(where = package_env)
  
  if (length(package_classes) == 0){
    return("No classes in package!")
  }
  
  checked_classes <- rep(FALSE, length(package_classes))
  names(checked_classes) <- package_classes
  
  class_graph <- graph::graphNEL(nodes = base_classes, edgemode = "directed")
  nodeDataDefaults(class_graph, "type") <- "base_class"
  edgeDataDefaults(class_graph, "type") <- "slot"
  
  nodeData(class_graph, base_classes, "type") <- "base_class"
  
  class_graph <- addNode(package_classes, class_graph)
  
  while (sum(checked_classes) != length(checked_classes)){
    to_check <- names(checked_classes)[!checked_classes]
    for (i_class in to_check){
      is_node <- i_class %in% nodes(class_graph)
      if (!is_node){
        class_data <- methods::getClass(i_class, where = package_env)
        class_graph <- addNode(i_class)
      }
      
    }
  }
}



#' get package env
#' 
#' @param package string giving the package environment to find
#' 
#' @return environment
#' @export
get_package_env <- function(package = "."){
  stopifnot(is.character(package))
  
  has_package <- grepl("^package:", package)
  
  if ((package == ".") || (!has_package)){
    package_env <- devtools::load_all(devtools::as.package(package))$env
  } else {
      loaded_packages <- search()
      pos <- match(package, loaded_packages)
      package_env <- as.environment(pos)
    }
  package_env
  
}