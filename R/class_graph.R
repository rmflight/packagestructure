#' class object
#' 
#' @slot id an identifier for the object, character, normally name:type:parent:package
#' @slot name the name of the class (also the class of it)
#' @slot type is it a "slot" of another class or a "class" itself
#' @slot parent what is the parent class
#' @slot package what package does the object belong to
#' @slot checked has it been checked whether it contains any slots
#' @export
setClass("class_object",
         slots = list(id = "character",
                      name = "character",
                      type = "character",
                      parent = "character",
                      package = "character",
                      checked = "logical"),
         prototype = prototype(type = "class",
                               checked = FALSE))

#' slot object
#' 
#' @slot id identifier of the slot, normally name:type:parent:package
#' @slot name the name of the slot
#' @slot type slot
#' @slot parent the parent class object
#' @slot slot_class the class of the slot
#' @slot package the package it belongs to
#' @export
setClass("slot_object",
         slots = list(id = "character",
                      name = "character",
                      type = "character",
                      parent = "character",
                      slot_class = "character",
                      package = "character"),
         prototype = prototype(type = "slot"))

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
  
  package_objects <- methods::getClasses(where = package_env)
  
  if (length(package_classes) == 0){
    return("No classes in package!")
  }
  
  package_objects <- lapply(methods::getClasses(where = package_env), function(x){
    new("class_object",
        id = paste(x, ":", "class", "::", x, ":", package_name, sep = ""),
        name = x,
        type = "class",
        parent = "",
        ob_class = x,
        package = package_name)
  })
  
  object_checked <- sapply(package_objects, function(x){x@checked})
  
  curr_classes <- sapply(package_objects, function(x){x@ob_class})
  
  while (sum(object_checked) != length(object_checked)){
    to_check <- package_objects[!object_checked]
    for (i_class in seq_along(to_check)){
      tmp_obj <- to_check[[i_class]]
      tmp_class <- methods::getClass(tmp_obj@name)
      
      tmp_slots <- tmp_class@slots
      
      if (length(tmp_slots) != 0){
        new_classes <- character(0)
        slot_objects <- lapply(tmp_slots, function(x){
          new("class_object")
        })
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