#' class object
#' 
#' @slot id an identifier for the object, character, normally name:type:parent:package
#' @slot name the name of the class (also the class of it)
#' @slot type is it a "slot" of another class or a "class" itself
#' @slot parent what is the parent class
#' @slot package what package does the object belong to
#' @slot checked has it been checked whether it contains any slots
#' @slot level what level of package depth have we gone
#' @export
setClass("class_object",
         slots = list(id = "character",
                      name = "character",
                      type = "character",
                      parent = "character",
                      package = "character",
                      checked = "logical",
                      level = "numeric"),
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
#' @slot level what level of package checking have we gone to
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
  base_classes <- c("list", "character", "numeric", "double", "integer", "matrix", "data.frame", "ANY")
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
        parent = "",
        package = package_name,
        level = 0)
  })
  
  slot_objects <- list()
  
  object_checked <- sapply(package_objects, function(x){x@checked})
  
  curr_classes <- sapply(package_objects, function(x){x@name})
  
  while (sum(object_checked) != length(object_checked)){
    to_check <- which(!object_checked)
    for (i_class in to_check){
      print(i_class)
      tmp_obj <- package_objects[[i_class]]
      tmp_class <- methods::getClass(tmp_obj@name)
      
      tmp_slots <- tmp_class@slots
      
      if (length(tmp_slots) != 0){
        new_classes <- character(0)
        tmp_objects <- lapply(names(tmp_slots), function(x){
          t_slot <- tmp_slots[[x]]
          new("slot_object",
              name = x,
              parent = tmp_obj@id,
              slot_class = t_slot,
              package = tmp_obj@package)
        })
        tmp_objects <- lapply(tmp_objects, function(x){
          x@id <- paste(x@name, x@type, x@parent, x@package, sep = ":")
          x
        })
        slot_objects <- c(slot_objects, tmp_objects)
        
        if (tmp_obj@level < (depth + 1)){
          slot_classes <- sapply(tmp_objects, function(x){
            x@slot_class
          })
          new_classes <- slot_classes[!(slot_classes %in% c(base_classes, curr_classes))]
          
          if (length(new_classes) != 0){
            tmp_class_objects <- lapply(new_classes, function(x){
              x_class <- methods::getClass(x)
              new("class_object",
                  id = paste(x_class@className, ":", "class", "::", x_class@package, sep = ""),
                  name = x_class@className,
                  parent = "",
                  package = x_class@package,
                  level = tmp_obj@level + 1)
            })
            package_objects <- c(package_objects, tmp_class_objects)
            
          }
        }
        
      }
      tmp_obj@checked <- TRUE
      package_objects[[i_class]] <- tmp_obj
      
    }
    object_checked <- sapply(package_objects, function(x){x@checked})
    
    curr_classes <- sapply(package_objects, function(x){x@name})
  }
  
  out_graph <- create_class_graph(package_objects, slot_objects, base_classes)
}

#' create a class graph
#' 
#' given the package objects / classes, and slot objects / classes, create a graph that represents
#' their relationships
#' 
#' @param package_objects the package objects
#' @param slot_objects the slot objects
#' @param other_classes other, generally base classes
#' 
#' @return a graph
#' @export
create_class_graph <- function(package_objects, slot_objects, other_classes){
  package_nodes <- sapply(package_objects, function(x){x@id})
  slot_nodes <- sapply(slot_objects, function(x){x@id})
  
  other_nodes <- other_classes
  
  out_graph <- graphNEL(nodes = c(package_nodes, slot_nodes, other_nodes))
  nodeDataDefaults(out_graph, "type") <- "none"
  nodeDataDefaults(out_graph, "package") <- "none"
  nodeDataDefaults(out_graph, "name") <- "none"
  edgeDataDefaults(out_graph, "type") <- "none"
  
  nodeData(out_graph, package_nodes, "type") <- "class"
  nodeData(out_graph, slot_nodes, "type") <- "slot"
  nodeData(out_graph, other_nodes, "type") <- "base"
  
  for (i_slot in seq_along(slot_objects)){
    tmp_slot <- slot_objects[[i_slot]]
    n1 <- tmp_slot@id
    n2 <- tmp_slot@parent
    out_graph <- addEdge(n1, n2, out_graph, 2)
    edgeData(out_graph, n1, n2, "type") <- "slot"
    
    n3 <- tmp_slot@slot_class
    if (n3 %in% nodes(out_graph)){
      out_graph <- addEdge(n1, n3, out_graph, 1)
      edgeData(out_graph, n1, n3, "type") <- "class"
    }
    
  }
  out_graph
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