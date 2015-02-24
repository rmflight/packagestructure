#' class object
#' 
#' @slot id an identifier for the object, character, normally name:type:package
#' @slot name the name of the class (also the class of it)
#' @slot type is it a "slot" of another class or a "class" itself
#' @slot package what package does the object belong to
#' @slot checked has it been checked whether it contains any slots
#' @slot level what level of package depth have we gone
#' @export
setClass("class_object",
         slots = list(id = "character",
                      name = "character",
                      type = "character",
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
  
  package_classes <- methods::getClasses(where = package_env)
  
  if (length(package_classes) == 0){
    return("No classes in package!")
  }
  
  package_class_tree <- multiclass_tree(package_classes, where = package_env, all = TRUE)
  
  package_class_vertices <- igraph::V(package_class_tree)$name
  
  package_classes <- lapply(package_class_nodes, getClassDef, where = package_env)
  
  package_class_id <- sapply(package_classes, function(x){paste(x@className, x@package, sep = ":")})
  
  class_slots <- lapply(package_classes, function(x){
    tmp_slots <- x@slots
    n_slots <- length(tmp_slots)
    slot_package <- slot_names <- slot_class <- slot_id <- "NA"
    if (n_slots != 0){
      slot_package <- x@package
      slot_names <- names(tmp_slots)
      slot_class <- unlist(tmp_slots)
      slot_class_package <- sapply(tmp_slots, attr, "package")
      slot_id <- paste(slot_names, slot_class, slot_package, sep = ":")
    }
    
    return(list(id = slot_id, names = slot_names, package = slot_package, class = slot_class, class_package = slot_class_package))
  })
  
  n_in_vertices <- sapply(seq(1, length(package_class_nodes)), function(node_id){
    length(incident(package_class_tree, node_id, mode = "in"))
  })
  
  leaf_vertices <- which(n_in_vertices == 1) # the leaves to iterate over, doing recursion
  
  
  
  slot_classes <- list()
  
  pkg_classes_checked <- sapply(package_classes, function(x){x@checked})
  
  curr_classes <- sapply(package_classes, function(x){x@name})
  
  while (sum(pkg_classes_checked) != length(pkg_classes_checked)){
    to_check <- which(!pkg_classes_checked)
    for (i_class in to_check){
      print(i_class)
      tmp_obj <- package_classes[[i_class]]
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
        tmp_slot_classes <- lapply(tmp_objects, function(x){
          x@id <- paste(x@name, x@type, x@parent, x@package, sep = ":")
          x
        })
        slot_classes <- c(slot_classes, tmp_slot_classes)
        
        if (tmp_obj@level < (depth + 1)){
          get_classes <- sapply(tmp_objects, function(x){
            x@slot_class
          })
          new_classes <- get_classes[!(get_classes %in% c(base_classes, curr_classes))]
          
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
            package_classes <- c(package_classes, tmp_class_objects)
            
          }
        }
        
      }
      tmp_obj@checked <- TRUE
      package_classes[[i_class]] <- tmp_obj
      
    }
    pkg_classes_checked <- sapply(package_classes, function(x){x@checked})
    
    curr_classes <- sapply(package_classes, function(x){x@name})
  }
  
  out_graph <- create_class_graph(package_classes, slot_classes, base_classes)
  out_graph
}

#' create a class graph
#' 
#' given the package objects / classes, and slot objects / classes, create a graph that represents
#' their relationships
#' 
#' @param package_classes the package objects
#' @param slot_classes the slot objects
#' @param other_classes other, generally base classes, these get appended to names
#' 
#' @return a graph
#' @export
create_class_graph <- function(package_classes, slot_classes, other_classes){
  package_nodes <- sapply(package_classes, function(x){x@id})
  package_class_names <- sapply(package_classes, function(x){x@name})
  names(package_class_names) <- package_nodes
  slot_nodes <- sapply(slot_classes, function(x){x@id})
  
  out_graph <- graph::graphNEL(nodes = c(package_nodes, slot_nodes), edgemode = "directed")
  graph::nodeDataDefaults(out_graph, "type") <- "none"
  graph::nodeDataDefaults(out_graph, "package") <- "none"
  graph::nodeDataDefaults(out_graph, "name") <- "none"
  graph::edgeDataDefaults(out_graph, "type") <- "none"
  
  graph::nodeData(out_graph, package_nodes, "type") <- "class"
  graph::nodeData(out_graph, slot_nodes, "type") <- "slot"
    
  graph::nodeData(out_graph, package_nodes, "name") <- sapply(package_classes, function(x){x@name})
  graph::nodeData(out_graph, package_nodes, "package") <- sapply(package_classes, function(x){x@package})
  
  graph::nodeData(out_graph, slot_nodes, "name") <- sapply(slot_classes, function(x){x@name})
  graph::nodeData(out_graph, slot_nodes, "package") <- sapply(slot_classes, function(x){x@package})
  
  other_class_slots <- sapply(slot_classes, function(x){x@slot_class}) %in% other_classes
  graph::nodeData(out_graph, slot_nodes[other_class_slots], "name") <- sapply(slot_classes[other_class_slots], function(x){paste(x@name, x@slot_class, sep = ":")})
  
  
  for (i_slot in seq_along(slot_classes)){
    tmp_slot <- slot_classes[[i_slot]]
    n1 <- tmp_slot@id
    n2 <- tmp_slot@parent
    out_graph <- graph::addEdge(n2, n1, out_graph, 2)
    graph::edgeData(out_graph, n2, n1, "type") <- "slot"
    
    n3 <- tmp_slot@slot_class
    if (n3 %in% graph::nodes(out_graph)){
      out_graph <- graph::addEdge(n1, n3, out_graph, 1)
      graph::edgeData(out_graph, n1, n3, "type") <- "class"
    }
    
    if (n3 %in% package_class_names){
      n3_true <- names(package_class_names)[package_class_names %in% n3]
      out_graph <- graph::addEdge(n1, n3_true, out_graph, 1)
      graph::edgeData(out_graph, n1, n3_true, "type") <- "class"
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


#' multi-class tree
#' 
#' Given many different classes for a package, get each of their trees, and then join them
#' together to form a single tree
#' 
#' @param classes the classes to get
#' @param where the package namespace of the classes
#' @param all get all of the sub-classes or only direct
#' 
#' @return igraph
#' @import igraph
#' @export
multiclass_tree <- function(classes, where, all = FALSE){
  
  multi_tree <- igraph::graph.empty(directed = TRUE)
  
  for (i_tree in seq_along(classes)){
    tmp_tree <- classTree(classes[i_tree], where, all)
    multi_tree <- igraph::graph.union(multi_tree, tmp_tree)
  }
  
  return(multi_tree)
  
}

#' generate class tree
#' 
#' Given a class name, generates the class tree for that class.
#' 
#' This function is originally written by Martin Maechler in the \package{classGraph} package.
#' I copied it from the \package{classGraph} package. Didn't want to depend on it as that then forces
#' a dependency on \package{Rgraphviz} that I want to avoid. 
#' 
#' @author Martin Maechler
#' @param Cl class
#' @param where the package namespace to query
#' @param all \emph{all} of the inherited classes or just direct sub-classes to return
#' 
#' @export
#' @return igraph
#' @import igraph
classTree <- function(Cl, where, all = FALSE)
{
  ## First a check
  if (isClassDef(Cl)) {
    cDef <- Cl
    Cl <- cDef@className
  } else cDef <- getClass(Cl)
  
  ## Now define a recursive function that computes the extended subtree
  ## for one class, and uses this for all sub-classes of Cl
  subtree <- function(cl, all) {
    stopifnot(isClassDef(cl))
    clN <- cl@className
    if(getOption('verbose')) cat(" ST",clN,":")
    sc <- subClasses(cl, directOnly = !all)
    if(length(sc) == 0) {
      if(getOption('verbose'))  cat(" is leaf\n")
      ## one node named 'cl':
      g <- igraph::graph.empty()
      g <- igraph::add.vertices(g, 1, name=clN)
    }
    else {
      if(getOption('verbose'))  cat(" has leaves:\n\t")
      g <- bGraph(root = clN, leaves = sc, mode = "directed")
      for(cc in sc) {
        if(getOption('verbose'))  cat(":: ",clN,"-",cc,sep="")
        st <- subtree(getClass(cc, where = where), all = all)
        ##    -------## recursive
        #if(numNodes(st) > 1)
          g <- igraph::graph.union(g, st)
      }
    }
    g
  }
  
  subtree(cDef, all = all)
}

subClasses <- function(Cl, directOnly = TRUE, complete = TRUE, ...)
{
  ## utility for classTree():
  if (isClassDef(Cl)) {
    cDef <- Cl
    Cl <- cDef@className
  } else { ## need getClass() can give error because sub classes can
    ## be "not defined" (?!)   -- e.g. "iMatrix"
    cDef <- if (complete) getClass(Cl) else getClassDef(Cl)
  }
  
  subs <- showExtends(cDef@subclasses, printTo = FALSE)
  if(directOnly) subs$what[subs$how == "directly"] else subs$what
}

numOutEdges <- function(g)
{
  ## Purpose: returns a named integer vector giving for each node in g,
  ##  	the number of edges *from* the node
  ## ----------------------------------------------------------------------
  ## Arguments: g: graph
  ## ----------------------------------------------------------------------
  ## Author: Martin Maechler, Date:  8 Feb 2007, 22:59
  el <- sapply(edgeL(g), `[[`, "edges")
  sapply(el, length)
}

is.leaf <- function(g) numOutEdges(g) == 0
## The graph package now defines a leaves() generic {w/ degree.dir}
##     leaves  <- function(g) nodes(g)[is.leaf(g)]


bGraph <- function(n, root = "Mom",
                   leaves = paste(l.prefix, seq(length=n), sep=""),
                   l.prefix = "D", # for 'D'aughter
                   weights = NULL,
                   mode = c("undirected", "directed"))
{
  ## Purpose: Create a "branch graph", a simple tree with root and
  ##		n branches / leaves
  ## ----------------------------------------------------------------------
  ## Author: Martin Maechler, Date: Aug 2005
  if(!missing(leaves)) {
    stopifnot(is.character(leaves))
    n <- length(leaves)
  } else stopifnot(is.numeric(n), length(n) == 1, n >= 0)
  
  mode <- "directed" %in% mode
  igraph::graph.edgelist(cbind(root, leaves), directed = mode)
}