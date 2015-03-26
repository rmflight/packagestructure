#' class graph of package
#' 
#' @param package the directory or package name in current environment
#' 
#' @import devtools methods igraph
#' @author Robert M Flight
#' @export
#' @return igraph
class_graph <- function(package = "."){
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
  
  package_class_tree <- multiclass_tree(package_classes, where = package_env, all = FALSE)
  
  # remove ClassUnions before proceeding, they mess us up in all kinds of ways
  package_class_vertices <- igraph::V(package_class_tree)$name
  package_classes <- lapply(package_class_vertices, getClassDef, where = package_env)
  is_union <- which(sapply(package_classes, isClassUnion))
  
  if (length(is_union) > 0){
    package_class_tree <- igraph::delete.vertices(package_class_tree, is_union)
    package_class_vertices <- igraph::V(package_class_tree)$name
    package_classes <- lapply(package_class_vertices, getClassDef, where = package_env)
  }
  
  package_class_id <- sapply(package_classes, function(x){paste(x@className, x@package, sep = ":")})
  V(package_class_tree)$name <- package_class_id
  V(package_class_tree)$label <- package_class_vertices # using the class names as labels
  V(package_class_tree)$type <- "class"
  names(package_classes) <- package_class_id
  
  class_slots <- lapply(package_classes, function(x){
    tmp_slots <- x@slots
    n_slots <- length(tmp_slots)
    slot_package <- slot_names <- slot_class <- slot_id <- slot_class_package <-  "NA"
    if (n_slots != 0){
      slot_package <- x@package
      slot_names <- names(tmp_slots)
      slot_class <- unlist(tmp_slots)
      slot_class_package <- sapply(tmp_slots, attr, "package")
      slot_id <- paste(slot_names, slot_class, slot_package, sep = ":")
    }
    
    return(list(id = slot_id, names = slot_names, package = slot_package, class = slot_class, class_package = slot_class_package))
  })
  
  n_in_vertices <- sapply(seq(1, length(package_class_vertices)), function(node_id){
    length(igraph::incident(package_class_tree, node_id, mode = "in"))
  })
  n_out_vertices <- sapply(seq(1, length(package_class_vertices)), function(node_id){
    length(igraph::incident(package_class_tree, node_id, mode = "out"))
  })
  
  leaf_vertices <- which((n_in_vertices == 0) & (n_out_vertices >= 1)) # the leaves to iterate over, doing recursion
  
  slot_list <- lapply(class_slots, function(x){x$id})
  slot_names <- lapply(class_slots, function(x){x$names})
  
  keep_slots <- slot_list
  
  all_slots <- unlist(slot_list, use.names = FALSE)
  all_names <- unlist(slot_names, use.names = FALSE)
  
  dup_slots <- duplicated(all_slots)
  all_slots <- all_slots[!dup_slots]
  all_names <- all_names[!dup_slots]
  # removes slots from a list based on an initial and continuously updated list
  remove_slots <- function(start_vertex, tree, slot_list){
    if (slot_list[1] != "NA"){
      slot_to_remove <<- unique(c(slot_to_remove, slot_list[[start_vertex]]))
    }
    below <- igraph::neighbors(tree, start_vertex, 1)
    if (length(below) > 0){
      for (i_below in below){
        #cat(i_below, ", ", sep = "")
        keep_slots[[i_below]] <<- keep_slots[[i_below]][!(keep_slots[[i_below]] %in% slot_to_remove)]
        remove_slots(i_below, tree, slot_list)
      }
    }
  }
  
  for (i_leaf in leaf_vertices){
    #cat("\nleaf: ", i_leaf, "\n", sep = "")
    slot_to_remove <- character(0)
    if (slot_list[[i_leaf]][1] != "NA"){
      slot_to_remove <- unique(c(slot_to_remove, slot_list[[i_leaf]]))
    }
    remove_slots(i_leaf, package_class_tree, slot_list)
  }
  
  # need to add slots to graph, and then add edges between slots and classes based on keep_slots
  # should have data frame with *name* of vertex, *id* of vertex (class:package, slot:class:package)
  # and index of vertex in the graph. This should allow easy matching to create the class - slot edges
  slot_tree <- igraph::graph.empty(directed = TRUE)
  slot_tree <- igraph::add.vertices(slot_tree, length(all_slots), name = all_slots, label = all_names, type = "slot")
  
  class_data <- data.frame(name = V(package_class_tree)$name, label = V(package_class_tree)$label, type = V(package_class_tree)$type, stringsAsFactors = FALSE)
  row.names(class_data) <- class_data$name
  slot_data <- data.frame(name = V(slot_tree)$name, label = V(slot_tree)$label, type = V(slot_tree)$type, stringsAsFactors = FALSE)
  row.names(slot_data) <- slot_data$name
  
  pc_tree <- package_class_tree
  pc_tree <- remove.vertex.attribute(pc_tree, "label")
  pc_tree <- remove.vertex.attribute(pc_tree, "type")
  
  s_tree <- slot_tree 
  s_tree <- remove.vertex.attribute(s_tree, "label")
  s_tree <- remove.vertex.attribute(s_tree, "type")
  
  pcs_tree <- igraph::graph.union(pc_tree, s_tree)
  
  cs_data <- rbind(class_data, slot_data)
  
  V(pcs_tree)$label <- cs_data[V(pcs_tree)$name, "label"]
  V(pcs_tree)$type <- cs_data[V(pcs_tree)$name, "type"]
  
  for (i_class in names(keep_slots)){
    n_slot <- length(keep_slots[[i_class]])
    if (n_slot > 0){
      pcs_tree[from = rep(i_class, n_slot), to = keep_slots[[i_class]]] <- TRUE
    }
  }
  
  # this creates links from classes to their slots. Now we need to go through the slots and see if they link to other classes
  # already present
  # we do this by finding the classes of all the slots, and then searching for bits of the tree that 
  # have the "class_id", and making new edges
  slot_data <- unique(data.frame(id = unlist(lapply(class_slots, function(x){x$id}), use.names = FALSE),
                          name = unlist(lapply(class_slots, function(x){x$names}), use.names = FALSE),
                          class = unlist(lapply(class_slots, function(x){x$class}), use.names = FALSE),
                          class_package = unlist(lapply(class_slots, function(x){x$class_package}), use.names = FALSE),
                          stringsAsFactors = FALSE))
  slot_data$class_id <- paste(slot_data$class, slot_data$class_package, sep = ":")
  
  slot_data <- slot_data[(slot_data$class_id) %in% V(pcs_tree)$name,]
  
  n_slot_link <- nrow(slot_data)
  
  if (n_slot_link > 0){
    for (i_slot in seq(1, n_slot_link)){
      pcs_tree[from = slot_data[i_slot, "id"], to = slot_data[i_slot, "class_id"]] <- TRUE
    }
  }

  V(pcs_tree)$shape <- "square"
  V(pcs_tree)$shape[V(pcs_tree)$type == "slot"] <- "circle"
  
  pcs_tree
  
}


#' get package env
#' 
#' @param package string giving the package environment to find
#' 
#' @return environment
#' @author Robert M Flight
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
#' @author Robert M Flight
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
#' I copied it from the \package{classGraph} package, as I did not want to depend on
#' \package{RGraphviz} for this package.
#' 
#' @author Martin Maechler
#' @param Cl class
#' @param where the package namespace to query
#' @param all \emph{all} of the inherited classes or just direct sub-classes to return
#' 
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

#' get sub-classes of a class
#' 
#' given a class definition, get the subclasses of that class
#' 
#' @parma Cl class to get sub-classes of
#' @param directOnly get only the direct sub-classes
#' @param complete get complete??
#' @author Martin Maechler
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


#' create a branch graph
#' 
#' creates a \emph{branch graph}, a simple tree with root and n branches
#' 
#' @param n number of leaves
#' @parma root the root node
#' @param leaves the named leaves
#' @param l.prefix prefix to use for leaves
#' @param weights weights of edges to the leaves
#' @param mode one of "undirected" or "directed"
#' 
#' @author Martin Maechler
bGraph <- function(n, root = "Mom",
                   leaves = paste(l.prefix, seq(length=n), sep=""),
                   l.prefix = "D", # for 'D'aughter
                   weights = NULL,
                   mode = c("undirected", "directed"))
{
  if(!missing(leaves)) {
    stopifnot(is.character(leaves))
    n <- length(leaves)
  } else stopifnot(is.numeric(n), length(n) == 1, n >= 0)
  
  mode <- "directed" %in% mode
  igraph::graph.edgelist(cbind(root, leaves), directed = mode)
}