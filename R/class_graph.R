#' class graph of package
#' 
#' @param package the directory or package name in current environment
#' @param depth how deep to go for classes \emph{outside} the package (default = 0)
#' 
#' @import devtools
#' @export
#' @return graph of classes
class_graph <- function(package = ".", depth = 0){
  package_env <- get_package_env(package)
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
      assign(".packageName", value = substring(package, 9), envir = package_env)
    }
  package_env
  
}