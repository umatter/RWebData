##' Directly interact with any API
##' 
##'  A high-level function that directly queries and transforms data from any web API. 
##' @usage apiEasyData(x, base.url, shortnames=TRUE, ...)
##' @param x either a charachter string containing the whole url for the request or a named list 
##' containing the parameter names and values (see details)
##' @param base.url a character string containing the basic url for the api
##' @param shortnames logical, indicating whether the resulting tables (data frames) should have 
##' short variable names (default is FALSE, variable names contain nesting hierarchy)
##' @param ... currently only one parameter (simplify) passed down to the mapping algorithm if 
##' simplify is TRUE, the document tree is made simpler if possible (by removing unnecessary nodes)
##' @return a list of data-frames, containing the returned data in a flat representation
##' @import stringr
##' @import jsonlite
##' @import XML
##' @import igraph
##' @import RCurl
##' @import RJSONIO
##' @import plyr
##' @import XML2R
##' @import httr
##' @import mime
##' @import yaml
##' @export
##' @examples
##' \dontrun{apidata <- apiEasyData(x) }


apiEasyData <- 
  function(x, base.url, shortnames=TRUE, ...) {
    stopifnot( (is.list(x) | is.character(x)))
    
    if(is.list(x)) {  # parameter/values as list? proceed with apiDatalight...
      
      reqfun <- apiRequestFunction(x=x, base.url=base.url)
      req <- reqfun()
      data <- apiDatalight(req, ...)
      
      # cosmetics
      if (shortnames==TRUE) {
            data <- lapply(data, onlyLeafnames)}
      if (length(data)==1 & is.data.frame(data[[1]])) {
            data <- data[[1]]}
      
      return(data)
      
    } else { # url as input? simple procedure with less information in api-objects
      
      req <- url2apirequest(x)
      data <- apiDatalight(req, ...)
      
      # cosmetics
      if (shortnames==TRUE) {
            data <- lapply(data, onlyLeafnames)}
      if (length(data)==1 & is.data.frame(data[[1]])) {
            data <- data[[1]]}
      
      return(data)

   }
  }
    