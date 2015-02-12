##' Extract the data from a JSON document as one (or several) data frame(s)
##' 
##' Given JSON document, this function maps the potentially nested JSON data to one or several data frames.
##' The data extraction algorithm applied in this function is based on the assumption that the JSON document describes either one or several entity types containing a set of observations described by different variables.   
##' @usage JSONtoDataFrame(x)
##' @param x either a string containing JSON or the name of a file containing the JSON
##' @return one data frame or a list of several data frames
##' @export
##' @examples
##' JSON.ex <- system.file("exdata", "JSON_fiction.json", package = "RwebAPI")
##' JSONtoDataFrame(JSON.ex)




JSONtoDataFrame <-
  function(x) {
    
    x.list <- try(RJSONIO::fromJSON(x, nullValue=NA, simplify=FALSE), silent=TRUE)
    if (class(x.list)=="try-error") {x.list <- .fromJSON_R_NA(x)} # based and dependend on rjson
    x.vector <- flattenTree(x.list)
    x.df <- auto.tree2flat(x.vector)
    
    return(x.df)
    
  }
