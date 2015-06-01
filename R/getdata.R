##' Extract API Data
##' 
##'  Extracts the converted data from an apidata object.
##' @usage getdata(x)
##' @param x an object of class apidata
##' @return either a data frame or a list containing data frames.
##' @export
##' @examples
##' \dontrun{apidata <- apiData(x) # only works with a proper PVS API key}
##' \dontrun{getdata(apidata)}


getdata <-
      function(x) {
            stopifnot(is.apidata(x))
            
            return(x@data)
      }