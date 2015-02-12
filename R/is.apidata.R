##' Check if object is an apidata
##' 
##'  A function to check if an object is of class "apidata".
##' @usage is.apidata(x)
##' @param x any R object
##' @return logical, TRUE if x is of class apidata.
##' @export


is.apidata <- 
  function(x) {
    
    inherits(x, "apidata")
    
  }
