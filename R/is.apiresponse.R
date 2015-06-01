##' Check if object is an apiresponse
##' 
##'  A function to check if an object is of class "apiresponse".
##' @usage is.apiresponse(x)
##' @param x any R object
##' @return logical, TRUE if x is of class apiresponse.
##' @export


is.apiresponse <-
      function(x) {
            inherits(x, "apiresponse")
      }
