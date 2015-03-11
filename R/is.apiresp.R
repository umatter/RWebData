##' Check if object is an apiresp
##' 
##'  A function to check if an object is of class "apiresp".
##' @usage is.apiresp(x)
##' @param x any R object
##' @return logical, TRUE if x is of class apiresp.
##' @export


is.apiresp <-
      function(x) {
            inherits(x, "apiresp")
      }
