##' Check if object is an apirequest
##' 
##'  A function to check if an object is of class "apisreqest".
##' @usage is.apirequest(x)
##' @param x any R object
##' @return logical, TRUE if x is of class apirequest.
##' @export


is.apirequest <-
      function(x) {
            inherits(x, "apirequest")
      }
