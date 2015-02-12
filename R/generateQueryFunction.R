
##' Writes a function to interact with an API
##' 
##' A function that generates a function to send specific API-requests and handle the returned data.
##' @usage generateQueryFunction(x, base.url, multiparam=NULL, key.param=NA, 
##' key.object=NA, .vectorizeIt=FALSE)
##' @param x a two-column data frame containing the parameter names and values (see details).
##' @param base.url a character string containing the host url of the api.
##' @param multiparam a character string with the name of a parameter that can take multiple input values in the resulting function  (defaults to NULL).
##' @param key.param a character string containing the api key parameter required in the url.
##' @param key.object the name of the object the api.key is saved in (as character).  
##' @param .vectorizeIt logical, indicating whether the resulting function should contain an implicit loop over parameters with no default value (i.e., allows vectors as inputs). Default is FALSE. TRUE only works if x is a data frame.      
##' @return a function
##' @details x should contain the parameter names in the first column and respective default-values in the second column (both as character strings) Parameters that have no default value have NAs in the second column.The function attempts to get the api-key from the environment "apikeys" (therefore has to be defined there before; see saveAPIkey()).
##' @export
##' @examples
##' # First, make sure the necessary API key is saved in your R session:
##' # (This example is based on the Project Vote Smart API [PVS API])
##' saveAPIkey(key.var="pvs", key="YOUR-KEY-HERE")
##' pvsmeasure <- "http://api.votesmart.org/Measure.getMeasure?"
##' measureparameters <- data.frame(parameter="measureId", value=NA)
##' \dontrun{getMeasure <- generateOSIFunction(x=measureparameters, base.url=pvsmeasure,
##'  key.param="key", key.object="pvs")}


generateQueryFunction <-
  function(x, base.url, multiparam=NULL, key.param=NA, key.object=NA, .vectorizeIt=FALSE){
    stopifnot((is.null(multiparam) | is.character(multiparam)), length(multiparam)<=1, (is.data.frame(x)| is.list(x)))
    if (!is.data.frame(x) & is.list(x) & .vectorizeIt ) { stop(".vectorizeIt=TRUE can only be used in combination with x as a data frame.\nx is not a data frame.")}
    
    
    f <- function(){}  # set basic function to be extended...
    
    
    reqfun <-  apiRequestFunction(x=x, 
                                  base.url=base.url,
                                  multiparam=multiparam,
                                  key.param=key.param,
                                  key.object=key.object,
                                  vectorizeIt=FALSE)
    
    # 1) set formals
    formals(f) <- formals(reqfun) # the formals must be the same as in urlfun 
    
    # 2) write and set body
    
    # a) write the reqfun part in f
    # not very elegant solution: don't use reqfun as internal function but
    # just use its body as part of this functions body!
    reqfun.string <- deparse(body(reqfun))
    reqfun.string2 <- reqfun.string[2:(length(reqfun.string)-2)]
    e.reqfun <- parse(text=reqfun.string2)
    
    # b) rest of the request
    e.get <- expression(data <- apiDatalight(apirequest))
    e.df <- expression(if (length(data)==1 & is.data.frame(data[[1]])) {data <- data[[1]]})
    e.return <- expression(return(data))


    # define body of the function
    e <- c(e.reqfun, e.get, e.df, e.return)
    body(f) <- as.call(c(as.name("{"),e))
    
    if (.vectorizeIt){# should function contain implicit loop over no-default parameters?
      .plist <- as.paramlist(x)    
      
      if (length(.plist$nodefaults)>0){ 
        f <- vectorizeIt(f) 
      } else {
        warning("All parameters contain default values, no vectorization possible.")
      }
    }
    
    
    return(f)
    
  }
