## Query data from an API
## 
##  A high-level function that automates the querying and extraction of data from a web API. Unlike
##  apiData(), only the pure data is returned.
## @usage apiDatalight(x)
## @param x an apirequest object
## @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
## @return a list containing the returned data in a flat representation
## @export
## @examples
## \dontrun{apidata <- apiDatalight(x)}



apiDatalight <-
function(x, alignVariables) {
      stopifnot((is.apirequest(x) | is.character(x)))
      
      if (is.character(x)) {
            x <- url2apirequest(x)
      }
      
      # extract request arguments to merge with returned data (in order to facilitate joining different
      # resulting data frames later on)  
      nodefault <- x@nodefault.parameters
      requestarg <- x@request.arguments
      requestarg <- requestarg[!(names(requestarg) == "apikey")]
      
      if (nrow(nodefault)!=0){
            inputargs <- cbindFill(requestarg, nodefault)
            } else {
                  inputargs <- requestarg
            }
      
      if (ncol(inputargs)==0){ # no input-arguments (possible if not a form-url)?
            inputargs <- data.frame(API_url=x@URL, stringsAsFactors=FALSE)
            } else {
                  names(inputargs) <- paste("INPUT", names(inputargs), sep="_:_")
            }
      
      for ( i in names(inputargs)){
            inputargs[, i] <- as.character(inputargs[, i])
      }
    
    # query API and manage response
    apiresponse <- apiGET(x)
    
    if (apiresponseOK(apiresponse)) {
          nestedlistdata <- content2list(apiresponse)
          flatdata <- listToDataFrame(nestedlistdata, alignVariables)
          
          if (length(flatdata)==0){  # if empty, only return the input/query-variables
                flatdata <- list(inputargs)
                names(flatdata) <- "root"
          } else {
                flatdata <- lapply(flatdata, cbindFill, inputargs )
                #flatdata <- lapply(flatdata, cleanFlatdata) # BETTER TO CLEAN AT THE HIGHEST LEVEL WITH AN OPTION TO RETURN INPUT AS WELL OR NOT!
          }
          
          return(flatdata)
          
    } else {
          flatdata <- handleHTTPError(apiresponse)
          flatdata <- list(cbind(flatdata, inputargs))
          
          return(flatdata)
    }
}
