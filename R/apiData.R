##' Query data from an API
##' 
##'  A high-level function that automates the querying and extraction of data from a web API.
##' @usage apiData(x, shortnames=FALSE)
##' @param x an apirequest object
##' @param shortnames logical, indicating whether the resulting tables (data frames) should have 
##' short variable names (default is FALSE, variable names contain nesting hierarchy)
##' @param method character, either "RwebAPI", "jsonlite", or "XML2R"
##' @return an apidata-object containing the returned data in a flat representation
##' @export
##' @examples
##' \dontrun{apidata <- apiData(x) }


apiData <-
function(x, shortnames=FALSE, method="RwebAPI") {
	stopifnot((is.character(x) |is.apirequest(x)), 
		(method=="RwebAPI" | method=="XML2R" | method=="jsonlite")
		 )
	
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
	
	if (ncol(inputargs)==0){  # no input-arguments (possible if not a form-url)?
            inputargs <- data.frame(API_url=x@URL, stringsAsFactors=FALSE)
            } else {
                  names(inputargs) <- paste("INPUT", names(inputargs), sep="_:_")
            }
	
	for ( i in names(inputargs)){
            inputargs[, i] <- as.character(inputargs[, i])
	}
	
	# query API and manage response
	apiresp <- apiGET(x)
      
      if (apirespOK(apiresp)) {
            if (method=="RwebAPI") {
                  nestedlistdata <- content2list(apiresp)
                  flatdata <- auto.tree2flat(nestedlistdata)
                  flatdata <- lapply(flatdata, cbindFill, inputargs )
                  flatdata <- lapply(flatdata, cleanFlatdata)
                  } else if (method=="XML2R" & grepl(pattern="xml", apiresp@type)){
                        flatdata <- xml2r(apiresp@body)
                        flatdata <- lapply(flatdata, cbindFill, inputargs )
                        flatdata <- lapply(flatdata, cleanFlatdata)
                        flatdata <- lapply(flatdata, FUN=function(i) { i[,names(i)!="url_key"] })
                  }
            
            if (shortnames==TRUE) flatdata <- lapply(flatdata, onlyLeafnames)
            
            apidata <- new(Class="apidata", 
                           data=flatdata, 
                           raw.data=apiresp@body, 
                           raw.type=apiresp@type, 
                           request.statusMessage=apiresp@statusMessage,
                           request.arguments=apiresp@request.arguments)
            
            return(apidata)
            
            } else {
                  
                  flatdata <- handleHTTPError(apiresp)
                  flatdata <- lapply(flatdata, cbindFill, inputargs )
                  flatdata <- lapply(flatdata, cleanFlatdata)
                  
                  apidata <- new(Class = "apidata",
                                 data = list(flatdata),
                                 raw.data=apiresp@body, 
                                 raw.type=apiresp@type, 
                                 request.statusMessage=apiresp@statusMessage,
                                 request.arguments=apiresp@request.arguments)
                  
                  return(apidata)
            }
}
