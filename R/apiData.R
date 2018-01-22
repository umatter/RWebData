# This file is part of RWebData.
# 
# RWebData is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# RWebData is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with RWebData.  If not, see <http://www.gnu.org/licenses/>.


##' Query data from an API
##' 
##'  A high-level function that automates the querying and extraction of data from a web API.
##' @usage apiData(x, shortnames=FALSE, method="RWebData", alignVariables=FALSE)
##' @param x an apirequest object
##' @param shortnames logical, indicating whether the resulting tables (data frames) should have 
##' short variable names (default is FALSE, variable names contain nesting hierarchy)
##' @param method character, either "RWebData", "jsonlite", or "XML2R"
##' @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
##' @return an apidata-object containing the returned data in a flat representation
##' @export
##' @examples
##' \dontrun{apidata <- apiData(x) }


apiData <-
function(x, shortnames=FALSE, method="RWebData", alignVariables=FALSE) {
	stopifnot((is.character(x) |is.apirequest(x)), 
		(method=="RWebData" | method=="XML2R")
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
	apiresponse <- apiGET(x)
      
      if (apiresponseOK(apiresponse)) {
            if (method=="RWebData") {
                  nestedlistdata <- content2list(apiresponse)
                  flatdata <- listToDataFrame(nestedlistdata, alignVariables)
                  flatdata <- lapply(flatdata, cbindFill, inputargs )
                  flatdata <- lapply(flatdata, cleanFlatdata)
                  } else if (method=="XML2R" & grepl(pattern="xml", apiresponse@type)){
                        flatdata <- xml2r(apiresponse@body)
                        flatdata <- lapply(flatdata, cbindFill, inputargs )
                        flatdata <- lapply(flatdata, cleanFlatdata)
                        flatdata <- lapply(flatdata, FUN=function(i) { i[,names(i)!="url_key"] })
                  }
            
            if (shortnames==TRUE){
                  flatdata <- lapply(flatdata, onlyLeafnames)
            }
            
            apidata <- new(Class="apidata", 
                           data=flatdata, 
                           raw.data=apiresponse@body, 
                           raw.type=apiresponse@type, 
                           request.statusMessage=apiresponse@statusMessage,
                           request.arguments=apiresponse@request.arguments)
            
            return(apidata)
            
            } else {
                  
                  flatdata <- handleHTTPError(apiresponse)
                  flatdata <- list(cbind(flatdata, inputargs))
                  
                  apidata <- new(Class = "apidata",
                                 data = list(flatdata),
                                 raw.data=apiresponse@body, 
                                 raw.type=apiresponse@type, 
                                 request.statusMessage=apiresponse@statusMessage,
                                 request.arguments=apiresponse@request.arguments)
                  
                  return(apidata)
            }
}
