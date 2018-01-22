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


## Basic HTTP GET request to a web API
## 
## Sends GET requests to an API based on an apirequest object and handles the response. (wrapped around RCurl::getURL).
## @usage apiGET(x)
## @param x an apirequest object
## @return an apiresponse-object containing the response
## @examples
## # First, make sure the necessary API key is saved in your R session:
## # (This example is based on the Project Vote Smart API [PVS API])
## saveAPIkey(key.var="pvs", key="YOUR-KEY-HERE")
## # first create a request function:
## pvsmeasure <- "http://api.votesmart.org/Measure.getMeasure?"
## measureparameters <- data.frame(parameter="measureId", value=NA)
## getMeasureRequest <- apiRequestFunction(x=measureparameters, base.url=pvsmeasure,key.param="key",key.object="pvs")
## mr <- getMeasureRequest(measureId=1632) # create a request object
## # get some data from the PVS API...
## \dontrun{apiresponse <- apiGET(mr) # only works with a proper PVS API key}


apiGET <-
      function(x) {
            stopifnot(is.apirequest(x))
            
            url <- x@URL
            
            hf <- basicHeaderGatherer()
            body  <- try(getURL(url, headerfunction=hf$update, useragent="RCurl-RWebData",
                                .opts=curlOptions(followlocation=TRUE)), silent=TRUE)
            
            # Error handling in case of unexpected binary response:
            if (class(body)=="try-error"){
                  if (grepl("embedded nul in string", attributes(body)$condition )){
                        
                        # save binary data temporarily to decompress and read body
                        bin <- getBinaryURL(url, headerfunction=hf$update, useragent="RCurl-RWebData")
                        temp <- tempfile()
                        con <- file(temp, open = "wb")
                        writeBin(bin, con)
                        close(con)
                        body <- paste(readLines(temp, warn=FALSE), collapse="")
                        unlink(temp)
                        
                        } else {
                              body <- getURLContent(url, headerfunction=hf$update, useragent="RCurl-RWebData",
                                                    .opts=curlOptions(followlocation=TRUE))
                        }
            }
            
            header <- hf$value() # for optional use in further functions
            type <- header["Content-Type"]
            statusMessage <- header["statusMessage"]
            
            resp <- new("apiresponse", 
                        body=body,
                        header=header,
                        type=type, 
                        statusMessage=statusMessage, 
                        request.arguments=x@request.arguments,
                        nodefault.parameters=x@nodefault.parameters)
            
            return(resp)
      }
