## Inspect the HTTP status of an apiresponse object
## 
## Inspects an apiresponse object for HTTP errors and issues warnings if an HTTP error occurred.
## @usage apiresponseOK(x) 
## @param x an apiresponse object
## @return logical. TRUE if no problem occured, FALSE if an HTTP error occurred.
## @examples
## # First, make sure the necessary API key is saved in your R session:
## # (This example is based on the Project Vote Smart API [PVS API])
## saveAPIkey(key.var="pvs", key="YOUR-KEY-HERE")
## # first create a request function:
## pvsmeasure <- "http://api.votesmart.org/Measure.getMeasure?"
## measureparameters <- data.frame(parameter="measureId", value=NA)
## getMeasureRequest <- apiRequestFunction(x=measureparameters, base.url=pvsmeasure,key.param="key",
## key.object="pvs")
## mr <- getMeasureRequest(measureId=1632) # create a request object
## # get some data from the PVS API...
## \dontrun{apiresponse <- apiGET(mr) # only works with a proper PVS API key}
## \dontrun{apiresponseOK(apiresponse) # only works with a proper PVS API key}




apiresponseOK <-
      function(x){
            stopifnot(is.apiresponse(x))
            
            status <- HTTPstatus(x)
            
            if (status==200) {
                  ok <- TRUE
                  } else {
                        ok <- FALSE
                  }
            return(ok)
      }
