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
