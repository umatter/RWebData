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



##' Get web data in tabular form (data frames)
##' 
##'  A high-level function that directly queries and transforms data from any web API to a data frame or list of data frames. 
##' @usage getTabularData(x, base.url, shortnames=TRUE, alignVariables=FALSE)
##' @param x either a charachter string containing the whole url for the request or a named list 
##' containing the parameter names and values (see details)
##' @param base.url a character string containing the basic url for the api
##' @param shortnames logical, indicating whether the resulting tables (data frames) should have 
##' short variable names (default is FALSE, variable names contain nesting hierarchy)
##' @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
##' @return a list of data-frames, containing the returned data in a flat representation
##' @import stringr
##' @import XML
##' @import igraph
##' @import RCurl
##' @importFrom jsonlite fromJSON validate
##' @import plyr
##' @import XML2R
##' @import httr
##' @import mime
##' @import yaml
##' @import methods
##' @export
##' @examples
##' \dontrun{apidata <- getTabularData(x) }


getTabularData <-
      function(x, base.url, shortnames=TRUE, alignVariables=FALSE) {
            stopifnot( (is.list(x) | is.character(x)))
            
            if (is.list(x)) {
                  # parameter/values as list? proceed with apiDatalight...
                  
                  reqfun <- apiRequestFunction(x=x, base.url=base.url)
                  req <- reqfun()
                  data <- apiDatalight(req, alignVariables)
                  
                  # cosmetics
                  if (shortnames==TRUE) {
                        data <- lapply(data, onlyLeafnames)
                  }
                  if (length(data)==1 & is.data.frame(data[[1]])) {
                        data <- data[[1]]
                  }
                  
                  return(data)
                  
            } else { # url as input? simple procedure with less information in api-objects
                  
                  req <- url2apirequest(x)
                  data <- apiDatalight(req, alignVariables)
                  
                  # cosmetics
                  if (shortnames==TRUE) {
                        data <- lapply(data, onlyLeafnames)
                  }
                  
                  if (length(data)==1 & is.data.frame(data[[1]])) {
                        data <- data[[1]]
                  }
                  
                  return(data)
            }
      }
    