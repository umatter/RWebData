##' Extract the data from a JSON document as one (or several) data frame(s)
##' 
##' Given JSON document, this function maps the potentially nested JSON data to one or several 
##' data frames.
##' The data extraction algorithm applied in this function is based on the assumption that the 
##' JSON document describes either one or several entity types containing a set of observations 
##' described by different variables.   
##' @usage JSONtoDataFrame(x, alignVariables=FALSE)
##' @param x either a string containing JSON or the name of a file containing the JSON
##' @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
##' @return one data frame or a list of several data frames
##' @export
##' @examples
##' JSON.ex <- system.file("exdata", "microcapital.json", package = "RWebData")
##' JSONtoDataFrame(JSON.ex, alignVariables=FALSE)



JSONtoDataFrame <-
      function(x, alignVariables=FALSE) {
            rx <- try(fromJSON(x, simplifyVector=FALSE), silent=TRUE)  # stable in case of missing values 
            if (is.character(rx)) {
                  if (validate(rx)) {
                        rx <- try(fromJSON(rx, simplifyVector=FALSE), silent=TRUE)  # special case, if json is theoretically valid but contains too many escapes 
                  }
            }
            # jsonlite is, however, sensitive to not very well formatted json (throws lexical errors)
            # see, e.g., http://api.opencongress.org/people?last_name=Kerry&format=json
            # therefore also apply other existing/extended parsers to make this function most robust.
            if (class(rx)=="try-error"){
                  rx <- try(RJSONIO::fromJSON(x, nullValue=NA, simplify=FALSE), silent=TRUE)
                  if (class(rx)=="try-error") {
                        stop("json parsing error")
                        #     rx <- .fromJSON_R_NA(body) # based and dependend on rjson
                  }  
      
            }
            
            x.df <- listToDataFrame(rx, alignVariables)
            
            return(x.df)
      }
