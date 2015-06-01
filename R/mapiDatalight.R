## Query data from an API
## 
##  A vectorized version of apiDatalight(), wrapped around apiDatalight()
## @usage mapiDatalight(x)
## @param x list of apirequest objects or urls
## @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
## @return a (nested) list containing the returned data in a flat representation
## @export
## @examples
## \dontrun{apidata <- mapiDatalight(list(mr,mr2)) # only works with a proper PVS API key}


mapiDatalight <-
      function(x, alignVariables=FALSE, ...) {
            resp.list <- lapply(x, apiDatalight, alignVariables, ...)
            
            return(resp.list)
      }
