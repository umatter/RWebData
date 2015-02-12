# 
# ' Flattens a nested list representing tree structured data
# ' 
# ' Flattens a nested list representing tree structured data
# ' @usage flattenTree(x, collapse.values=TRUE, collapse="_")
# ' @param x a nested list representing tree structured data (XML/JSON)
# ' @param collapse.values logical, indicating whether leaf-values should be collapsed if more than one string in one leaf (default: TRUE)
# ' @param collapse a character string with the symbol used to collapse several strings in a leaf value (only needed if collapse.values=TRUE), default: "_"
# ' @return a named character vector 
# ' @examples
# ' # First, make sure the necessary API key is saved in your R session:
# ' # (This example is based on the Project Vote Smart API [PVS API])
# ' saveAPIkey(key.var="pvs", key="YOUR-KEY-HERE")
# ' # first create a request function:
# ' pvsmeasure <- "http://api.votesmart.org/Measure.getMeasure?"}
# ' measureparameters <- data.frame(parameter="measureId", value=NA)}
# ' getMeasureRequest <- apiRequestFunction(x=measureparameters, base.url=pvsmeasure,key.param="key",key.object="pvs")
# ' # get some data from the PVS API...
# ' mr <- getMeasureRequest(measureId=1632) # create a request object
# ' \dontrun{x <- apiGET(mr)}
# ' \dontrun{x.list <- content2list(x)}
# ' \dontrun{x.flat <- flattenTree(x.list)}


flattenTree <-
function(x, collapse.values=TRUE, collapse="_") {
    
    stopifnot(is.list(x))
    
    # collapse several leaf-value strings to one
    # deals with attributes-xml
    x <- rapply(x, f=paste, how="replace", collapse="_")
    
    # flatten tree structure/nested list
    x.flat <- unlist(x)
    
    if (is.null(x.flat)){
          return(x.flat)
    }
    
    if (!is.null(names(x.flat))) {
          # clean names
          names(x.flat) <- gsub(pattern="-", replacement="", x=names(x.flat), fixed=TRUE)
    }

    return(x.flat)
    
  }
