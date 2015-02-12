# 
# ' Extract the body of an apiresp object as nested list
# ' 
# ' Transforms the HTTP response body stored in an apiresp object (either XML or JSON) to a nested list
# ' @usage content2list(x)
# ' @param x an apiresp or apidata object 
# ' @return a nested list
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




content2list <-
function(x)  { 
    stopifnot(is.apiresp(x) | is.apidata(x))
    
    if (is.apiresp(x)) {
    
    body <- x@body
    type.json <- grepl(pattern="json", x@type)
    type.xml <- grepl(pattern="xml", x@type)
    
    if (!(type.xml | type.json )) { # type not found? guess type based on url
      mime <- guess_type(as.character(unlist(x@request.arguments)))
      type.json <- grepl(pattern="json", mime)
      type.xml <- grepl(pattern="xml", mime)
      if (!(type.xml | type.json )) {
        type.json <- grepl(pattern="json", as.character(unlist(x@request.arguments))) # temporary
        type.xml <- grepl(pattern="xml", as.character(unlist(x@request.arguments))) # temporary --> find better solution
      }
    }
    
    } else {
      body <- x@raw.data
      type.json <- grepl(pattern="json", x@raw.type)
      type.xml <- grepl(pattern="xml", x@raw.type)
      
      if (!(type.xml | type.json )) { # type not found? guess type based on url
        mime <- guess_type(as.character(unlist(x@request.arguments)))
        type.json <- grepl(pattern="json", mime)
        type.xml <- grepl(pattern="xml", mime)
        if (!(type.xml | type.json )) {
        type.json <- grepl(pattern="json", as.character(unlist(x@request.arguments))) # temporary
        type.xml <- grepl(pattern="xml", as.character(unlist(x@request.arguments))) # temporary --> find better solution
        }
      }
 
    }
      
      
    if (!(type.xml | type.json )) stop("Media-type is neither xml nor json")
    
    if (type.json) {
      rx <- jsonlite::fromJSON(body) # stable in case of missing values (but only used if directly 
                                     # results in data frame)
      if (!is.data.frame(rx)){
        rx <- try(RJSONIO::fromJSON(body, nullValue=NA, simplify=FALSE), silent=TRUE)
        if (class(rx)=="try-error") {rx <- .fromJSON_R_NA(body)} # based and dependend on rjson
      }
        
    } else {
      if (type.xml) rx <- xml2list(body) # xml2list instead of XML::xmlToList, depends on XML
    }
       
    
    return(rx)
  }
