##
##' Transform tree-structured data into flat representation
##' 
##' Transforms a nested list representing tree structured data into one or several data frames
##' @usage tree2flat(x, id)
##' @param x a (nested list) representing tree structured data from an API response (originally JSON or XML).
##' @param id the id-variable to separate individual observations in x, a character string.
##' @return a list containing one or several data frame(s)
##' @examples
##' # First, make sure the necessary API key is saved in your R session:
##' # (This example is based on the Project Vote Smart API [PVS API])
##' saveAPIkey(key.var="pvs", key="YOUR-KEY-HERE")
##' # first create a request function:
##' pvsmeasure <- "http://api.votesmart.org/Measure.getMeasure?"}
##' measureparameters <- data.frame(parameter="measureId", value=NA)}
##' getMeasureRequest <- apiRequestFunction(x=measureparameters, base.url=pvsmeasure,key.param="key",key.object="pvs")
##' # get some data from the PVS API...
##' mr <- getMeasureRequest(measureId=1632) # create a request object
##' \dontrun{x <- apiGET(mr)}
##' \dontrun{x.list <- content2list(x)}
##' \dontrun{x.df <- tree2flat(x.list, id="measureId")}


tree2flat <-
      function(x, id) {
            stopifnot(is.list(x), (is.character(id) & length(id)==1) )
            
            x.flat <- flattenTree(x)
            vars <- names(x.flat)
            
            # data describing one or several observations?
            id.occurences <- nin(id,vars, exact=FALSE)
            
            if (id.occurences==0) stop(paste("Error: id-variable '",id,"' not found in x"))
            
            
            if (id.occurences == 1) { # data describes one obs, return x.flat
                  names(x.flat) <- numberedNames(x.flat)
                  return(x.flat)
                  
                  } else {
                        # count how often each of the unique variable names shows up in the df
                        
                        uvar <- unique(vars)
                        freqvar <- sapply(uvar, function(x) sum(as.numeric(vars %in% x)))
                        freqvar <- freqvar[freqvar>1]
                        
                        if (length(freqvar)>0) {
                              
                              seqfirst <- names(freqvar[1])
                              seqlast  <- names(freqvar[length(freqvar)])
                              
                              seqbegin <- sapply(c(seqfirst, seqlast), function(x) { which(vars==x)})
                              
                              if (!(length(seqbegin)>0)) {
                                    return("error")
                              }
                              
                              flattable.list <- lapply(1:nrow(seqbegin), FUN=function(i){
                                    start <- seqbegin[i,1]
                                    end <- seqbegin[i,2]
                                    x.flat[,start:end]
                                    }
                                    )
                              
                              flattable.df <- dfList(flattable.list) # (cont. here) optionally also add the variables that only occur once (same for each row!)
                              
                              names(flattable.df) <- numberedNames(flattable.df)
                              return(flattable.df)
                              
                              } else { # only unique occurence of vars --> suggests only one observation described by tree-data
                                    names(x.flat) <- numberedNames(x.flat)
                                    
                                    return(x.flat)
                              }
                  }
      }
