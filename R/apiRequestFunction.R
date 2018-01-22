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


## Writes a function to generate API requests
## 
## An extended version of generateURLFunction(). The generated function returns an object of class 
## "apirequest".
## @usage apiRequestFunction(x, base.url, multiparam=NULL, key.param=NA,
##  key.object=NA, vectorizeIt=FALSE)
## @param x a two-column data frame or a named list containing the parameter names and values 
## (see details).
## @param base.url a character string containing the basic url for the api.
## @param multiparam a character string with the name of a parameter that can take multiple input 
## values in the resulting function  (defaults to NULL).
## @param key.param a character string containing the api key parameter required in the url.
## @param key.object the name of the object the api.key is saved in (as character).
## @param vectorizeIt logical, indicating whether the resulting function should contain an implicit
## loop over parameters with no default value (i.e., allows vectors as inputs). Default is FALSE. 
## TRUE only works if x is a data frame.      
## @return an object of class apirequest.
## @details x (if a data frame) should contain the parameter names in the first column and 
## respective default-values in the second column (both as character strings). Parameters that 
## have no default value have NAs in the second column.The function attempts to get the api-key 
## from the environment "apikeys" (therefore has to be defined there before; see saveAPIkey()).
## if x is a list, the parametern names values are provided like in a function call, i.e., 
## list(param1=value.param1, param2=...). 
## The option of providing x as a list ist basically implemented for getTabularData() 
## (direct individual requests to an API, no vectorization).
## @export
## @examples
## # First, make sure the necessary API key is saved in your R session:
## # (This example is based on the Project Vote Smart API [PVS API])
## saveAPIkey(key.var="pvs", key="YOUR-KEY-HERE")
## pvsmeasure <- "http://api.votesmart.org/Measure.getMeasure?"
## measureparameters <- data.frame(parameter="measureId", value=NA)
## \dontrun{getMeasureRequest <- apiRequestFunction(x=measureparameters, base.url=pvsmeasure,
## key.param="key",key.object="pvs")}
## 




apiRequestFunction <-
      function(x, base.url, multiparam=NULL, key.param=NA, key.object=NA, vectorizeIt=FALSE){
            stopifnot((is.null(multiparam) | is.character(multiparam)), length(multiparam)<=1, (is.list(x) | is.data.frame(x)))
            
            if (!is.data.frame(x) & is.list(x) & !is.null(multiparam) ) {
                  stop("multiparam can only be used in combination with x as a data frame.\nx is not a data frame.")
            }
            if (!is.data.frame(x) & is.list(x) & vectorizeIt==TRUE ) {
                  stop("vectorizeIt=TRUE can only be used in combination with x as a data frame.\nx is not a data frame.")
            }
            
            
            f <- function(){}  # set basic function to be extended...
            
            # I) STANDARD INPUT/PROCEDURE: X IS A DATA FRAME:
            if (is.data.frame(x)) {# standard input to build apiRequestFunction?
                  
                  # extract normal parameters, add multiparam if necessary
                  plist <- as.paramlist(x)
                  
                  if (is.character(multiparam)) { # add repeated param to paramlist
                        
                        multivalues <- structure(replicate(length(multiparam), formals(function(x){})$x ), names=multiparam)
                        plist$parameters <- c(multivalues, plist$parameters)
                  }
                  
                  formals(f) <- plist$parameters 
                  
                  # write string of basic url (server) in order to be pased on in the resulting function
                  server <- base.url
                  serverstring <-  paste("server <- '", server, "'", sep="")
                  e.server <- parse(text=serverstring)
                  
                  # extract parameters as strings, construct expressions for body
                  pstrings <- plist$p.strings
                  pstrings2 <- paste("'&", pstrings, "=',", sep="")
                  symbolproblem <- grepl(pattern="-", x=pstrings, fixed=TRUE) # NEW
                  pstrings[symbolproblem] <- paste0("`", pstrings[symbolproblem],"`") # NEW deals with "-" in strings
                  pstrings.url <- paste(pstrings2, pstrings, collapse=", ")
                  pstrings.url <- sub(pattern="&", replacement="", x=pstrings.url, fixed=TRUE) # remove first &
                  pstrings.url <- paste("params <- paste0(", pstrings.url, ")", sep="")
                  
                  # write adding of basic url and api-key if available
                  if (is.na(key.param) & is.na(key.object)) {
                        urlstring <- paste("'", base.url, "', params ", sep="") 
                        keystring <- ""    
                  } else {
                        urlstring <- paste("'", base.url, "', params, ", "'&", key.param, "=', api.key", sep="") 
                        keystring <- paste0("api.key <- get('", key.object,  "', pos='apikeys')")
                  }
                  
                  urlpaste <- paste0("url <- paste0(", urlstring, ")")
                  
                  e.params <- parse(text=pstrings.url)
                  e.key <- parse(text=keystring)
                  e.url <- parse(text=urlpaste)
                  
                  # add request parameters (in order to keep track of what request caused an error) 
                  if (length(plist$nodefaults)>0) {
                        
                        ndstrings <- plist$nodefaults
                        symbprob <- grepl(pattern="-", x=ndstrings, fixed=TRUE) # NEW
                        ndstrings[symbprob] <- paste0("`", ndstrings[symbprob],"`") # NEW deals with "-" in strings
                        
                        ndstrings.arg <- paste(ndstrings, collapse=", ")
                        ndstrings.df <- paste("ndrequest <- data.frame(", ndstrings.arg, ")", sep="")
                        ndstrings2 <- paste0("'", ndstrings, "'", collapse=", ")
                        ndstrings.dfnames <- paste0("names(ndrequest) <- c(", ndstrings2,")")
                        
                        e.paramdf <- parse(text=ndstrings.df)
                        e.namesdf <- parse(text=ndstrings.dfnames)
                        
                  } else {
                        e.paramdf <- expression( )
                        e.namesdf <- expression( ndrequest <- data.frame())
                  }
                  
                  e.args <- expression(rarg <- as.data.frame(t(as.list(environment())), stringsAsFactors=FALSE))
                  e.object <- expression(apirequest <- new("apirequest",
                                                           URL=url,
                                                           request.arguments=rarg,
                                                           nodefault.parameters=ndrequest,
                                                           server=server ))
                  e.return <- expression(return(apirequest))
                  
                  e <- c(e.args, e.params, e.key, e.url, e.paramdf, e.namesdf, e.server, e.object, e.return)
                  
                  
                  if (is.character(multiparam)) { # add multiparam
                        
                        multistring <- paste0("for (i in 1:length(", multiparam, ")) { mparams <- paste0(mparams, '&",
                                              multiparam,"=', ", multiparam, "[i])}")
                        
                        e.multi1 <- expression(mparams <- c())
                        e.multi2 <- parse(text=multistring)
                        e.multi3 <- expression(mparams <- sub(pattern="&", replacement="", x=mparams, fixed=TRUE)) # remove first &
                        e.multi4 <- expression(params <- paste(mparams, params, sep="&"))
                        
                        e <- c(e.args,
                               e.params,
                               e.multi1,
                               e.multi2,
                               e.multi3,
                               e.multi4,
                               e.key,
                               e.url,
                               e.paramdf,
                               e.namesdf,
                               e.server,
                               e.object,
                               e.return)
                  } 
                  
                  
                  # define body of the function
                  body(f) <- as.call(c(as.name("{"),e))
                  
                  if (vectorizeIt==TRUE & length(plist$nodefaults)>0 ){ # should function contain implicit loop over no-default parameters?
                        
                        f <- vectorizeIt(f)
                  }
                  
                  return(f)
                  
                  # ALTERNATIVE II) X IS NOT A DATA FRAME --> SIMPLIFIED PROCEDURE (NO MULTIPARAM ALLOWED)
                  
                  } else {
                        
                        formals(f) <- x
                        
                        # write string of basic url (server) in order to be pased on in the resulting function
                        server <- base.url
                        serverstring <-  paste("server <- '", server, "'", sep="")
                        e.server <- parse(text=serverstring)
                        
                        # extract parameters as strings, construct expressions for body
                        pstrings <- names(x)
                        pstrings2 <- paste("'&", pstrings, "=',", sep="")
                        pstrings.url <- paste(pstrings2, pstrings, collapse=", ")
                        pstrings.url <- sub(pattern="&", replacement="", x=pstrings.url, fixed=TRUE) # remove first &
                        pstrings.url <- paste("params <- paste0(", pstrings.url, ")", sep="")
                        
                        # write adding of basic url and api-key
                        if (is.na(key.param) & is.na(key.object)){
                              urlstring <- paste("'", base.url, "', params ", sep="") 
                              keystring <- ""
                              } else {
                                    urlstring <- paste("'", base.url, "', params, ", "'&", key.param, "=', api.key", sep="")
                                    keystring <- paste0("api.key <- get('", key.object,  "', pos='apikeys')")
                              }
                        
                        urlpaste <- paste0("url <- paste0(", urlstring, ")")
                        
                        e.params <- parse(text=pstrings.url)
                        e.key <- parse(text=keystring)
                        e.url <- parse(text=urlpaste)
                        
                        # add request parameters (in order to keep track of what request caused an error)
                        # nodefault parameters not allowed hence only...
                        
                        e.paramdf <- expression( )
                        e.namesdf <- expression( ndrequest <- data.frame() )
                        
                        e.args <- expression(rarg <- as.data.frame(t(as.list(environment())), stringsAsFactors=FALSE))
                        e.object <- expression(apirequest <- new("apirequest",
                                                                 URL=url,
                                                                 request.arguments=rarg,
                                                                 nodefault.parameters=ndrequest,
                                                                 server=server ))
                        e.return <- expression(return(apirequest))
                        
                        e <- c(e.args, e.params, e.key, e.url, e.paramdf, e.namesdf, e.server, e.object, e.return)
                        
                        # define body of the function
                        body(f) <- as.call(c(as.name("{"),e))
                        
                        return(f)
                  }
      }
    
