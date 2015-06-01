##' Download and transform data from a web API
##' 
##' A high-level function that automates the querying and extraction of data for multiple apirequest objects.
##' @usage apiDownload(x, chunksize=50, pause=0, backupfile="apiDL.list.Rdata", 
##' shortnames=FALSE, progress="bar", ...)
##' @param x a list of apirequest objects
##' @param chunksize numeric, the size (number of requests) that should be processed in one batch (default is 50)
##' @param pause numeric, the number of seconds the download process should be paused after each request (default is 0)
##' @param backupfile character string with the path/name of the backup-file where the data should 
##' be saved batchwise during the download process.
##' @param shortnames logical, indicating whether the resulting tables (data frames) should have 
##' short variable names (default is FALSE, variable names contain nesting hierarchy)
##' @param progress, either character "bar" (indicates progress with a progress-bar), "text" 
##' (textual indication of progress)
##' @param ... currently only one parameter (simplify) passed down to the mapping algorithm if 
##' simplify is TRUE, the document tree is made simpler if possible (by removing unnecessary nodes)
##' @return either one  data frame or a list containing several data.frames into which the tree 
##' structured web-data has been transformed.
##' @details The core of the function is based on mapiDatalight(). However, it is specifically 
##' designed for the processing of various requests and a save download of the data.
##' the requests are split up in batches and saved batchwise on disk during the download process. 
##' @export
##' @examples
##' \dontrun{apidata <- apiDownload(x)}


apiDownload <-
function(x, chunksize=50, pause=0, backupfile="apiDL.list.Rdata",
         shortnames=FALSE, progress="bar", ... ) {
      
      stopifnot((unlist(lapply(x,is.apirequest))|is.character(x)))
      
      if (all(is.character(x))) {x <- lapply(x,url2apirequest)}
      
      # to be extended/reconsidered:
      # check whether requests are all for the same API method
      # this could later on be changed into a warning and the function extended to handling
      # requests for different API methods separately
      #     servers <- unique(unlist(lapply(x, FUN=function(i){i@server})))
      #     if (length(servers)>1) stop("x contains apirequest-objects for more than one request-method.")
      
      response.list <- saveDL(request.function=mapiDatalight,
                              request.id=x,
                              chunksize=chunksize,
                              pause=pause,
                              backupfile=backupfile,
                              progress=progress,
                              ...)
      
      response.list2 <- unlist(response.list, recursive=FALSE)
      
      if (length(response.list2)==length(x)) {# only one df per request as response? simply rbind all dfs
            
            response.list <- redlist(response.list)
            if(is.list(response.list[[1]])) response.list <- unlist(response.list, recursive=FALSE)
            response <- dfList(response.list)
            
            } else { # several dfs per request as response? rbind dfs with same name separately
                  dfnames <- names(response.list2)
                  udfnames <- unique(dfnames)
                  n <- length(response.list2)
                  response <- list()
                  
                  for (i in udfnames) {
                        ldfs <- response.list2[dfnames %in% i]
                        i.response <- dfList(ldfs)
                        
                        response[[i]] <- i.response
                  }
            }
      
      # cosmetics...
      if (shortnames==TRUE) {response <- lapply(response, onlyLeafnames)}
      
      
      return(response)

}
