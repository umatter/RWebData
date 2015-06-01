## Reduce nesting structure
## 
## The function removes unnecessary nesting structure from a nested list (based on tree-structured web data)
## @usage reduceNestingStructure(x)
## @param x a nested list
## @return logical, returns TRUE if nested list is considered an entity type 
## @examples
## a <- list(list(list(list(b=list("value")))))
## c <- list(list(list(list(b=list("value")))))
## x <- list(a,c)

reduceNestingStructure <-
      function(x) {
            stopifnot(is.list(x))
            
            allLengths <- unlist(lapply(x, length))
            if (length(allLengths)>0 ){
                  lengthBelow <- max(allLengths)
            } else {
                  lengthBelow <- 0
            }
            oneElementBelow <- lengthBelow == 1
            #namesBelow <- unique(unlist(lapply(x, names)))
            allListSubBelow <- all(unlist(lapply(x, lapply, is.list)))
            
            while (allListSubBelow & oneElementBelow ) {
                  
                  x <- unlist(x, recursive=FALSE)
                  
                 # currentNames <- names(x)
                  #currentIsList <- is.list(x)
                 allLengths <- unlist(lapply(x, length))
                 if (length(allLengths)>0 ){
                       lengthBelow <- max(allLengths)
                 } else {
                       lengthBelow <- 0
                 }
                 oneElementBelow <- lengthBelow == 1
                 # namesBelow <- unique(unlist(lapply(x, names)))
                  allListSubBelow <- all(unlist(lapply(x, lapply, is.list)))
            }
            
            return(x)
      }