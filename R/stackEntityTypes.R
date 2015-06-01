## Stack entity types if possible
## 
## A function that stacks the entities types together in one data frame (if possible)
## @usage stackEntityTypes(x)
## @param x a nested list containing data frames
## @return a list containing data frames
## @examples
## url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/UN_DEN/AUS+CAN+FRA+DEU+NZL+GBR+USA+OECD/OECD?startTime=1960&endTime=2012"
## data <- apiDatalight(url)
## summary(data)
## data2 <- stackEntityTypes(data)
## summary(data2)

stackEntityTypes <-
      function(x) {
            stopifnot(is.list(x))
            
            entityTypeNames <- str_after(x=names(x), pattern=".", after=FALSE)
            duplicatedEntityTypesNames <- unique(entityTypeNames[duplicated(entityTypeNames)])
            
            if (length(duplicatedEntityTypesNames)>0) { # could any types at all be combined?
                  names(x) <- entityTypeNames
                  
                  for (i in duplicatedEntityTypesNames) {
                        entityTypes.i <- x[entityTypeNames==i]
                        stackedEntityTypes.i <- try(do.call("rbind", entityTypes.i)) # current implementation is brute force, however this only occurs in special cases
                        
                        if (!class(stackedEntityTypes.i)=="try-error") { # stacking worked?
                              entityTypesIndex <- which(entityTypeNames==i)
                              firstEntry <- entityTypesIndex[1]
                              rest <- entityTypesIndex[-1]
                              x[[firstEntry]] <- stackedEntityTypes.i
                              x <- x[-rest]
                             
                        }
                  }
            }
            
            return(x)
      }