## Stack entities of the same type
## 
## A function that stacks the entities together in a data-frame within each entity-type.
## @usage stackEntities(x)
## @param x a nested list
## @return a list containing data frames
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## entityTypes <- extractEntityTypes(x)
## stackedEntities <- stackEntities(entityTypes)

stackEntities <- 
      function(x) {
            
            # a) Stack entities within entity-types
            entityTypes <- x$entitytypes
            
            # make sure input is proper
            emptyEntries <- lapply(entityTypes,length) == 0
            entityTypes <- entityTypes[!emptyEntries]
            
            if (length(entityTypes)>0) {  # are there some entity types in the data? if yes stack entities
                  
                  Types <- names(entityTypes)
                  if (is.null(Types)) {
                        # if we don't have names, we use 1,2,3... as keys instead
                        Types <- seq(1, length(entityTypes))
                  }
                  
                  entityTypesDataFrames <- list()
                  
                  for (i in 1:length(entityTypes)) {
                        currentEntities <- entityTypes[[i]];
                        currentType <- Types[i]
                        
                        if (!is.null(currentEntities)){
                              entityTypesDataFrames[[currentType]] <- rbindEntities(currentEntities)
                        }
                  }
                  
                  if (is.null(names(entityTypesDataFrames))) {
                        names(entityTypesDataFrames) <- rep("entityType", length(entityTypesDataFrames))
                  }
                  
            } else { # if no entity types present, set to null
                  
                  entityTypesDataFrames <- NULL  
            }
            
            
            # b) convert metadata-entries to one data frame
            metadata <- x$meta_.._data
            if (length(metadata)>0) {
                  metadata.df <- data.frame(t(unlist(metadata)), stringsAsFactors=FALSE)
                  metadata <- list(metadata=metadata.df)
            } else {
                  metadata <- NULL
            }
            
            # format output
            output <- c(metadata, entityTypesDataFrames)
            #output <- entityTypesDataFrames
            
            return(output)
      }

