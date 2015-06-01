## Check consistency of entities
## 
## A function that checks wether entities within the same type actually 
## belong together as rows or should rather be merged.
## @usage entityCheck(x)
## @param x a list containing entities (rows)
## @return a list with indeces of entities that belong together
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## x <- extractEntityTypes(x)
## entityTypes <- x$entitytypes
## currentEntities <- entityTypes[[1]]
## entityCheck(currentEntities)
## 

# entityCheck: 
# x a list containing entities (rows)
entityCheck <-
      function(x) {
            
            if (!is.list(x)) {
                  return(NULL)
            }
            
            numberOfEntities <- length(x)
            variableNames <- lapply(x, names)
#             if (length(variableNames)==0){
#                   variableNames <- 1:numberOfEntities
#             }
            variablesInCommon <- list()            
            
            for (i in 1:numberOfEntities) {
                  
                  currentVariables <- variableNames[[i]]
                  namesmatch <- lapply(variableNames, FUN=function(y) {
                        
                        any(y %in% currentVariables)
                  }
                  )
                  
                  entitiesIndex <- which(unlist(namesmatch))
                  if (length(entitiesIndex)>0) {
                        variablesInCommon[[i]] <- entitiesIndex
                  }
                        
            }
            
            return(unique(variablesInCommon))
      }