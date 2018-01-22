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



## Extract entity types from a nested list based on parsed web data
## 
## Turns a nested data structure (list or data frame) into a flat list consisting of one list with entity types
## and one list with metadata.
## @usage extractEntityTypes(x)
## @param x a nested list
## @return a list 
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## extractEntityTypes(x)


extractEntityTypes <- 
      function(x) {
            stopifnot(is.list(x))
            
            depositList <- list()
            resultList <- list()
            
            # First, check if the whole object is itself one entity type and return it directly if that is the case
            isEntityType <- entityTypeCheck(x)
            if (isEntityType) {
                  
                  Type <- unique(names(x))
                  if (is.null(Type) | length(Type)>1){
                        Type <- "entityType"  # if there is no name, call it "entityType" (should be well considered later)
                  }
                  resultList[[Type]] <- x
                  
                  return(list(entitytypes=resultList, meta_.._data=depositList))
                  
            }
            
            # Second, check if the object contains a part at the highest nesting level that would be itself considered an entity type
            # if so, extract that part of x, add it as type to the result, and continue with remainder of x
            isEntityTypePart <- entityTypeCheckWide(x)
            if (any(isEntityTypePart)) {
                  
                  x_entityType <- x[isEntityTypePart]
                  
                  Type <- unique(names(x_entityType))
                  if (is.null(Type) | length(Type)>1){
                        Type <- "entityType"  # if there is no name, call it "entityType" (should be well considered later)
                  }
                  resultList[[Type]] <- x_entityType
                  
                  # remainder of x:
                  x <- x[!isEntityTypePart]
            }
                  
            
            # Third, the actual processing:
            # Object itself or a part of it are not an entity type. thus iterate through it to extract the entit types and metadata
            keys<-numberedNames(x)
            if (is.null(keys)) {
                  # if we don't have names, we use 1,2,3... as keys instead
                  keys <- seq(1, length(x))
                  isNumberedKey <- TRUE
            } else {
                  isNumberedKey <- FALSE
            }
            
            # checks to make function more robust
            numberOfListElements <- length(x)
            if (length(numberOfListElements)==0) { # make sure that loop-indexing is not problematic (i.e. 1:logical(0))
                  stop("there is a problem...")
            }
            
            if (numberOfListElements == 0) { # if input is a empty list, replace with NA and return it
                  
                  currentKey <- keys[1]
                  currentValue <- NA
                  resultList[[currentKey]] <- currentValue
                  
                  return(list(entitytypes=resultList, meta_.._data=depositList))
            }
            
            # actual processing of data
            for (i in 1:numberOfListElements) { 
                  currentKey <- keys[i];
                  currentValue <- x[[i]];
                  
                  # if the current item is a non-empty list, dig deeper into the nesting structure
                  stillList <- is.list(currentValue)
                  notEmpty <- length(currentValue) > 0
                  
                  if (stillList & notEmpty) {
                        # check if all the conditions to consider the current value as entity type are met
                        isEntityType <- entityTypeCheck(currentValue)
                        
                        if (isEntityType) {
                              
                              subElementNames <- names(currentValue)
                              uniqueSubElementName <- unique(subElementNames)
                              
                              # find the name for the entitytype and add entitytype to results
                              if (is.null(currentKey) & is.null(uniqueSubElementName)){
                                    resultList[["entityType"]] <- currentValue
                                    
                              } else if (is.null(currentKey)) {
                                    currentKey <- uniqueSubElementName
                                    resultList <- c(resultList, currentValue)
                                    
                              } else {
                                    resultList[[currentKey]] <- currentValue
                              }
                              
                        } else {
                              
                              # recursive call to this very function
                              children <- extractEntityTypes(currentValue)
                              # append the children to the resultList and deposit to depositlist
                              deposit <- children$meta_.._data
                              if (length(deposit)>0){
                                    depositList <- c(depositList, deposit)
                                    
                              }
                              
                              entitytypes <- children$entitytypes
                              if (length(entitytypes)>0){
                                    resultList <- c(resultList, entitytypes)
                              }
                        }
                        
                  } else {
                        
                        # it's a leaf node (i.e. just a value), so we simply add that to the deposit (singly occuring leaf --> metadata)
                        # and remove it from x (removal is crucial in order to detect entity types on the same level)
                        depositList[[currentKey]] <- currentValue
                  }
            } 

            return(list(entitytypes=resultList, meta_.._data=depositList))
      }

