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



## Combine entities (as data.frames) by row
## 
## A function that r-binds or merges entities of the same type 
## @usage rbindEntities(x)
## @param x a list with entities of the same type
## @return a list containing data frames
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## x <- extractEntityTypes(x)
## entityTypes <- x$entitytypes
## currentEntities <- entityTypes[[1]]
## rbindEntities(currentEntities)


rbindEntities <- 
      function(x){
            entitiesSeparated <- entityCheck(x) # depending on results in unit test, make this test and the parts that depend on it optional
            numberOfSeparatedEntities <- length(entitiesSeparated)
            
            if (length(entitiesSeparated)==0) { # this is true if x is only one entity. in this case no loop is needed, directly transform to data frame..
                                                # This should be improved later on with a function that explicitly checks whether x is only one entity and not a list of several entities!
                  currentEntity <- x
                  
                  # make sure, that only leaf-elements' names are used as variable names!
                  # NOTE: this is clearly distinct from old RWebAPI approach. Consider later
                  # an option that would keep the whole tree structure in the name.
                  currentVariableNames <- unlist(lapply(currentEntity, names), use.names=FALSE)
                  emptyNames <- currentVariableNames == ""
                  if (any(emptyNames)) {
                        numberOfVariables <- length(currentVariableNames)
                        # pseudoNames <- rep("Var", numberOfVariables) # NOTE: reconsider this after unit test
                        pseudoNames <- paste0("Var_", 1:numberOfVariables)
                        currentVariableNames[emptyNames] <- pseudoNames[emptyNames]
                  }
                  
                  row.j <- data.frame(t(unlist(currentEntity, use.names=FALSE )), stringsAsFactors=FALSE)
                  if (length(currentVariableNames) == length(row.j)) {
                        names(row.j) <- currentVariableNames
                  } else {
                        row.j <- data.frame(t(unlist(currentEntity, use.names=TRUE )), stringsAsFactors=FALSE)
                  }
                  
                  stackedEntities <- row.j
            
            } else { # x consists of several entities, loop through x, and bind entities as rows in a df
                  rows <- list()
                  for (j in 1:length(x)) {
                        currentEntity <- x[j];
                        
                        # make sure, that only leaf-elements' names are used as variable names!
                        # NOTE: this is clearly distinct from old RWebAPI approach. Consider later
                        # an option that would keep the whole tree structure in the name.
                        currentVariableNames <- unlist(lapply(currentEntity, names), use.names=FALSE)
                        emptyNames <- currentVariableNames == ""
                        if (any(emptyNames)) {
                              numberOfVariables <- length(currentVariableNames)
                              # pseudoNames <- rep("Var", numberOfVariables) # NOTE: reconsider this after unit test
                              pseudoNames <- paste0("Var_", 1:numberOfVariables)
                              currentVariableNames[emptyNames] <- pseudoNames[emptyNames]
                        }
                        
                        row.j <- data.frame(t(unlist(currentEntity, use.names=FALSE )), stringsAsFactors=FALSE)
                        if (length(currentVariableNames) == length(row.j)) {
                              names(row.j) <- currentVariableNames
                        } else {
                              row.j <- data.frame(t(unlist(currentEntity, use.names=TRUE )), stringsAsFactors=FALSE)
                        }
                        rows[[j]] <- row.j
                  }
                  
                  if (numberOfSeparatedEntities==1 | is.null(entitiesSeparated)) {
                        
                        stackedEntities <- rbind.fill(rows)
                        
                  } else {
                        
                        stackedEntitiesSep <- list()
                        
                        for (i in 1:numberOfSeparatedEntities) {
                              
                              # a) cbind entitiy-specific 'metadata' depending on certain conditions:
                              # NOTE: depending on unit tests results this might not be advisable as a default option!
                              
                              # condition: all variable names differ
                              sepRows <- rows[entitiesSeparated[[i]]]
                              allVariables <- names(unlist(sepRows))
                              allDiffer <- length(allVariables) == length(unique(allVariables))
                              
                              if (allDiffer) {
                                    sepRows <- do.call("cbind", sepRows)
                              }
                              
                              stackedEntitiesSep[[i]] <- rbind.fill(sepRows)
                        }
                        
                        stackedEntities <- try(do.call("merge", stackedEntitiesSep), silent = TRUE)
                        if (class(stackedEntities)[1]=="try-error") {
                              stackedEntitiesSep <- lapply(stackedEntitiesSep, unique)
                              stackedEntitiesSep[sapply(stackedEntitiesSep, is.null)] <- NULL
                              stackedEntities <- do.call("cbind", stackedEntitiesSep)
                        }
                        
                  }
            
            }
            
            return(stackedEntities)
      }

