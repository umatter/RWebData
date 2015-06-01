## Align variables in a data frame (post-processing for listToDataFrame-output)
## 
## A function that rearranges variables/variable values
## if the input to listToDataFrame was malformed (missing variable names).
## @usage alignVariables(x)
## @param x a data frame (part of listToDataFrame-output)
## @return a data frame
## @details The function should be applied if the order of variables is allways the same
## but different entities do not have the same variable names.
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## dfs <- listToDataFrame(x)
## dfs[[2]]
## alignVariables(dfs[[2]])

alignVariables <- 
      function(x) {
            
            variables <- names(x)
            isPseudoName <- grepl(pattern="Var_", x=variables)
            pseudoNames <- variables[isPseudoName]
            
            for (i in pseudoNames) {
                  currentColumnNumber <- as.numeric(sub(pattern="Var_", "", i))
                  noRowEntryInColumn <- is.na(x[,currentColumnNumber])
                  hasRowEntryInPseudoVar <- !is.na(x[,i])
                  
                  rowsEntriesToReplace <- noRowEntryInColumn & hasRowEntryInPseudoVar
                  if (any(rowsEntriesToReplace)) {
                        
                        x[rowsEntriesToReplace, currentColumnNumber] <- x[rowsEntriesToReplace, i]
                        x[,i  ] <- NULL
                        
                  }
            }
            
            return(x)
      }