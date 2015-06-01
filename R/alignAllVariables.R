## Vectorized version of alignVariables()
## 
## A function that applies alignVariables() to all data frames in a list
## if the input to listToDataFrame was malformed (missing variable names).
## @usage alignVariables(x)
## @param x a list with data frames (listToDataFrame-output)
## @return a list with data frames
## @details The function should be applied if the order of variables is allways the same
## but different entities do not have the same variable names.
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## dfs <- listToDataFrame(x)
## alignAllVariables(dfs)
## 

alignAllVariables <- 
      function(x) {
            
            x2 <- lapply(x, alignVariables)
            
            return(x2)
      }