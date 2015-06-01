## Internal function to detect entity types as part of an object
## 
## The function checks whether the hightest nesting level of a nested list (potentially a part of a bigger nested list) contains 
## a structure that is in line with what we consider in this context as an entity type (according to several conditions that must be met)
## @usage entityTypeCheckWide(x)
## @param x a nested list
## @return logical vector of length(x), all elements that contain a part of one entity type are set to TRUE
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## entityTypeCheckWide(x)


entityTypeCheckWide <- 
      function(x) {
            
            isRecursiveElement <- unlist(lapply(x, is.recursive))
            recursiveElementsOnly <-  x[isRecursiveElement]
            isEntityType <- entityTypeCheck(recursiveElementsOnly)
            
            if (isEntityType) {
                  
                  return(isRecursiveElement) 
            } else {
                  return(rep(FALSE, length(x)))
            }
 
      }