## Extract data in a tabular form (data-frames) from a tree-structure (nested list based on parsed web data)
## 
## Tree-data mapping algorithm: first extract entity types then stack entities within each type and save
## them in one data-frame per type (and one for metadata) 
## @usage listToDataFrame(x, alignVariables=FALSE)
## @param x a nested list (based on parsed web data)
## @param alignVariables logical, indicating whether variables should be rearranged (if input data was malformed).
## Default is FALSE.
## @return a list with data frames
## @export
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## listToDataFrame(x)
## 

listToDataFrame <-
      function(x, alignVariables) {
            stopifnot(is.list(x))
            
            # a) make sure, data is in proper form
            x <- changeNullToNA(x) # this is a temporary solution (creates quite an overhead), better:solution change parser to parse empty values as NAs
            
            # b) data mapping algorithm
            entityTypes <- extractEntityTypes(x)
            listOfDataFrames <- stackEntities(entityTypes)
            
            # c) cosmetics
            listOfDataFrames <- stackEntityTypes(listOfDataFrames)
            
            if (alignVariables) {
                  listOfDataFrames <- alignAllVariables(listOfDataFrames)
            }
            
            return(listOfDataFrames)
      }

