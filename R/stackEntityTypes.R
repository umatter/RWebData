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