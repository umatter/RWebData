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



## Reduce nesting structure
## 
## The function removes unnecessary nesting structure from a nested list (based on tree-structured web data)
## @usage reduceNestingStructure(x)
## @param x a nested list
## @return logical, returns TRUE if nested list is considered an entity type 
## @examples
## a <- list(list(list(list(b=list("value")))))
## c <- list(list(list(list(b=list("value")))))
## x <- list(a,c)

reduceNestingStructure <-
      function(x) {
            stopifnot(is.list(x))
            
            allLengths <- unlist(lapply(x, length))
            if (length(allLengths)>0 ){
                  lengthBelow <- max(allLengths)
            } else {
                  lengthBelow <- 0
            }
            oneElementBelow <- lengthBelow == 1
            #namesBelow <- unique(unlist(lapply(x, names)))
            allListSubBelow <- all(unlist(lapply(x, lapply, is.list)))
            
            while (allListSubBelow & oneElementBelow ) {
                  
                  x <- unlist(x, recursive=FALSE)
                  
                 # currentNames <- names(x)
                  #currentIsList <- is.list(x)
                 allLengths <- unlist(lapply(x, length))
                 if (length(allLengths)>0 ){
                       lengthBelow <- max(allLengths)
                 } else {
                       lengthBelow <- 0
                 }
                 oneElementBelow <- lengthBelow == 1
                 # namesBelow <- unique(unlist(lapply(x, names)))
                  allListSubBelow <- all(unlist(lapply(x, lapply, is.list)))
            }
            
            return(x)
      }