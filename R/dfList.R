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



## Harmonize and combine different data frames in a list
## 
##  A function that harmonizes and combines different data frames in a list to one data frame.
## @usage dfList(l)
## @param l a list containing several data frames
## @return A data frame combining all individual data frames from the input list
## @examples
## a <- data.frame(a=c(1,2,4),b=c(5,5,6))
## b <- data.frame(a=c(3,4,5,6), b=c(8,8,5,4), c=c(9,4,2,1))
## x <- list(a,b)
## dfList(x)


dfList <-
      function(l) {
            # coerce df columns to character, clean data
            l <- lapply(l, FUN = function(i){
                  
                  if (!is.null(names(i))){
                        i <- i[,!is.na(names(i))]
                  }
                  
                  for (j in names(i)) {
                        i[,j] <- as.character(i[,j])
                        i[!is.na(i[,j]) & i[,j]=="list()",j] <- NA # test this
                        i[,j] <- as.character(i[,j])
                  }
                  
                  return(i)
            }
            )
            
            if (length(l)==1) {
                  l2 <- l
                  } else {
                        # first: extract all variable names that come up in the data frames of the list
                        l.names.list <- lapply(l, FUN = function(x) names(x))
                        l.names <- do.call("c",l.names.list )
                        l.names <- unique(l.names)
                        
                        # second: complete and clean each df of the list.
                        l2 <- lapply(l, FUN=function(x) {  # check which variable names from l.names are in x
                              
                              available <- sapply(l.names, FUN= function(i) {
                                    match(i,names(x), nomatch=0)==0
                              }
                              )
                              
                              available <- data.frame(missing=available,name=names(available))
                              # extract missing variable names
                              missing <- as.character(available[which(available$missing==TRUE),"name"])
                              
                              # generate df with missing names (filled with NA)
                              df.missing <- data.frame(matrix(NA, nrow=1, ncol=length(missing)))
                              names(df.missing) <- missing
                              names(df.missing) <- missing
                              
                              # extend existing df x with the df of missing names
                              x <- data.frame(x,df.missing, check.names=FALSE)
                              
                              #order names according to l.names to guarantee "correct rbind"
                              x <- x[,l.names]
                              
                              return(x)
                        }
                        )
                  }
            
            # now the list is ready to be rbind to one dataframe 
            l.df <- do.call("rbind", l2)
            
            return(l.df)
      }
