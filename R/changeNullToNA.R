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



## Replace all NULLs in a (nested) list with NAs
## 
## The function recursively replaces each NULL value contained in an element of a potentially nested list 
## with NA.
## @usage changeNullToNA(x)
## @param x a (nested) list
## @return a list
## @examples
## exampleList <- list(list(list(b=NULL)), list(list(list(a=NULL))))
## changeNullToNA(exampleList)


changeNullToNA <- 
      function(x){ 
            
            if (is.null(x)) {
                  x <- NA
            }
            if( is.list(x) ){
                  x <- lapply( x, changeNullToNA)
            }
            
            return(x)
      }