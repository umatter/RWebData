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