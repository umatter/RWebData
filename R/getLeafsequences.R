# getLeafsequences 
# detects repeated leaf sequences and returns x split into those repeated leaf sequences
# x a named character vector representing tree structured data
# 

getLeafsequences <-
  function(x) {
    stopifnot(is.character(x))
    
    xn <- names(x)
    #xn <- getParentname(xn)
    #seqs <- repseqSplit2(xn)
    seqs <- repseqSplit3(xn)
    
    lseq.list <- lapply(seqs, FUN=function(i){
      
      x[is.element(el=xn, set=i)]
      
    })
    
    return(lseq.list)
   
  }
