# find sequences

# x <- c("a", "b", "c", "a", "b", "a","b","c", "d", "a", "b", "c", "a","b","c",
#        "x", "y","x","y","x","y",
#        "q", "s", "t", "q", "s", "q", "t")
# 
# 
# x <- c("I.a", "I.b", "I.c", "I.a", "I.b", "I.a","I.b","I.c", "I.d", "I.a", "I.b", "I.c", "I.a",
# "I.b","I.c","I.a","I.b","I.c","I.e", "I.1.x", "I.1.y","I.1.x","I.1.y","I.1.x","I.1.y","I.1.z",
# "I.1.z2", "II.q", "II.s", "II.t", "II.q", "II.s", "II.q", "II.t")

repseqSplit3 <-
      function(x) {
            seqs <- list()
            deposit <- list()
            lx <- length(x)
            
            while (lx>0) {
                  # I) while the first element i does not occur several times, put i in the deposit
                  seqstart <- x[1]
                  occ <- which(x==seqstart)[-1]
                  
                  while (length(occ)==0) {
                        deposit <- c(deposit, list(seqstart))
                        x <- x[-1]
                        seqstart <- x[1]
                        occ <- which(x==seqstart)[-1]
                  }
                  
                  # II) if the first element i appears several times, take all the vector from the first element i
                  # to its nth occurrence in the vector. this is class j.
                  nth_occ <- max(occ)
                  class.j <- x[1:nth_occ]
                  
                  # III) if something is left in x, extend class j with the following elements in the remaining vector
                  # "rest" up to the last occurrence of class j's elements. 
                  lseq <- length(class.j)
                  if (lseq==lx) {  # class.j contains all x? --> no rest..
                        rest <- NULL
                        } else { 
                              rest <- x[(nth_occ+1):length(x)]
                              ucj.elements <- unique(class.j)
                              occ_ucje <- which(rest %in% ucj.elements)
                              
                              if (length(occ_ucje)>0) {
                                    mocc_ucje <- max(occ_ucje)
                                    restseq <- rest[1:mocc_ucje]
                                    class.j <- c(class.j,restseq)
                                    
                                    if (identical(rest,restseq)) {
                                          rest <- NULL
                                          } else {
                                                extr.rest <- (mocc_ucje+1):length(rest)
                                                rest <- rest[extr.rest]
                                          }
                              }
                        }
                  
                  if (length(rest)!=0) {
                        
                        # IV) while the new first element in rest occurs only once and has the same parent/branch as
                        # the elements in class j, add it to class j. if it occurs only once but does not have the same
                        # parent, add it to the deposit (Do we have to check whether the parent occurs in the remainder too?)
                        
                        parents.classj <- unique(getParentname(class.j))
                        new.seqstart <- rest[1]
                        occrest <- which(rest[-1]==new.seqstart)
                        
                        while (length(occrest)==0 & length(rest)>0) {
                              parents.i <- getParentname(new.seqstart)
                              pi.occ <- any(parents.i==parents.classj)
                              
                              if (pi.occ) {
                                    class.j <- c(class.j, new.seqstart)
                              } else {
                                    deposit <- c(deposit, new.seqstart)
                              }
                              rest <- rest[-1]
                              new.seqstart <- rest[1]
                              occrest <- which(rest[-1]==new.seqstart)
                        }
                  }
                  
                  # add class j to list of classes, set new starting points for main loop
                  seqs <- c(seqs, list(class.j))
                  x <- rest
                  lx <- length(x)
            }
            
            if (length(deposit)>0) {
                  seqs <- c(list(deposit),seqs)
            }
            
            return(seqs)
      }

