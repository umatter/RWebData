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


## Describe tree-structured data
## 
##  Returns a data frame describing the structure of tree structured data with rows for each data
##  value (leaf) and columns for each node-level.
## @usage treeStructureDF(x, check.nodenames=FALSE)
## @param x either a nested list, a named character vector, or a data frame with one row
## representing tree-structured data
## @param check.nodenames logical, indicating whether potentially ambigious nodenames should be
## changed (default is FALSE; see details for more info)
## @return a data frame describing the structure of tree structured data with rows for each data
## value (leaf) and columns for each node-level.
## @details If check.nodenames=TRUE, the function attempts to change the names of child nodes
## that either also show up as parent nodes higher in the hierarchy or show up as children of
## other parent nodes on the same hierarchy level.
## @examples
## f <-  system.file("exampleData", "catalog.xml",  package = "XML") # from package XML
## doc  <-  xmlInternalTreeParse(f)
## xlist <- xmlToList(doc)
## xdf <- flattenTree(xlist)
## xch <- as.character(t(xdf))
## names(xch) <- names(xdf)
## treeStructureDF(xdf)
## treeStructureDF(xlist)
## treeStructureDF(xch)


treeStructureDF <-
      function(x, check.nodenames=FALSE) {
            
            stopifnot((is.data.frame(x) & nrow(x)==1) | is.character(x) | is.list(x))
            
            if (is.list(x)) {
                  x <- flattenTree(x)
            }
            
            xnames <- names(x)
            
            if (is.null(xnames)) {
                  return(NULL)
            }
            
            nsep <- strsplit(xnames, split=".", fixed=TRUE)
            nsep <- lapply(nsep, FUN=function(i){
                  
                  i <- i[i!=""]
                  }) # remove empty nodenames (can occur if attributes are used instead of nodes)
            
            ncols <- max(unlist(lapply(nsep, length)))
            nrows <- length(nsep)
            
            namesdf.list <- lapply(nsep, FUN=function(x){
                  data.i <- t(x)
                  empty <- ncols - length(data.i)
                  data.i <- c(data.i, rep("", times=empty))
                  #as.data.frame(t(x) , stringsAsFactors=FALSE )
                  })
            
            nodes.m <- do.call("rbind", namesdf.list)
            #nodes.df <- as.data.frame(nodes.m, stringsAsFactors=FALSE)
            nodes.df <- data.frame(root="root", nodes.m, stringsAsFactors=FALSE)
            nodes.df[nodes.df==""] <- NA
            
            
            if (check.nodenames==TRUE){     # Check whether same nodes occur on different levels, add parent name to duplicated node names to avoid ambiguity (especially with respect to visualization)
                  
                  for (i in (length(nodes.df)-1):2) {
                        
                        u.level.i <- unique(nodes.df[,i])
                        jstart <- i+1
                        
                        for (j in length(nodes.df):jstart) {
                              
                              if (j<=ncol(nodes.df)) {
                                    
                                    u.level.j <- unique(nodes.df[,j])
                                    
                                    # rename lower level node names if duplicated with higher level
                                    j.duplicated <- na.omit(u.level.j[u.level.j %in% u.level.i])        
                                    nodes.df[nodes.df[,j] %in% j.duplicated ,j] <- paste0(nodes.df[nodes.df[,j] %in% j.duplicated ,i],nodes.df[nodes.df[,j] %in% j.duplicated ,j])
                                    
                              }

                              } # end inner loop
                        
                        } # end outer loop
                  
                  for (i in 2:(length(nodes.df)-1)) { # change names of childnodes that are on the same level but have different parents
                        
                        j <- i+1
                        if (j<=ncol(nodes.df)) {
                              u.ij <- unique(nodes.df[,c(i,j)])
                              u.ij <- na.omit(u.ij)
                              
                              dupl.j <- u.ij[duplicated(u.ij[,2]),2]
                              row.dupl.j <-  row.names(u.ij[(u.ij[,2] %in% dupl.j ),])
                              
                              if (length(row.dupl.j) > 0) { # found any duplicated node names? if so, change the name
                                    nodes.df[row.dupl.j, j ] <- paste0(nodes.df[row.dupl.j, i ] ,nodes.df[row.dupl.j, j ])
                              }
                        }
                        
 
                  }
            } # end checknames
            
            return(nodes.df)
      }
