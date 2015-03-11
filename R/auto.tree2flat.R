# 
##' Transform tree-structured  data automatically into flat representation
##' 
##' Transforms a nested list representing tree structured data automatically into one or several data frames.
##' @usage auto.tree2flat(x, simplify=FALSE)
##' @param x a nested list representing tree structured data (xml/json)
##' @return a list containing one or several data frame(s)
##' @examples
##' # First, make sure the necessary API key is saved in your R session:
##' # (This example is based on the Project Vote Smart API [PVS API])
##' saveAPIkey(key.var="pvs", key="YOUR-KEY-HERE")
##' # first create a request function:
##' pvsmeasure <- "http://api.votesmart.org/Measure.getMeasure?"}
##' measureparameters <- data.frame(parameter="measureId", value=NA)}
##' getMeasureRequest <- apiRequestFunction(x=measureparameters, base.url=pvsmeasure)
##' # get some data from the PVS API...
##' mr <- getMeasureRequest(measureId=1632) # create a request object
##' \dontrun{x <- apiGET(mr)}
##' \dontrun{x.list <- content2list(x)}
##' \dontrun{x.df <- auto.tree2flat(x.list)}
##' \dontrun{x.df_simplenames <- auto.tree2flat(x.list)}



auto.tree2flat <-
      function(x, simplify=FALSE) {
            # make sure that data is in the right format
            stopifnot((is.list(x) | is.character(x)))
            
            if (is.data.frame(x)) {# content already as df? flatten (if nested) and return in list
                  x <- flatDF(x)
                  if (is.data.frame(x)) {
                        x <- list(root=x)
                  }
                  return(x)
                  
                  } else {
                        
                        if (is.list(x)) {
                              x <- flattenTree(x)
                        }
                        
                        if (is.null(x)) {
                        return(list())
                        }
                        
                        # I) keep original names, unify and clean node-names for processing 
                        originalnames <- names(x)
                        u_originalnames <- originalnames[!duplicated(originalnames)]
                        names(x) <- uniqueNodenames(x, only.leafs=FALSE)
                        x <- cleanNodenames(x)
                        newnames <- names(x)
                        u_newnames <- newnames[!duplicated(originalnames)]
                        
                        
                        # II) make the tree simpler if possible (remove unnecessary nodes)
                        simplified <- FALSE
                        if (simplify & !onlyRootChildren(x)) {
                              xs <- simplifyTree2(x)
                              if (!identical(xs,x)) {
                                    x <- xs
                                    simplified <- TRUE
                              }
                        }
                        
                        
                        # III) extract singly occurring leafs, save in root df
                        
                        nodenames <- names(x)
                        leafs <- leafNames3(x)
                        root.df <- x[nodenames %in% leafs]
                        root.df <- as.data.frame(t(root.df), stringsAsFactors=FALSE)
                        
                        x <- x[ !(nodenames %in% leafs)]  # remove already extracted data
                        names(x) <- nodenames[!(nodenames %in% leafs)] # keep original names (no numbering)
                        
                        vars.remaining <- unique(names(x))
                        if (length(vars.remaining)==0) {  # all data processed? finish here...
                              # keep original names for root.df (simplification does not matter here!)
                              names(root.df) <- originalnames
                              main.list <- list(root=root.df)
                              
                              return(main.list)
                              
                              } else {
                                    # IV) transform each remaining subtree (with repeatedly occuring leafs) to one table
                                    #     (split the remaining vector into repeated leaf-sequences, transform each part into
                                    #     a table)
                                    
                                    lseq <- getLeafsequences(x)     
                                    rest.list <- lapply(lseq, function(i) {
                                          
                                          parentname <- parentName(i)
                                          if(length(parentname)==0) {
                                                parentname <- "miscellaneous"
                                          }
                                          
                                          i.df <- wide2long_2(i) # rearrange as df
                                          
                                         
                                          # cosmetics for post-processing
                                          if (!simplified) {
                                                i.names <- names(i.df)
                                                names(i.df) <- u_originalnames[u_newnames %in% i.names]
                                                names(i.df)[is.na(names(i.df))] <- i.names[is.na(names(i.df))]  # keep names if otherwise would result in NAs
                                          }
                                          
                                          names(i.df) <- numberedNames(i.df)
                                          i.list <- list(i.df)
                                          
                                          if (length(parentname)!=0) {
                                                names(i.list) <- parentname
                                          }
                                          
                                          return(i.list)
                                          }
                                          )
                                    } # (end of data contains subtrees with different parent/child elements)
                        # V) Finish list with data frames to be returned
                        
                        if (ncol(root.df)==0) {  # root.df remained empty in the process... only return rest
                              
                              df.list <- unlist(rest.list, recursive=FALSE)
                              return(df.list)
                              
                        } else {
                              
                              main_name <-  parentName(root.df)  
                              main.list <- list(root.df)
                              if (length(main_name)==0) {
                                    main_name <- "root"
                              }
                              
                              names(main.list) <- main_name
                              df.list <- c(list(main.list),rest.list)
                              df.list <- unlist(df.list, recursive=FALSE)
                              return (df.list)
                              
                        }
                  }
      }
