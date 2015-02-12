# NEEDS TO BE IMPROVED!! --> ID/PATH ASPECT
# ' Unify division of variables between several tables
# ' 
# ' Given a list of data frames (as a result of several requests processed with 
# ' apiDownload()) describing the same observations, the function attempts to 
# ' unify the data frames by distributing variables that show up in both the 
# ' root data frame and other data frames to their respective data frame.
# ' 
# ' @usage unifyDataframes(x, rootname="root")
# ' @param x a list of data frames
# ' @param rootname character, indicating the name of the root data frame
# ' @return a list containing one or several data frame(s)
# ' @examples
# ' root <- data.frame(firmname=c("microdata", "bigdata"), 
# '                       path=c("x1","x2"),
# '                       employees.name=c(NA,"john"))
# '                       
# ' products <- data.frame(products.productname=c("a","b","c","d"),
# '                           path=c("x1", "x1", "x2", "x2"))
# '                           
# ' employees <- data.frame(employees.name=c("sarah", "max"),
# '                         path=c("x1","x1"))
# '                         
# ' xlist <- list(root=root, products=products, employees=employees)
# ' 
# ' unifyDataframes(xlist)
# ' 

unifyDataframes_all <- 
  function(x) {
    
    dfs <- names(x)
    
    for (i in dfs) {
      
     x <-  unifyDataframes(x, rootname=i)
      
    }
    
    return(x)
 
  }


unifyDataframes <-
  function(x, rootname="root") {
    stopifnot(is.list(x))
    
    
    dfs <- names(x)[names(x)!=rootname]
    rootnames <- names(x[[rootname]])
    
    # I) check which variables show up in the root element AND in other dfs
    #    if there are none showing up, return x as it is
    
    dfnamesocc <- lapply(dfs, FUN=function(j){
      
      i <- x[[j]]
      
      names.i <- names(i)
      inroot <- rootnames %in% names.i[names.i!="path"]
      
      resp <- rootnames[inroot]
      if (length(resp)==0) {resp <- NA}
      return(resp)
      
    })
    names(dfnamesocc) <- dfs
    dfnamesocc <- dfnamesocc[!is.na(dfnamesocc)] # keep only non-empty entries
    
    if (length(dfnamesocc)==0) return(x)
    
    
    # II) ...if some variables occur several times. extract them from the
    # root data frame and add them (ther respective obs) to the respective
    # df.
    
    for (i in names(dfnamesocc)) {
      
      # a) extract from root add to respective table, delete in root
      vars <- dfnamesocc[[i]]
      names(vars) <- vars
      pn_vars <- parentName(vars)
      
      if (pn_vars!=rootname) {
        
        extrcol <- x[[rootname]][,vars]
        extrobs <- x[[rootname]][!is.na(extrcol),c(vars,"path")] # also extract path/ID!
        extrobs <- extrobs[!is.na(extrobs$path),]
        
        x[[pn_vars]] <- dfList(list(x[[pn_vars]], extrobs)) # add to parenttable
        x[[rootname]] <- x[[rootname]][,!(names(x[[rootname]]) %in% vars)]  # delete in root
        
      }
      
    }
    
    # delete list entries if only contain df with ID or no df at all
    x <- lapply(x, FUN=function(i){
      
      if (!is.data.frame(i)){
          NULL
        } else {
          i
        }
      })
    x <- lapply(x, FUN=function(i){
      
      if (length(names(i))==1){
        if (names(i)=="path"){
          NULL
        } else {
          i
        }
      } else {
        i
      }
      
    })
    x <- x[lapply(x,length)!=0] 
    
    return(x)
    
    
    
  }
    