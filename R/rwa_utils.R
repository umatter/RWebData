#---------------------------------
# map xml according to XML2R package
# wrapper around several XML2R functions
#---------------------------------

xml2r <- 
  function(x) {
        # from XML2R::urlsToDocs
        x.doc <- try_default(xmlParse(x, asText = TRUE), 
                             NULL, quiet = TRUE)
        
        x.nodes <- docsToNodes(list(x.doc), "/")
        x.list <- nodesToList(x.nodes)
        x.obs <- listsToObs(x.list, "1")
        x.flat <- collapse_obs(x.obs)
        x.flat.df <- lapply(x.flat, as.data.frame, stringsAsFactors=FALSE)
        
        return(x.flat.df)
  }


#----------------------------------
# flatten nested data frames
# x a data frame that potentially contains
# more data frames in one or several columns
#----------------------------------

flatDF <-
  function(x){
        stopifnot(is.data.frame(x))
        
        cols <- names(x)
        colclasses <- unlist(lapply(cols, function(i){class(x[,i])=="list"}))
        
        if (any(colclasses)) {
              
              x.sub <- x[colclasses]
              subclasses <- unlist(lapply(names(x.sub), function(i){class(x.sub[,i][[1]])}))
              nesteddfs <- subclasses=="data.frame"
              x.nesteddfs <- x.sub[nesteddfs]
              
              flat.list <- lapply(names(x.nesteddfs), FUN=function(i){
                    df.i <- dfList(x.nesteddfs[,i])
              })
              
              names(flat.list) <- names(x.nesteddfs)
              
              x.df <- x[, !(names(x) %in% names(flat.list))]
              x.list <- c(list(root=x.df), flat.list ) # add clean part to list
              
              return(x.list)
              
        } else {
              return(x)
        }
    
  }


#-----------------------------------
# cbind 2 data frames with different
# numbers of rows via recycling
#-----------------------------------

cbindFill <- 
  function(df1, df2) {
    
    for ( i in names(df2)) {
      
      df1[,i] <- df2[,i]
      
    }
    
    return(df1)
    
  }


#---------------------------
# remove duplicated columns
# and clean names
#---------------------------

cleanFlatdata <-
  function(x) {
    
    x <- x[!duplicated(lapply(x, c))]
    names(x) <- sub(pattern="INPUT_:_", replacement="", x=names(x))
    return(x)
    
  }


#------------------------
# postprocess names
#------------------------

onlyLeafnames <-
  function(x) {
    stopifnot(is.data.frame(x))
    
    names(x) <- str_after(x=names(x), pattern=".", n="last")
    return(x)
  }




#------------------------
# json2DataFrame(x)
# given a string containing json, the function 
# maps the tree structured json data to one or several data frames
#------------------------

# JSONtoDataFrame <-
#   function(x) {
# 
#     x.list <- try(RJSONIO::fromJSON(x, nullValue=NA, simplify=FALSE), silent=TRUE)
#     if (class(test)=="try-error") {x.list <- .fromJSON_R_NA(x)} # based and dependend on rjson
#     x.vector <- flattenTree(x.list)
#     x.df <- auto.tree2flat(x.vector, primary.key.name="RWebData_ID", primary.key="a")
#     
#     return(x.df)
# 
#   }


#------------------------
# xml2DataFrame(x)
# given a string containing xml, the function 
# maps the tree structured xml data to one or several data frames
#------------------------

# XMLtoDataFrame <-
#   function(x) {
#     
#     x.list <- xml2list(body)
#     x.vector <- flattenTree(x.list)
#     x.df <- auto.tree2flat(x.vector)
#     return(x.df)
#   }


#----------------------------------------------------------------------
# .fromJSON_R_NA, .parseValue_NA, .parseNull_NA
# The following code is taken from the rjson package (Couture-Beil)    
# and slightly changed for the use in RWebData                         
# Parses JSON null as NA, not as NULL                                  
# #----------------------------------------------------------------------
# 
# 
# .fromJSON_R_NA <- function( json_str ) # changed for RWebDatause
# {
#   if( !is.character(json_str) )
#     stop( "JSON objects must be a character string" )
#   chars = strsplit(json_str, "")[[1]]
#   tmp <- .parseValue_NA( chars, 1) # changed here for RWebDatause
#   if( is.null( tmp$incomplete ) )
#     return( tmp$val )
#   else
#     return( NULL )
# }
# 
# .parseValue_NA <- function( chars, i ) # changed for RWebData use
# {
#   if( i > length( chars ) )
#     return( list( "incomplete" = TRUE ) )
#   
#   #ignore whitespace
#   while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
#     i = i + 1
#     if( i > length( chars ) )
#       return( list( "incomplete" = TRUE ) )
#   }
#   
#   ch = chars[i]
#   if( ch == "{" ) {
#     return( .parseObj( chars, i ) )
#   }
#   if( ch == "[" ) {
#     return( .parseArray( chars, i ) )
#   }
#   if( ch == "\"" ) {
#     return( .parseString_escape( chars, i ) ) # changed for RWebData
#   }
#   if( any(grep("[0-9\\-]", ch)) ) {
#     return( rjson:::.parseNumber( chars, i ) )
#   }
#   if( ch == "t" ) {
#     return( rjson:::.parseTrue( chars, i ) )
#   }
#   if( ch == "f" ) {
#     return( rjson:::.parseFalse( chars, i ) )
#   }
#   if( ch == "n" ) {
#     return( .parseNull_NA( chars, i ) ) # changed here for RWebData use
#   }
#   #stop("shouldnt reach end of parseValue")
#   
#   err <- paste( "unexpected data:", paste( chars[ i:length(chars)], collapse = "" ) )
#   stop( err )
# }
# 
# .parseNull_NA <- function( chars, i )
# {
#   if( paste(chars[i:(i+3)], collapse="") == "null" )
#     return( list(val=NA,size=i+4) ) # changed val=NULL to val=NA for RWebData use
#   stop("error parsing null value (maybe the word starts with n but isnt null)")
# }
# 
# .parseString_escape <- function( chars, i )
# {
#   str_start = i
#   if( chars[i] != "\"") stop("error")
#   i = i + 1
#   if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#   
#   while( TRUE ) {
#     while( chars[i] != "\\" && chars[i] != "\"" ) {
#       i = i + 1
#       if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     }
#     if( chars[i] == "\\" ) {
#       i = i + 2 #skip the next char
#       if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     }
#     else
#       break
#   }
#   str_end = i
#   i = i + 1
#   return(list(
#     val=eval(parse(text=gsub(pattern="\\/", replacement="/", 
#                              x=paste(chars[str_start:str_end], collapse=""), # added for RWebData
#                              fixed=TRUE))), 
#     size=i ))
# }
# 
# 
# .parseObj <- function( chars, i )
# {
#   obj <- list()
#   if( chars[i] != "{" ) stop("error - no openning tag")
#   i = i + 1
#   if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#   
#   first_pass <- TRUE
#   while( TRUE ) {
#     
#     #ignore whitespace
#     while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
#       i = i + 1
#       if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     }
#     
#     
#     #look out for empty lists
#     if( chars[i] == "}" && first_pass == TRUE ) {
#       i = i + 1
#       break
#     }
#     first_pass <- FALSE
#     
#     #get key
#     str = .parseString_escape( chars, i ) # changed for RWebData
#     if( is.null( str$incomplete ) == FALSE ) return( str )
#     key = str$val
#     i = str$size
#     if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     
#     #ignore whitespace
#     while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
#       i = i + 1
#       if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     }
#     
#     
#     #verify seperater
#     if( chars[i] != ":" ) stop("error - no seperator")
#     i = i + 1
#     if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     
#     
#     #ignore whitespace
#     while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
#       i = i + 1
#       if( i > length( chars ) )
#         return( list( "incomplete" = TRUE ) )
#     }
#     
#     
#     #get value
#     val = .parseValue_NA( chars, i ) # changed for RWebData
#     if( is.null( val$incomplete ) == FALSE ) return( val )
#     obj[key] <- list(val$val)
#     i = val$size
#     
#     if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     
#     #ignore whitespace
#     while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
#       i = i + 1
#       if( i > length( chars ) )
#         return( list( "incomplete" = TRUE ) )
#     }
#     
#     if( chars[i] == "}" ) {
#       i = i + 1
#       break
#     }
#     if( chars[i] != "," ) stop("error - no closing tag")
#     i = i + 1
#     if( i > length( chars ) )
#       return( list( "incomplete" = TRUE ) )
#   }
#   return( list(val=obj, size=i) )
# }
# 
# 
# .parseArray <- function( chars, i )
# {
#   useVect <- TRUE
#   arr <- list()
#   if( chars[i] != "[" ) stop("error - no openning tag")
#   
#   i = i + 1
#   if( i > length( chars ) )
#     return( list( "incomplete" = TRUE ) )
#   
#   while( TRUE ) {
#     
#     #ignore whitespace
#     while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
#       i = i + 1
#       if( i > length( chars ) )
#         return( list( "incomplete" = TRUE ) )
#     }
#     
#     #look out for empty arrays
#     if( chars[i] == "]" ) { 
#       i = i + 1
#       useVect <- FALSE #force an empty list instead of NULL (i.e. value = vector("list",0))
#       break
#     }
#     
#     #get value
#     val = .parseValue_NA( chars, i ) # changed for RWebData
#     if( is.null( val$incomplete ) == FALSE ) return( val )
#     arr[length(arr)+1] <- list(val$val)
#     if( is.list(val$val) || length(val$val) > 1 || is.null(val$val) )
#       useVect <- FALSE
#     
#     i = val$size
#     if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
#     
#     #ignore whitespace
#     while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
#       i = i + 1
#       if( i > length( chars ) )
#         return( list( "incomplete" = TRUE ) )
#     }
#     
#     if( chars[i] == "]" ) { 
#       i = i + 1
#       break
#     }
#     if( chars[i] != "," ) stop("error - no closing tag")
#     i = i + 1
#     if( i > length( chars ) )
#       return( list( "incomplete" = TRUE ) )
#   }
#   if( useVect )
#     arr <- unlist(arr)
#   return( list(val=arr, size=i) )
# }
# 

#------------------------
# cleanNodenames(x)
# removes duplicated "." and ends with "." from nodenames
# x named character vector representing a tree structured document
# returns character vector
#-------------------------

cleanNodenames <- 
  function(x) {
        xn <- names(x)
        xn <- sub(pattern="\\.{2,}", replacement="", x=xn) # remove double edges ("..")
        xn <- sub(pattern="\\.$", replacement="", x=xn) # remove edge without node to follow ("asdf.")
        
        names(x) <- xn
        
        return(x)
  }


#-------------------------
# getNodes(x, by="leafnames", match)
# returns all nodenames that match a certain criteria (i.e. leafname)
# x character string, the nodenames of a tree structured document represented in a character vector
# by character string indicating what criteria the nodes should match (so far only "leafnames")
# match character string with the matching criteria
# 
#--------------------------

getNodes <- 
  function(x, by="leafnames", match) {
        if (by=="leafnames") {
              
              l<- getLeafname(x)
              xmatch <- x[ l %in% match]
              
              return(xmatch)
        }
        
        if (by=="branch") {
              b<- getParentname(x)
              xmatch <- x[ b %in% match]
              
              return(xmatch) 
        }
  }


#-------------------------
# getLeafname(x)
# a function that returns the nodename of the leaf element given a node's name including the hierarchy
# nodename character string, the name of a node (in a tree structured document represented as a character vector)
#--------------------------

getLeafname <- 
  function(nodename, pattern="."){
        stopifnot(is.character(nodename))
        
        leafname <- str_after(nodename, pattern=pattern)
        return(leafname)
  }


#-------------------------
# getSiblingname(x)
# a function that returns the nodenames of siblings given a node's name
# x names of a named character vector representing tree structured data
# nodename character string, the name of a node (in a tree structured document represented as a character vector)
#--------------------------

getSiblingname <- 
  function(x, nodename){
        stopifnot(is.character(nodename), is.character(x))
        
        parent <- getParentname(nodename)
        leaf <- getLeafname(nodename)
        
        if (leaf==parent){# no parent? --> no siblings --> return empty
              return("")
        }
        
        #siblings <- str_match_all(string=x, pattern=parent )
        siblings <- grep(pattern=parent, x=x, value=TRUE, fixed=TRUE)
        siblings2 <- siblings[!grepl(pattern=leaf, x=siblings, fixed=TRUE)] # only siblings, not node itself
        
        return(siblings2)
  }


#-------------------------
# getParentsSiblingname(x)
# a function that returns the nodenames of the parents siblings given a node's name
# x names of a named character vector representing tree structured data
# nodename character string, the name of a node (in a tree structured document represented as a character vector)
#--------------------------

getParentsSiblingname <- 
  function(x, nodename){
    
    stopifnot(is.character(nodename), is.character(x))
    
    parent <- getParentname(nodename)
    leaf <- getLeafname(parent)
    parent <- getParentname(parent) # parent siblings needed, hence grandparent is parent
    
    
    if (leaf==parent){# no parent? --> no siblings --> return empty
      
      return("")
      
    }
    
    #siblings <- str_match_all(string=x, pattern=parent )
    
    siblings <- grep(pattern=parent, x=x, value=TRUE, fixed=TRUE)
    siblings2 <- siblings[!grepl(pattern=leaf, x=siblings, fixed=TRUE)] # only siblings, not node itself
    
    return(siblings2)
    
  }

#-------------------------
# getParentname(x, pattern=".")
# a function that returns the parent-node's name given a node's name
# x character string, the name of a node (in a tree structured document represented as a character vector)
# pattern character string, the pattern/symbol that separates nodes
#--------------------------

getParentname <- 
  function(x, pattern="."){
    
    stopifnot(is.character(x))
    
    parent <- str_after(x, pattern=pattern, after=FALSE)
    
    return(parent)
    
  }


#-------------------------
# another alternative to simplify
# x a vector or data frame representing tree structured data
# note: the names of x have first been processed with uniqueNodenames!
# names(x) <- uniqueNodenames(x)
#--------------------------

simplifyTree2 <-
  function(x){
    
    xn <- names(x)
    
    # I) count occurrence of all strings
    xntab <- table(xn)
    count <- as.vector(xntab)
    names(count) <- names(xntab)
    count <- count[xn]
    
    # II) count occurrence of last part (leaf)
    leafs <- str_after(xn, ".")
    leafstab <- table(leafs)
    leafscount <- as.vector(leafstab)
    names(leafscount) <- names(leafstab)
    leafscount <- leafscount[leafs]
    
    
    # III) get indeces of those names that:
    # a) occur only once as a whole AND
    # b) have a last part (leaf) that shows up several times
    # c) have two or more edges
    
    indices <- which(count==1 & leafscount >1)
    nedges <- str_count(string=xn[indices], pattern="\\.")
    indices <- indices[nedges>1]
    
    # d) have the same siblings
    # if the node with the fewest leafs contains all occurring siblings,
    # it cannot be that there are nodes with other siblings
    
    ab <- xn[indices]
    leafs <- getLeafname(ab)
    uleafs <- unique(leafs)
    uparents <- unique(getParentname(ab))
    allsibl <- getNodes(x=xn, by="branch", match=uparents)
    uallsibl_leafs <- unique(getLeafname(allsibl))
    nleafs <- count(getParentname(ab))
    min_nleafs <- nleafs[which.min(nleafs$freq),"x"]
    min_nleafs_sibl <- getLeafname(getNodes(x=allsibl, by="branch", match=as.character(min_nleafs)))
    othersiblings <- !all(uallsibl_leafs %in% min_nleafs_sibl)
    
    # CONTINUE HERE!!! SOLUTION ABOVE IS NOT ENOUGH! SEE EXAMPLE IN BUGSCRIPT
    
    if (othersiblings) { # any other siblings possible? check where and remove from indices
    
    samesibl.list <- lapply(leafs, FUN=function(i){
      
      i.nodes <- getNodes(ab, match=i)
      
      siblings <- unlist(lapply(i.nodes, FUN=function(j){getSiblingname(x=xn, nodename=j)}))
      
      # anyDuplicated(siblings)>0
      check <- any(duplicated(getLeafname(siblings)))
      
      
    })
    samesibl.check <- unlist(samesibl.list)
    
    indices <- indices[samesibl.check]
    
    }
    
    if (length(indices)>1) { # any unnecessary additional nodes found?
      
      # III) delete unnecessary additional node by replacing the names
      # Important: if any, only the parent nodes of leafs should be deleted
      # not any nodes higher up in the hierarchy (i.e.: I.1.a --> I.a, but NOT: I.C.2.b --> I.b)
      
      nedges <- str_count(string=xn, pattern="\\.")
      max.nedges <- max(nedges)
      
      if (max.nedges<=2) {
        
        xn[indices] <- sub(pattern="\\..+\\.", replacement="\\.", x=xn[indices])
        names(x) <- xn
        
      } else { # more than 2 edges involved? only consider leaf + 2 levels (see above)
        
        # cut in two pieces: beginning part up to leaf+2levels, and leaf+2levels, only process second part
        # and then paste back together
        xn.split <- str_cut(xn, "\\.", from.left=FALSE, n=3)
        xn.split <- do.call("rbind", xn.split)
        xn.split.first <- xn.split[,1]
        xn.split.second <- xn.split[,2]
        
        xn.split.second[indices] <- sub(pattern="\\..+\\.", replacement="\\.", x=xn.split.second[indices])
        xn <- paste(xn.split.first, ".", xn.split.second, sep="")
        xn <- sub(pattern="^\\.", replacement="", x=xn) # remove "." if it  is the first character
        
        names(x) <- xn
        
      }
      
    }
    
    
    
    return(x)
    
  }



#-------------------
# uniqueNodenames(x)
# takes a vector or data frame representing tree structured data and changes identical
# nodenames that show up on different levels in the tree hierarchy to avoid ambiguity
# x a vector or data frame representing tree structured data
# details: the names are changed as follows: if two nodes have the same name, the node higher in the
# hierarchy is not changed and the one lower in the hierarchy is extended with the name of the parent element
# value: returns the names of the input vector (df) with the changed names
#-------------------

uniqueNodenames <-
  function(x, only.leafs) {
    
    xnames <- names(x)
    
    
    # I) extract nodenames for each level and arrange in matrix 
    
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
    nodes.df <- as.data.frame(nodes.m, stringsAsFactors=FALSE)
    #nodes.df[is.na(nodes.df)] <- "" # clean NAs out
    
    if (ncol(nodes.df)==1) {# only one level? return xnames as it is
      
      return(xnames)
      
    } else {
      
      # II) replace ambigious nodenames (for-loop is potentially inefficient)
      nc <- ncol(nodes.df)
      if (only.leafs & nc>2) {
        nodes.begin <- nodes.df[,1:(nc-2)]
        nodes.df <- nodes.df[,(nc-1):nc]
      }
      
      
      for (i in ((length(nodes.df)-1):1)) {
        
        u.level.i <- unique(nodes.df[,i])
        jstart <- i+1
        
        for (j in length(nodes.df):jstart) {
          
          u.level.j <- unique(nodes.df[,j])
          
          # rename lower level node names if duplicated with higher level
          j.duplicated <- na.omit(u.level.j[u.level.j %in% u.level.i])
          j.duplicated <- j.duplicated[j.duplicated!=""]
          nodes.df[nodes.df[,j] %in% j.duplicated ,j] <- paste(nodes.df[nodes.df[,j] %in% j.duplicated ,i],nodes.df[nodes.df[,j] %in% j.duplicated ,j], sep="_")
          
        } # end inner loop
        
      } # end outer loop
      
      
      # III) rearrange in one character vector (and clean names)
      
      if (only.leafs & nc>2) {
        nodes.df <- cbind(nodes.begin, nodes.df)
      }
      
      
      nodes.list <- as.list(nodes.df)
      
      x2 <- do.call("paste", c(nodes.list, sep="."))
      x2 <- sub(pattern="\\.{2,}", replacement="", x=x2) # remove double edges ("..")
      x2 <- sub(pattern="\\.$", replacement="", x=x2) # remove edge without node to follow ("asdf.")
      
      
      
      return(x2)
      
    }
    
  }


#----------------------------
# simplifyTree(x)
# A function that attempts to simplify tree structured data in the sense that
# subtrees of the same structure and the same child nodes are merged to one subtree containing
# several edges to these child nodes (can especially make sense if the original data format is json, 
# and nodes are numbered list entries).
# example/motivation: 
# legiscandata <- apiEasyData("http://api.legiscan.com/?key=LEGISCANKEY&op=getMasterList&state=CA")
# x a nested list representing tree structured data
# NOTE: NOT VERY EFFICIENT, TEST WHETHER CHANGE TO USING VECTORS AS FLAT REPRESENTATION IS IMPROVING THIS.
#--------------------------

simplifyTree <- 
      function(x) {
            leafsequence <- detectLeafSequence(names(x)) # get the indices of the starting end ending point of repeated leaf sequences on the lowest level of the hierarchy
            
            if (length(leafsequence)>1) { # is there a leaf sequence? simplify the tree accordingly
                  
                  seqfirst <- leafsequence[1]
                  seqlast <- leafsequence[length(leafsequence)]
                  
                  # 1) check whether sequence has a break
                  in_increment <- leafsequence[2] - seqfirst
                  sim_sequence <- seq(from=seqfirst, by=in_increment, along.with=leafsequence)
                  sim_sequence[length(sim_sequence)] <- (sim_sequence[length(sim_sequence)]-1) # set the last index to the last index of the sequence, not the beginning of the next sequence
                  stopifnot(sim_sequence==leafsequence) # add error handling here...
                  
                  # 2) remove the parent nodes of all leafs in the sequence 
                  #    (this should be sufficient if wrappers have been removed before)
                  x_leafsequence <- x[,seqfirst:seqlast]
                  parentslevel <- max(nLevels(names(x))) # the parents level is the max level of leafs in the lowest hierarchy according to nLevels (counts first level as 0; see nLevels())
                  x_leafsequence <- removeLevelnodename(x=x_leafsequence, level=parentslevel)
                  names(x)[seqfirst:seqlast] <- names(x_leafsequence)
                  
            } 
      
  
  

  
  return(x)
  
}


#---------------------------
# detectLeafSequence(x)
# returns the indeces of repeated character sequences in the names of a character vector or a data frame
# (in the context of RWebData: detects the sequence of leaf elements at the lowest hierarchy level)
# x a character vector representing the nodes of tree structured data (i.e., only the names of a vector representing the tree structured data, not the values)
#---------------------------

detectLeafSequence <- function(x) {
  
  stopifnot(is.character(x))
  
  
  # I) extract all the leafs (lowest level only) and the respective parents
  nlev <- nLevels(x)
  all_leafs <- getLevelnodes(x, level=max(nlev)+1) # AT THE MOMENT ONLY HANDLES LEAFSEQUENCES ON THE LOWEST LEVEL! SHOULD BE GENERALIZED
  
  leafnames <- str_after(all_leafs, ".")
  uleafnames <- unique(leafnames)
  parents <- str_after(all_leafs,".", n=max(nlev))
  parentnames <- str_after(parents,".", after=FALSE)
  uparentnames <- unique(parentnames)
  uparents <- unique(parents)
  
  
  # II) check whether there might be a sequence or several
  
  # a) # all leafs have the same parent node? --> no need for simplyfication, return NULL
  
  if (length(uparents)==1) { 
    
    indices <- NULL  
    return(indices)
  }
  
  # THIS PART DOES NOT WORK PROPERLY! LEGISCAN EXAMPLE GIVES A TRUE --> WRONG
  # RETHINK STRATEGY
  #  # b) different parents with DIFFERENT leafs? --> no simplyfication, return NULL 
  #  # THIS SHOULD BE IMPROVED LATER ON WITH THE ABILITY TO HANDLE THE POSSIBILITY OF SEVERAL DIFFERENT
  #  # LEAF SEQUENCES. HENCE EITHER PROCESS THE PARENTS WITH DIFFERENT LEAFS DIRECTLY (VECTORIZE WITH DETECTSEQUENCE)
  #  # OR BY DIRECTLY IMPLEMENTING THE DETECTION OF SEVERAL SEQUENCES IN detectSequence.
  #   
  #   checklist <- lapply(uparentnames, FUN=function(i) {
  #     
  #     length(uleafnames) != length(whichin(string=i, x=uparents, exact=FALSE))
  #     
  #   })
  #   
  #   if (any(unlist(checklist))) {
  #     
  #     indices <- NULL  
  #     return(indices)
  # 
  #   }
  
  
  
  
  # III) detect a sequence in the leafs
  
  indices <-  detectSequence(all_leafs)
  return(indices) 
  
}


#-----------------------------
# detectSequence(x)
# given a character vector with the names of nodes representing the structure of a treestructured document,
# the function returns the start- and end-points of repeatedly occuring vector entries (nodenames)
# x a character vector
#-----------------------------

detectSequence <- 
  function(x) {
    
    all_leafschildren <- str_after(x,".")
    all_leafschildren <- x
    u_leafschildren <- unique(all_leafschildren)
    freq <- unlist(lapply(u_leafschildren, nin, x=all_leafschildren))
    freqvar <- u_leafschildren[freq>1] # only entries that show up more than once (otherwise no sequence is possible)
    
    if (length(freqvar)>0) {# any leaf sequence at all? 
      
      
      # get the indices of the starting  points of the repeatedly occuring sequence
      all_children <- str_after(x,".")
      indices <- which(apply(embed(all_children, length(freqvar)), 1, identical, rev(freqvar)))
      indices <- c(indices, indices[length(indices)] + (length(freqvar)-1)) # add the index of the last part of the last sequence
      
    } else {# no leaf sequence at all? set to NULL
      
      indices <- NULL
      
      
    }
    
    return(indices)
    
    
  }



#---------------------------
# HTTPstatusMessage(x) 
# returns the http status code 
# x an apiresponse object
# value: numeric
#----------------------------

HTTPstatusMessage <-
  function(x){
    stopifnot(is.apiresponse(x))
    
    message <- x@statusMessage
    
    return(message)
    
  }




#---------------------------
# HTTPstatus(x) 
# returns the http status code 
# x an apiresponse object
# value: numeric
#----------------------------

HTTPstatus <-
  function(x){
    stopifnot(is.apiresponse(x))
    
    head <- header(x)
    status <- as.numeric(unname(head["status"]))
    
    return(status)
    
  }



#---------------------------
# header(x) 
# returns the http header of an apiresponse object
# x an apiresponse object
#----------------------------

header <-
  function(x){
    stopifnot(is.apiresponse(x))
    
    return(x@header)
    
  }



#----------------------------
# handleHTTPError(x)
# handles apiresponse objects in case the HTTP status is not OK
# x an object of class apiresponse
# value an object of class apidata
#----------------------------

handleHTTPError <-
  function(x) { # if(apiresponseOK(x)) check this outside of the function --> if ok: process via content2list and auto.tree2flat...
    stopifnot(is.apiresponse(x))           # if not: process with this function 
    
    
    sm <- HTTPstatusMessage(x)
    sc <- HTTPstatus(x)
    rx <- x@request.arguments
    
    flatdata <- data.frame(statusMessage=sm)
    # as there is no data to be extracted the flatdata only consists of
    # of the http status message (these data)
    # would also added to the extracted data if available
    
    rx.string <- paste0(names(rx), " = ", rx[1,], collapse="; ")
    warg <- paste0("HTTP response not OK for: ",rx.string,  "\nHTTP status: ", sc, " ", sm)
    warning(warg, call.=FALSE)
    
    # HTTP status indicates a problem, instead of atempting to process the content of the body,
    # generate a response object with the statusMessage and the parameters in used in the request
    # Remaining issue: if http request is ok, but api responds with an error (e.g., xml error tag) this
    # is not noticed so far. check whether generalized solution makes sense at all. --> is there a 
    # standard for xml/json errors by apis?!
    
    return(flatdata) 
    
  }





#-----------------------------------
# vectorizeIt(x)
# takes a function (i.e. request function) as an input and returns a vectorized version of it
# Note: only vectorizes over function parameters without default values!!
# x a function 
# value a function
# example:
# mMeasureRequest <- vectorizeIt(getMeasureRequest)
#-----------------------------------

# CONTINUE HERE:  TEST THE FUNCTION FOR DIFFERENT INPUT FUNCTIONS!!!

vectorizeIt <- 
  function(x) {
    stopifnot(is.function(x))
    
    f.internal <-  x
    args <- formals(x)
    nd.args <- args[as.list(args)==""]
    rest.args <-  args[as.list(args)!=""]
    
    if (length(nd.args)==0) {stop("Original function contains only arguments with default values.")}
    
    # function skeleton
    f <- function(){}
    
    # a) redefine the internal function within the new function
    body.int <- as.character(body(f.internal))
    body.int <- c( body.int, "}")
    nd.args.int <- paste0(names(nd.args), collapse=",")
    rest.args.int <- paste0(names(rest.args), "='", rest.args, "'")
    rest.args.int2 <- paste0(rest.args.int, collapse=",")
    
    fdef.int <- paste0("f.int <- function(", nd.args.int, ",", rest.args.int2, ")")
    body.int <- c(fdef.int,body.int)
    
    # b) write formals for vectorized function
    nd.args2 <- nd.args
    names(nd.args2) <- paste0("v.",names(nd.args2))
    formals(f) <- c(nd.args2, rest.args)
    
    # c) write vectorization (take 1:n of first non-default argument in original function)
    
    vec.begin <- paste0("vec.list <- lapply( 1:length(",names(nd.args2)[1], "), FUN= function(i) {")
    vec.assign <- sapply(1:length(names(nd.args)), FUN=function(a){
      paste0(names(nd.args)[a], " <- ", names(nd.args2)[a],  "[i]")
    }) 
    vec.intargs <- paste0(names(nd.args), " = ", names(nd.args), collapse=", " )
    vec.intf <- paste0("f.int(", vec.intargs,")")
    vec.end <- "})"
    vec.return <- "return(redlist(vec.list))"
    
    body.vec <- c(vec.begin, vec.assign, vec.intf, vec.end, vec.return)
    
    
    # d) write the body of the new function
    
    e.int <- parse(text=body.int)
    e.vec <- parse(text=body.vec)
    e <- c(e.int, e.vec)
    body(f) <- as.call(c(as.name("{"),e))
    
    return(f)
  }


#-----------------------
# dlStatus(x, pause=0)
#-----------------------
# A function that shows the current id/object in a loop or vectorized function and pauses
# the loop for a certain time if needed
# x = the current id/object in the loop
# pause 

dlStatus <-
  function(x, pause=0) {
    
    Sys.sleep(pause)
    cat("\r",x)
    flush.console()
    
  }


#------------------------
# url2apirequest(url,key.param)
# converts an url into an apirequest-object
# url a character string with the url to query the api
# key.param (optional) a character string containing the api key parameter required in the url (included to distinguish between the key param and other parts of the query)
# details: if key.param not included, the key.param will be part of the request.arguments in the apirequest-object
# see: apiEasyData()
#-------------------------


url2apirequest <-
  function(url, key.param=NULL) {
    stopifnot(is.character(url), (is.character(key.param) | is.null(key.param)))
    
    p.url <- parse_url(url)
    
    if (!is.null(p.url$query)) {# url contains query parameters?
      
      q <- p.url$query
      rarg <- data.frame(t(unlist(q)))
      if (!is.null(key.param)) {rarg[,key.param] <- NULL}
      
      server <- modify_url(url, query="")
      
    } else { # query empty? --> use path argument
      
      p <- p.url$path
      rarg <- data.frame(path=p)
      server <- modify_url(url, path="")
      
    }
    
    req <- new("apirequest", URL = url, request.arguments = rarg, 
               nodefault.parameters = data.frame(), server = server)
    
    return(req)
    
  }


#------------------------
# xml2list(x)
# converts an xml document into a nested list
# x a character string containing a xml document
# This function is based on/adopted from the core idea in XML2R::nodesToList. It is an alternative
# to XML::xmlToList(). The difference to xmlToList() is essentially that every node and subnode is 
# converted into a sublist in the nested list (even if a logically simpler representation as element 
# of one sublist, such as a matrix would make sense). 
# Compare, e.g., the results between converting xml data from http://api.votesmart.org/Votes.getBill?
# with this function and xmlToList().
#-------------------------

xml2list <- function(x) {
  stopifnot(is.character(x))

  # clean malformed xml (removes any symbols occurring before the xml-document starts)
  # x <- sub(pattern="^(.*?)\\<", replacement="<", x=x)
  x <- sub(pattern="^[^<]*<", replacement="<", x=x)
  x <- sub(pattern="^<xml", replacement="<?xml", x=x)
  
  
  # partly taken from XML2R::urlsToDocs
  doc <- try_default(xmlParse(x, asText = TRUE), NULL, quiet = TRUE)
  
  if (!is.null(doc)) {
    attr(doc, "XMLsource") <- x
  }
  docs <-list(doc) # workaround
  nodes <- docsToNodes(docs, "/*/node()")
  list <- nodesToList(nodes) # problem: does not keep all names... 
  list <- list[[1]]
  
  # workaround to keep names
  childnames <- names(xmlRoot(doc))
  names(list) <- childnames
  
  return(list)
  
}



#------------------------
# nLevels(x)
# returns the number of hierarchy levels in x
# x a data frame representing flattened treestructure data.
# pattern a character used to separate elements of different levels in the names of x
# details: note that with the current implementation the n of levels returned counts the first level as 0
# a.b thus refers to one level, a.b.c refers to 2 etc. In other words the count refers to the number of edges (".")
#-------------------------

nLevels <- 
  function(x) {
    
    sapply(x, FUN=function(i) {
      str_count(string=i, "\\.")      
    }, USE.NAMES=TRUE)
    
    
  }


#--------------------------
# getLevelnodes(x, level)
# returns the nodes of a certain hierarchy level (the complete nodes, not only the names!)
# x a data frame or vector representing tree structured data
# level numeric, which level should be returned
#--------------------------

getLevelnodes <- function(x, level){
  
  stopifnot(is.data.frame(x) | is.character(x))
  
  if (is.data.frame(x)) {
    dfnames <- names(x)
    x <- as.character(t(x))
    names(x) <- dfnames}
  
  nlev <- nLevels(x) + 1 
  xlev <- x[nlev==level]
  
  return(xlev)
  
}


#--------------------------
# removeLevelnodename(x, level)
# removes the nodes (in terms of names, not values) of a certain hierarchy level 
# x a data frame representing tree structured data
# level numeric, which level should be removed
# value a character vector 
#--------------------------

removeLevelnodename <- function(x, level){
  
  stopifnot((is.data.frame(x) ))
  
  #   if (is.data.frame(x)) {
  #     dfnames <- names(x)
  #     x <- as.character(t(x))
  #   names(x) <- dfnames}
  
  
  nlev <- nLevels(names(x)) 
  xlev <- x[,nlev==level]
  
  nodenames <- names(xlev)
  nodenames_before <- str_after(x=nodenames, pattern=".", after=FALSE, n=level-1 )
  nodenames_after <- str_after(x=nodenames, pattern=".", after=TRUE, n=level+1 )
  
  nodenames2 <- paste(nodenames_before, nodenames_after, sep=".")
  
  names(x)[nlev==level] <- nodenames2
  
  return(x)
  
  
}



#------------------------
# onlyRootChildren(x) 
# NEW, SIMPLER IMPLEMENTATION OF ONLYCHILDREN --> WORKS ON NAMES OF VECTOR OR DF ONLY 
# returns TRUE if x only contains child elements of the root element (otherwise no ) 
# x a data frame representing flattened treestructure data.
#-------------------------

onlyRootChildren <-
  function(x) {
    
    n.edges <- nin("\\.", names(x), exact=FALSE)
    
    onlychildren <- (n.edges==0)
    
    
    return(onlychildren)
  }



#------------------------
# onlyChildren(x)
# returns TRUE if x only contains child elements 
# x a data frame representing flattened treestructure data.
#-------------------------

onlyChildren <-
  function(x) {
    stopifnot(is.character(x) | (is.data.frame(x)))
    
    elements <- treeStructureDF(x)
    if(is.null(elements)) {return(FALSE)}
    n_elements <- ncol(elements)
    parent <- unique(elements[,1])
    
    onlychildren <- (n_elements==2 & length(parent)==1) # only two levels and only top element as parent means: data only contains child elements  
    
    return(onlychildren)
  }


#------------------------
# sameParent(x)
# returns TRUE if x only contains child elements of the same parent element
# x a data frame representing flattened treestructure data.
#-------------------------

sameParent <-
  function(x) {
    stopifnot(is.character(x) | (is.data.frame(x) & nrow(x)==1))
    
    elements <- treeStructureDF(x)[,-1]
    n_elements <- ncol(elements)
    parent <- unique(elements[,1])
    
    sameparent <- (n_elements==2 & length(parent)==1) # only two levels and only one parent element means: data only contains child elements of the same parent element 
    
    return(sameparent)
  }


#-----------------------
# parentName(x)
# returns the name of the parent element of several tree-sctructure elements. The function guesses the 
# parent name based on the elements' name  preserving the tree structure (element-names separated by
# a sep character). i.e. if there are several elements of the same type called "x.y.a", "x.y.b", "x.y.c", where
# x is the top/root element, y is recognized as the parent element of a, b, and c.
# 
# x a data frame containing a flattened nested list representing tree structured data.
# pattern the character separating list elements in the names of x (defaults to ".")
#-----------------------

parentName <- 
  function(x, pattern=".") {
    stopifnot(is.character(x) | (is.data.frame(x) ))
    
    if (onlyChildren(x)) {return("top")} # if only contains child elements, return "top" (not the best solution, but simple to avoid errors)
    
    element.names <- names(x) 
    
    if(is.null(element.names)) {return(NA)}
    
    if (length(unique(element.names))==1) { #only one element type in x? simply return the name of the first element
      
      parentname <- unique(str_after(element.names, pattern=pattern, after=FALSE, n=1))
      
    } else { # different element types? figure out the common parent element...
      
      parts.df <- treeStructureDF(x)[,-1]
      
      #       # OLD:
      #     parts <- strsplit(element.names, split=pattern, fixed=TRUE)
      #     parts <- lapply(parts, function(x){as.data.frame(t(x))})
      #     parts.df <- dfList(parts)
      
      n = 1
      column = 0
      while (n==1) {
        column <- column + 1
        name <- unique(parts.df[,column])  
        n <-  length(name)
      }
      
      parentname <- as.character(unique(parts.df[,(column-1)]))
    }
    
    return(parentname)
    
  }



#-------------------------
# as.paramlist 
# converts a two-column matrix or data-frame to a list of function parameters (name/value)
# x a two-column matrix or data-frame containing the parameter names in column 1
# and the parameter values in column 2. If a parameter should not have a default-value,
# the respective row in the second column should be NA.
# Value: a list. the names of the list entries are the parameter names, the respective 
# list entries are the parameter values. The list can then directly be used to set 
# function parameters via formals()
# Example:
# x <- matrix(c("stateId", "officeId", "year", "CA", NA, "2012"), nrow=3) 
# f <- function(){}
# formals(f) <- as.paramlist(x)
# f
#--------------------------

as.paramlist <-
  function(x){
    stopifnot( (is.data.frame(x) | is.matrix(x)), ncol(x)==2)
    
    if (is.data.frame(x)){
      x[,1] <- as.character(x[,1])
      x[,2] <- as.character(x[,2])
    }
    
    
    # extract and coerce function parameters
    # to generate parameters without default, the workaround solution
    # formals(function(x){})$x  is used to get empty (not NULL!) list entries
    param.nodefault <- x[is.na(x[,2]),1]
    nodefaultvalues <- structure(replicate(length(param.nodefault), formals(function(x){})$x ), names=param.nodefault)
    param.null <- na.omit(x[x[,2]=="defaultNULL",1])
    nulldefaultvalues <- structure(replicate(length(param.null), NULL), names=param.null)
    param.default <- x[(!is.na(x[,2]) & !x[,2]=="defaultNULL"),]
    defaultvalues <- structure( as.list(param.default[,2]), names = param.default[,1]) 
    
    params <- c(nodefaultvalues, defaultvalues, nulldefaultvalues)
    pstrings <- c(param.nodefault, param.default[,1], param.null)
    
    params.list <- list(parameters=params, p.strings=pstrings, nodefaults=param.nodefault)
    
    return(params.list)
    
  }


# -------------------------
# rmSublist(x, rm)
# x a nested list 
# rm a character string/vector indicating lists to be removed from the top list
# -------------------------
rmSublist <-
  function(x, rm) {
    stopifnot(is.list(x), is.character(rm))
    
    sublistnames <- names(x)
    
    for (i in rm) {
      rmindex <- whichin(i,sublistnames)
      x <- x[[-rmindex]]
    }
    
    return(x)
    
  }



#--------------------
# listElements(x, na.omit=TRUE)
# summarizes the elements of a tree structured data set for each level.
# returns the summaries for n levels in a list of length n.
# x a nested list representing tree structured data
#----------------------

listElements <-
  function(x, na.omit=TRUE) {
    stopifnot(is.list(x), is.logical(na.omit))
    
    xflat <- flattenTree(x)
    xdf <- treeStructureDF(xflat)
    
    elementlist <- lapply(1:ncol(xdf), FUN=function(i){ 
      
      if (na.omit==TRUE) eli <- na.omit(factor(xdf[,i]))
      if (na.omit==FALSE) eli <- factor(xdf[,i])
      
      summary(eli)
      
    })
    
    return(elementlist)
    
  }


#--------------------
# wide2long_2(x)
# transform a wide data.frame (repeated column-names from different observations) to a long data.frame
# x a wide data.frame
#--------------------

wide2long_2 <-
  function(x) {
    stopifnot(is.character(x) | (is.data.frame(x) & nrow(x)==1))
    
    vars <- names(x)
    uvars <- unique(vars)
    
    if (length(uvars)==1) { # consists of only one variable? simply transpose and return...
      
      flattable.df <- data.frame(x, row.names=NULL, stringsAsFactors=FALSE)
      names(flattable.df) <- uvars
      return(flattable.df)
    } 
    
    
    xntab <- table(vars)
    freqvar <- as.vector(xntab)
    names(freqvar) <- names(xntab)
    freqvar <- freqvar[vars]# keep the order of vars!
    freqvar <- freqvar[freqvar>1]
    
    
    if (length(unique(names(freqvar)))<=1) { # only one or no name shows up repeatedly? --> there is no repeated frequence of variables, but just duplicated variable names
      # this indicates, that the data frame is already in "long" format, hence return as such
      return(data.frame(t(x), row.names=NULL, stringsAsFactors=FALSE))
      
    }
    
    seqfirst <- names(freqvar[1])
    seqlast  <- names(freqvar[length(freqvar)])
    
    seqbegin <- sapply(c(seqfirst, seqlast), function(x) { which(vars==x)})
    stopifnot((length(seqbegin)>0)) # this is rather as check during the development/tests with diverse apis (could be removed if never occurs)
    
    if ( is.list(seqbegin)) { # makes function robust to malformed XML source data (only XML attributes)
      
      seqstart <- seqbegin[[seqfirst]]
    } else {
      seqstart <- seqbegin[,1]
      
    }
      flattable.list <- lapply(1:(length(seqstart)-1), FUN=function(i){
        
        start <- seqstart[i]
        end <- seqstart[i+1]-1
        
        xi <- as.data.frame(t(x[start:end]), stringsAsFactors=FALSE, row.names=NULL) 
        names(xi) <- numberedNames(xi)
        xi
      })
      
      # add the rest 
      rest <- as.data.frame(t(x[seqstart[length(seqstart)]:length(x)]), stringsAsFactors=FALSE, row.names=NULL)
      names(rest) <- numberedNames(rest)
      flattable.list <- c(flattable.list, list(rest))
     
    flattable.df <- dfList(flattable.list) # combine to one df
    
    
    return(flattable.df)
    
  }


#--------------------
# wide2long(x)
# transform a wide data.frame (repeated column-names from different observations) to a long data.frame
# x a wide data.frame
#--------------------

wide2long <-
  function(x) {
    stopifnot(is.character(x) | (is.data.frame(x) & nrow(x)==1))
    
    vars <- names(x)
    uvars <- unique(vars)
    
    if (length(uvars)==1) { # consists of only one variable? simply transpose and return...
      
      flattable.df <- data.frame(x, row.names=NULL, stringsAsFactors=FALSE)
      names(flattable.df) <- uvars
      return(flattable.df)
    } 
    
    freqvar <- summary(factor(vars))[vars] # keep the order of vars!
    freqvar <- freqvar[freqvar>1]
        
    
    if (length(unique(names(freqvar)))<=1) { # only one or no name shows up repeatedly? --> there is no repeated frequence of variables, but just duplicated variable names
      # this indicates, that the data frame is already in "long" format, hence return as such
      return(data.frame(t(x), row.names=NULL, stringsAsFactors=FALSE))
      
    }
    
    seqfirst <- names(freqvar[1])
    seqlast  <- names(freqvar[length(freqvar)])
    
    seqbegin <- sapply(c(seqfirst, seqlast), function(x) { which(vars==x)})
    stopifnot((length(seqbegin)>0)) # this is rather as check during the development/tests with diverse apis (could be removed if never occurs)
    
    if ( is.list(seqbegin)) { # makes function robust to malformed XML source data (only XML attributes)
      
      seqstart <- seqbegin[[seqfirst]]
      flattable.list <- lapply(1:(length(seqbegin[[seqfirst]])-1), FUN=function(i){
        
        start <- seqstart[i]
        end <- seqstart[i+1]-1
        
        xi <- as.data.frame(t(x[start:end]), stringsAsFactors=FALSE, row.names=NULL) # if dfList is changed to work with vectors (not dfs) this should also be simplifyed
        names(xi) <- numberedNames(xi)
        xi
      })
      
    # add the rest 
    rest <- as.data.frame(t(x[seqstart[length(seqstart)]:length(x)]), stringsAsFactors=FALSE, row.names=NULL)
    names(rest) <- numberedNames(rest)
    flattable.list <- c(flattable.list, list(rest))
            
    } else {

    
    flattable.list <- lapply(1:nrow(seqbegin), FUN=function(i){
      
      start <- seqbegin[i,1]
      end <- seqbegin[i,2]
      
      xi <- as.data.frame(t(x[start:end]), stringsAsFactors=FALSE ) # if dfList is changed to work with vectors (not dfs) this should also be simplifyed
      
      
    })
    }
    
    #      flattable.df <- do.call("rbind", flattable.list)
    #      flattable.df <- as.data.frame(flattable.df, stringsAsFactors=FALSE)
    flattable.df <- dfList(flattable.list)
    
    
    names(flattable.df) <- numberedNames(flattable.df)
    return(flattable.df)
    
  }





#---------------------
# leafNames3(x)
# a function that extracts all leafnames only occuring once in the tree (= all leafs directly describing the entity behind the tree data)
# x a flattened nested list representing tree structured data (XML/JSON)
#---------------------
leafNames3 <-
  function(x) {
    stopifnot((is.data.frame(x) & nrow(x)==1) | is.character(x) )
    
    varnames <- names(x)
    unames <- unique(varnames)
    nodes <- summary(factor(varnames))[unames]
    unodes <- unique(summary(nodes))
    
    leafnames <- na.omit(names(nodes[nodes==1]))
    noleafsnames <- names(nodes[nodes!=1])
    
    
    if (length(noleafsnames) > 1 & length(leafnames)>0 ) { # the additional steps below make only sense if there are more than one 
      # additional node (otherwhise the adding of only singly remaining nodes to main.df in
      # auto.tree2.flat would not be consistent )
      # and if there are any leafnames
      
      
      # make sure that none of the nodes identyfied as leafs wouldn't fit better in
      # an other df (not top) --> only occurring once does not mean the top df is the most logical
      # unclear if the following procedure is enough.  Idea: occurrs only once & no siblings in 
      # more frequently occurring nodes up to highest level.
      
      nlev <- max(nLevels(leafnames))
      if (nlev>0) {
      
      for (i in 1:nlev) {
      # parents of leafs should not occur in parents of noleafs
      leafparents <- str_after(x=leafnames, pattern=".", after=FALSE, n=i)
      noleafsparents <- unique(str_after(x=noleafsnames, pattern=".", after=FALSE, n=i))
      leafnames <- leafnames[!(leafparents %in% noleafsparents)]
      }
      
      }

      
    }
    
    #     if (length(unodes)==1) { # all node names occur the same number of times means they are all on the same level, hence all leafs if an api returns data on several entities
    #     
    #       leafnames <- unames
    # 
    #     } else { # if there are different number of occurances, the ones only showing up once are leafs
    #       
    #       leafnames <- names(nodes[nodes==1])
    #     }
    return(leafnames)
  }



#---------------------
# leafNames2(x)
# a function that extracts all leafnames only occuring once in the tree (= all leafs directly describing the entity behind the tree data)
# x a flattened nested list representing tree structured data (XML/JSON)
#---------------------
leafNames2 <-
  function(x) {
    stopifnot((is.data.frame(x) & nrow(x)==1) | is.character(x) )
    
    varnames <- names(x)
    unames <- unique(varnames)
    nodes <- summary(factor(varnames))[unames]
    unodes <- unique(summary(nodes))
    
    # I) what complete nodenames occur only once
    leafnames <- na.omit(names(nodes[nodes==1]))
    noleafsnames <- names(nodes[nodes!=1])
    
    
    if (length(noleafsnames) > 1) { # the additional steps below make only sense if there are more than one 
      # additional node (otherwhise the adding of only singly remaining nodes to main.df in
      # auto.tree2.flat would not be consistent )
      
      
      # make sure that none of the nodes identyfied as leafs wouldn't fit better in
      # an other df (not top) --> only occurring once does not mean the top df is the most logical
      # unclear if the following procedure is enough
      
      leafparents <- str_after(x=leafnames, pattern=".", after=FALSE)
      noleafsparents <- unique(str_after(x=noleafsnames, pattern=".", after=FALSE))
      
      leafnames <- leafnames[!(leafparents %in% noleafsparents)]
      
      # II) make sure the branches (all up to the leafname) also occor only once
      parents <- getParentname(leafnames)
      partab <- table(parents)
      occ <- as.vector(partab)
      names(occ) <- names(partab)
      occleafs <- occ[occ==1]
      leafnames <- leafnames[names(occleafs) %in% parents]
      
      
    }
    
    
    return(leafnames)
  }

#---------------------
# leafNames(x)
# a function that extracts all leafnames only occuring once in the tree (= all leafs directly describing the entity behind the tree data)
# x a flattened nested list representing tree structured data (XML/JSON)
#---------------------
leafNames <-
  function(x) {
    stopifnot((is.data.frame(x) & nrow(x)==1) | is.character(x) )
    
    varnames <- names(x)
    unames <- unique(varnames)
    nodes <- summary(factor(varnames))[unames]
    unodes <- unique(summary(nodes))
    
    leafnames <- na.omit(names(nodes[nodes==1]))
    noleafsnames <- names(nodes[nodes!=1])
    
    
    if (length(noleafsnames) > 1) { # the additional steps below make only sense if there are more than one 
      # additional node (otherwhise the adding of only singly remaining nodes to main.df in
      # auto.tree2.flat would not be consistent )
      
      
      # make sure that none of the nodes identyfied as leafs wouldn't fit better in
      # an other df (not top) --> only occurring once does not mean the top df is the most logical
      # unclear if the following procedure is enough. Problem: go up two levels is somewhat arbitrary
      # find a simpler approach that does the same! Idea: occurrs only once & no siblings in 
      # more frequently occurring nodes up to highest level
      
      # parents of leafs should not occur in parents of noleafs
      leafparents <- str_after(x=leafnames, pattern=".", after=FALSE)
      noleafsparents <- unique(str_after(x=noleafsnames, pattern=".", after=FALSE))
      leafnames <- leafnames[!(leafparents %in% noleafsparents)]
      
#       # parents of parents of leafs should not occur in parents of noleafs
#       leafparents <- str_after(x=leafnames, pattern=".", after=FALSE, n=2)
#       noleafsparents <- unique(str_after(x=noleafsnames, pattern=".", after=FALSE, n=2))
#       leafnames <- leafnames[!(leafparents %in% noleafsparents)]
      
    }
    
    #     if (length(unodes)==1) { # all node names occur the same number of times means they are all on the same level, hence all leafs if an api returns data on several entities
    #     
    #       leafnames <- unames
    # 
    #     } else { # if there are different number of occurances, the ones only showing up once are leafs
    #       
    #       leafnames <- names(nodes[nodes==1])
    #     }
    return(leafnames)
  }


#----------------
# str_nextract(x, pattern, from.left=TRUE, n)
# return the parts of strings in a vector up to the nth occurrence of a certain pattern (counted either from left or right -->  begin or end of string)
# x a character vector
# from.left logical, indicating whether the nth occurrence of the pattern is counted from the left or the right (defaults to TRUE --> count from the left)
# n positive numeric indicating at what number of occurrences of the pattern the string should be cut
#---------------

str_nextract <-
  function(x, pattern, from.left=TRUE, n) {
    stopifnot(is.character(x), is.numeric(n), is.logical(from.left), is.character(pattern))
    
    parts <- strsplit(x, split=pattern, fixed=TRUE)
    lengths <- lapply(parts,length)
    
    if (from.left) {
      
      cut.list <- lapply(parts, FUN=function(i){
        
        begin <- paste(i[1:n], collapse=pattern)
        
        
      })
      
    } else {
      
      cut.list <- lapply(parts, FUN=function(i){
        
        end.cut <- ((length(i)-n)+1)
        end <- paste(i[end.cut:length(i)], collapse=pattern)
        end
        
      })
      
    }
    return(unlist(cut.list))
  }







#----------------
# str_cut(x, pattern, from.left=TRUE, n)
# return the  strings in a vector in two pieces, cut at the nth occurrence of a certain pattern (counted either from left or right -->  begin or end of string)
# x a character vector
# from.left logical, indicating whether the nth occurrence of the pattern is counted from the left or the right (defaults to TRUE --> count from the left)
# n positive numeric indicating at what number of occurrences of the pattern the string should be cut
#---------------

str_cut <-
  function(x, pattern, from.left=TRUE, n) {
    stopifnot(is.character(x), is.numeric(n), is.logical(from.left), is.character(pattern))
    
    pattern.loc <- str_locate_all(x, pattern)
    
    if (from.left) {
      
      split.list  <- lapply(1:length(x), FUN=function(i){
        
        i.x <- x[i]
        i.l <- pattern.loc[[i]]
        
        i.occ <-  nrow(i.l)
        i.splitpoint <- ifelse(i.occ<n, 0, i.l[n,])
        
        i.firststart <- 1
        i.firstend <- (i.splitpoint-1)
        i.first <- str_sub(i.x, start=i.firststart, end=i.firstend)
        
        i.secondstart <- ifelse(i.splitpoint==0, 0, (i.splitpoint+1))
        i.secondend <- ifelse(i.splitpoint==0, 0, -1)
        
        i.second <- str_sub(i.x, start=i.secondstart, i.secondend)
        
        c(i.first, i.second)
        
        
      })
      
    } else {
      
      split.list  <- lapply(1:length(x), FUN=function(i){
        
        i.x <- x[i]
        i.l <- pattern.loc[[i]]
        
        i.occ <-  nrow(i.l)
        i.splitpoint <- ifelse(i.occ<n, 0, i.l[i.occ+1-n,])
        
        i.firststart <- ifelse(i.splitpoint==0, 0, 1)
        i.firstend <- ifelse(i.splitpoint==0, 0, (i.splitpoint-1))
        i.first <- str_sub(i.x, start=i.firststart, end=i.firstend)
        
        i.secondstart <- (i.splitpoint+1)
        i.second <- str_sub(i.x, start=i.secondstart)
        
        c(i.first, i.second)
        
        
      })
      
      
    }
    
    return(split.list)
    
  }






#---------------
# strafter
# extract substring containing all after or before the first or last occurrence of certain character pattern
# x a character vector
# pattern a character string
# after logical, indicating whether the substring after or before the ocurrence of the pattern should be extracted
# (defaults to TRUE)
# n positive numeric (or character "last") indicating whether the part before or after the nth occurrence (or the last occurence: "last") should
# be extracted. (default is "last")
#---------------

str_after <-
  function(x, pattern, after=TRUE, n="last") {
    stopifnot(is.character(x), (n=="last" | (is.numeric(n) & n>0)), is.logical(after), is.character(pattern))
    
    parts <- strsplit(x, split=pattern, fixed=TRUE)
    
    if (after) { # return substring after pattern...
      
      if (n=="last") {
        
        resp <- lapply(parts, FUN=function(i){
          nlast <- length(i)
          i[nlast] })
        
      } else{
        
        resp <- lapply(parts, FUN=function(i){
          nlast <- length(i)
          if (nlast < n) n <- nlast
          paste(i[n:length(i)], collapse=pattern)
        })
      }
      
      
    } else { # return substring before pattern...
      
      if (n=="last") {
        
        resp <- lapply(parts, FUN=function(i){
          nlast <- length(i)
          paste(i[1:(nlast-1)], collapse=pattern) })
        
      } else {
        
        resp <- lapply(parts, FUN=function(i){
          nlast <- length(i)
          if (nlast < n) n <- nlast
          paste(i[1:n], collapse=pattern)
        })
      }
      
    }
    
    resp <- unlist(resp)
    return(resp)
    
  }





# how often is a certain string in a character vector x?
nin <- function(string, x, exact=TRUE) {
  
  if (exact==TRUE) {
    sum(as.numeric(x %in% string))
  } else {
    sum(as.numeric(grepl(pattern=string, x=x )))
  }
  
}

# in which elements of a character vector x is a certain string?
whichin <- function(string, x, exact=TRUE) {
  
  if (exact==TRUE) {
    namesmatch <- x %in% string
  } else {
    namesmatch <- grepl(pattern=string, x=x )  
  }
  
  nnames <- 1:length(x)
  indeces <- nnames[namesmatch]
  indeces
}


# numberedNames
# check if several variables with same name describing this obs, add numbering if yes
# x a data frame 
numberedNames <- 
  function(x, .sep=".") {
    
    varnames <- names(x)
    unames <- unique(names(x))
    
    if (length(varnames)==length(unames)) { # no need to change names
      
      return(names(x))
      
    } else {
      
      namefreq <- sapply(unames, nin, x=names(x))
      freqnames <- names(namefreq[namefreq>1])
      
      for (n in freqnames) {
        indeces <- whichin(n, names(x)) # example
        
        for (i in 1:length(indeces)) { 
          j <- indeces[i]  
          names(x)[j] <- paste(names(x)[j],.sep,i, sep="")
          
        }
      }
      
      return(names(x))
      
    } # end else/changes
    
  }

# namesCount
# count the nth occurrence for each name of a vector or data.frame (colname)
# returns a vector of length length(names(x)) with the number the respective 
# element (of the same name) occurred so far in the vector
# x a data frame 
namesCount <- 
  function(x) {
      uname <- unique(names(x))
      name <- names(x)
      
      count <- list()
      length(count) <- length(name)
      
      for (n in uname) {
        indeces <- whichin(n, name) # example
        n.count <- 1:length(indeces)
        count[indeces] <- n.count
      }
      
      return(unlist(count))
      
    } 


# from httr:
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#subTree(x,nm)
# based on a post from Bert Gunter: http://r.789695.n4.nabble.com/fast-subsetting-of-lists-in-lists-tp3076502p3077105.html
# x is a named list (of lists of ...)
# nm is the name of the variable containing a sub-tree to be extracted
subTree <- 
  function(x,nm) {
    
    st <- NULL
    
    for(nmx in names(x)) st <-  c(st,
                                  
{
  z <- x[[nmx]]
  if(nmx==nm) z
  else if(is.list(z)) Recall(z,nm)
}
    )
    return(st)
  } # should be extended with adding of primary keys (rel. database idea), here reference : subTree(x=x.list, "address_components") ?

#msubTree(x,parts)
# partly based on a post from Bert Gunter: http://r.789695.n4.nabble.com/fast-subsetting-of-lists-in-lists-tp3076502p3077105.html
# returns a list with lists representing subtrees to be transformed to flat representation individually
# x is a named list (of lists of ...)
# parts a character vector with the names of variables containing  sub-trees to be extracted
msubTree <- 
  function(x, parts) {
    
    parts.list <- lapply(parts, subTree, x=x)
    
  }





#------------------------
# summary.treestructure(x)
# prints a summary of a tree structure object
# x a data.frame describing a tree structured data set entry (see treeStructureDF)
# ------------------------
summary.treestructure <-
  function(x){
    
    stopifnot(is.list(x)) # consider changing to a class check when using this as a method$
    
    elementlist <- listElements(x, na.omit=TRUE)
    
    levelnames <- c("Toplevel:", paste("Level ", 2:length(elementlist),": ", sep=""))
    levelsummaries <- lapply(elementlist, FUN=function(i){
      
      sumi <- paste(names(i), " (", as.numeric(i),")", sep="")
      sumi_string <- paste(sumi, collapse=", ")
      sumi_string
      
    })
    levelsummaries <- paste(levelsummaries, "\n")
    
    ind <- 2 # indentation after "Level..."
    exd <- max(nchar(levelnames)) + ind +1 # indentation next paragraph
    levelsummaries <- str_wrap(levelsummaries, width=100, indent=ind, exdent=exd)
    
    
    for (j in 1:length(levelsummaries)) {
      
      cat(levelsummaries[j], labels=levelnames[j], fill=TRUE)
      
    }
    
    
  }



#---------------------------
# edgeMatrix(x, all=FALSE)
# a function returning a two-column matrix, each row defining one edge between a parent element in the first column
# and the child element in the second column. The edge-matrix is needed as input variable in the plot methods.
# EXTEND DESCRIPTION FOR OBJECT ORIENTED VERSION...
# x a nested list representing tree structured data (XML/JSON)
# all logical, indicating whether the edge-matrix should be generated for all elements (i.e. also duplicated parent-child combinations)
#     defaults to FALSE
# check.nodenames logical, indicating whether potentially ambigious nodenames should be changed (default is FALSE; see details for more info)
#----------------------------

edgeMatrix <-
  function(x, all=FALSE, check.nodenames=TRUE) {
    stopifnot((is.list(x) | is.character(x)))
    
    
    if (is.list(x)) { x.flat <-  flattenTree(x=x)
    } else {
      x.flat <- x
    }
    
    nodes.df <- treeStructureDF(x.flat, check.nodenames=check.nodenames)
    
    if (all==FALSE) {
      
      vertex.list <- lapply(1:(ncol(nodes.df)-1), FUN=function(i){
        
        unique.occ <- na.omit(unique(nodes.df[,i:(i+1)]))
        unname(as.matrix(unique.occ))
        
      })
      edge.matrix <- do.call("rbind", vertex.list)
      
    } else {
      
      vertex.list <- lapply(1:(ncol(nodes.df)-1), FUN=function(i){
        
        occ <- na.omit(nodes.df[,i:(i+1)])
        unname(as.matrix(occ))
        
      })
      edge.matrix <- do.call("rbind", vertex.list)
      
    }
    
    return(edge.matrix)
    
  }




#---------------------
# visualize.treedata.jitter(x, charlim, ...)
# plot function for treedata objects with a jitter for elements with long names on the same level
# (in order to make the node labels readable in the plot)
# x a nested list representing tree structured data (XML/JSON)
# charlim numeric, indicating the upper bound number of characters in node-labels without jittering 
# the labels on a level. 
# all logical, indicating whether the edge-matrix should be generated for all elements (i.e. also duplicated parent-child combinations)
#     defaults to FALSE (passed on to edgeMatrix())
# leveldist numeric, indicating the extent if the jitter (y-axis distance within a level with jittered elements)
# further parameters --> igraph.plot parameters 
#Details: the core of this function is a wrapper around the layout.reingold.tilford() function from the igraph package.
#---------------------

visualize.treedata.jitter <-
  function(x, char.lim, all=FALSE, check.nodenames=TRUE, leveldist=0.1, vertex.size=16, vertex.shape="none", vertex.label.cex=0.7, vertex.label.family="sans", vertex.label.color="steelblue", ...) {
    stopifnot((is.list(x) | is.character(x)), is.numeric(char.lim), length(char.lim)==1, is.numeric(leveldist), length(leveldist)==1)
    #require(igraph)
    
    # generate tree graph and get basic tree layout
    edge.matrix <- edgeMatrix(x,all=all, check.nodenames=check.nodenames)
    gtree <- graph.edgelist(el=edge.matrix, directed=FALSE)
    l <- layout.reingold.tilford(gtree, root=1)
    
    # check if vertex names might be too long for a nice graph
    vn <- get.vertex.attribute(gtree, "name")
    vertex.labelnchar <- nchar(vn)
    toolong <- vertex.labelnchar > char.lim
    
    # add additional levels to make long vertex labels readable
    levels.toolng <- unique(l[toolong,2])
    for (i in levels.toolng){
      
      new.i <- rep_len(c(i, i-leveldist), length(l[l[,2]==i,2]) )
      l[l[,2]==i,2] <- new.i
      
    }
    
    # actual plot function
    plot(gtree,
         rescale=TRUE,
         layout=l,
         vertex.size=vertex.size,
         vertex.shape=vertex.shape,
         vertex.label.cex=vertex.label.cex,
         vertex.label.family=vertex.label.family,
         vertex.label.color=vertex.label.color,
         asp=0,
         ...
    )
    
    
    
  }





#---------------------
# visualize.treedata.equal(x, charlim, ...)
# plot function for treedata objects based on manual scaling (optimized for treas with many branches/leafs, in order to make the node labels readable in the plot)
# x a nested list representing tree structured data (XML/JSON)
# all logical, indicating whether the edge-matrix should be generated for all elements (i.e. also duplicated parent-child combinations)
#     defaults to FALSE (passed on to edgeMatrix())
# further parameters --> igraph.plot parameters 
#Details: the core of this function is a wrapper around the layout.reingold.tilford() function from the igraph package.
#---------------------

visualize.treedata.manual <-
  function(x, all=FALSE, check.nodenames=TRUE, vertex.size=60, vertex.shape="none", vertex.label.cex=0.4, vertex.label.family="sans", vertex.label.color="steelblue", ...) {
    stopifnot((is.list(x) | is.character(x)))
    #require(igraph)
    
    # generate tree graph and get basic tree layout
    edge.matrix <- edgeMatrix(x,all=all, check.nodenames=check.nodenames)
    gtree <- graph.edgelist(el=edge.matrix, directed=FALSE)
    l <- layout.reingold.tilford(gtree, root=1)
    
    # change layout for manual scaling
    l <- data.frame(l)
    names(l) <- c("x", "y")
    
    minx <- min(l$x)
    maxx <- max(l$x)
    
    miny <- 0
    maxy <- (maxx-minx)/2
    
    ulevels <- unique(l$y)
    ynew <- seq(from=maxy, to=miny, length.out=length(ulevels))
    ynew <- data.frame(y=ulevels,ynew=ynew)
    l <- merge(l, ynew, by="y", all.x=TRUE)
    l <- as.matrix(l[,c("x","ynew")])
    
    xl <- c((minx), (maxx))
    yl <- c((miny), (maxy))
    
    l <- l[order(l[,2], decreasing=TRUE),]
    
    # actual plotting
    plot(gtree,
         xlim=xl,
         ylim=yl,
         vertex.size=vertex.size,
         vertex.shape=vertex.shape,
         vertex.label.cex=vertex.label.cex,
         vertex.label.family=vertex.label.family,
         vertex.label.color=vertex.label.color,
         rescale=FALSE,
         layout=l,
         asp=0,
         margin=0,
         ...
    )
    
    
    
  }




#---------------------
# visualize.treedata(x, all, vertex.size, vertex.shape, vertex.label.cex)
# plot function for treedata objects based on manual scaling (optimized for treas with many branches/leafs, in order to make the node labels readable in the plot)
# x a nested list representing tree structured data (XML/JSON)
# all logical, indicating whether the edge-matrix should be generated for all elements (i.e. also duplicated parent-child combinations)
#     defaults to FALSE (passed on to edgeMatrix())
# further parameters --> igraph.plot parameters 
#Details: the core of this function is a wrapper around the layout.reingold.tilford() function from the igraph package.
#---------------------

visualize.treedata <-
  function(x, all=FALSE, check.nodenames=TRUE, vertex.size=15, vertex.shape="none", vertex.label.cex=0.7, vertex.label.family="sans", vertex.label.color="steelblue", ...) {
    stopifnot((is.list(x) | is.character(x)), is.logical(all), is.numeric(vertex.size), is.character(vertex.shape), is.numeric(vertex.label.cex))
    #require(igraph)
    
    # generate tree graph and get basic tree layout
    edge.matrix <- edgeMatrix(x,all=all, check.nodenames=check.nodenames)
    gtree <- graph.edgelist(el=edge.matrix, directed=FALSE)
    l <- layout.reingold.tilford(gtree, root=1)
    
    # actual plotting
    plot(gtree,
         vertex.size=vertex.size,
         vertex.shape=vertex.shape,
         vertex.label.cex=vertex.label.cex,
         vertex.label.family=vertex.label.family,
         vertex.label.color=vertex.label.color,
         rescale=TRUE,
         layout=l,
         ...
    )
    
  }


############################
# plot.treedata(x)
# a plot method for treedata
# x nested list representing tree structured data
############################

plot.treedata <-
  function(x, type="normal", char.lim=8, all=FALSE, leveldist=0.1, vertex.size=1, vertex.shape="none", vertex.label.cex=0.6) 
  {
    stopifnot(is.list(x), (type=="normal" | type=="jitter" | type=="manualscale"))
    
    if (type=="normal") { visualize.treedata(x=x, all=all, vertex.size=vertex.size, vertex.shape=vertex.shape, vertex.label.cex=vertex.label.cex)}
    
    if (type=="jitter") { visualize.treedata.jitter(x=x, char.lim=char.lim, leveldist=leveldist, all=all, vertex.size=vertex.size, vertex.shape=vertex.shape, vertex.label.cex=vertex.label.cex)}
    
    if (type=="manualscale") { visualize.treedata.manual(x=x, all=all, vertex.size=vertex.size, vertex.shape=vertex.shape, vertex.label.cex=vertex.label.cex)}
    
  }


#----------------------------------------------------------------------------------
# converts an HTML-encoded string to its character representation as an R string
# usefull for urls and email addresses transmitted in html-encoded format
# references: http://www.metaprog.com/samples/encoder.htm
# details: the email address fred.flintstone@bedrock.com can be represented as html-encoded string: &#102;&#114;&#101;&#100;&#46;&#102;&#108;&#105;&#110;&#116;&#115;&#116;&#111;&#110;&#101;&#64;&#98;&#101;&#100;&#114;&#111;&#99;&#107;&#46;&#99;&#111;&#109;
#  HTML Codes for accent marks: http://www.starr.net/is/type/htmlcodes.html
#----------------------------------------------------------------------------------

html_decode <- 
	function(x) {
		stopifnot(is.character(x))
		
		if (any(grepl(pattern = ";", x))){
			xparsed <- gsub(pattern = "&#", replacement = "", x, fixed = TRUE)
			xsep <- unlist(strsplit(xparsed, split = ";", fixed = TRUE))
			xsep <- xsep[xsep!=""]
			
		} else {
			xsep <- unlist(strsplit(x, split = "&#", fixed = TRUE))
			xsep <- xsep[xsep!=""]
		}
		
		xchar <- rawToChar(as.raw(xsep))
		return(xchar)
	}

html_accent_decode <-
	function(x) {
		stopifnot(is.character(x))
		
		not_accent <- x %in% c("&amp;", "&quot;","&nbsp;")
		if (!not_accent) { # only replace accents
			xdec <- lapply(x, FUN=function(y) {
				xmlValue(getNodeSet(htmlParse(y, asText = TRUE), "//p")[[1]]) 
			})
			xdec <- unlist(xdec)
		} else {
			xdec <- x
		}
		
		return(xdec)
	}

html_decode_all <- 
	function(x){
		stopifnot(is.character(x))
		
		# decode and replace html-encoded strings (with ascii number)
		allenc <- unlist(str_extract_all(x, "(&#[0-9]+)+;?"))
		allenc <- unique(allenc[allenc!=""])
		alldec <- unlist(lapply(allenc, html_decode))
		not_empty <- which(alldec!="")
		alldec <- alldec[not_empty]
		allenc <- allenc[not_empty]
		
		nreplacements <- length(allenc)
		for (i in 1:nreplacements){
			enc.i <- allenc[i]
			dec.i <- alldec[i]
			x <- gsub(enc.i, dec.i, x, fixed=TRUE)
		}
		
		# decode and replace html-encoded accents etc
		allenc <- unlist(str_extract_all(x, "&[a-z]+;"))
		allenc <- unique(allenc[allenc!=""])
		alldec <- unlist(lapply(allenc, html_accent_decode))
		not_empty <- which(alldec!="")
		alldec <- alldec[not_empty]
		allenc <- allenc[not_empty]
		
		nreplacements <- length(allenc)
		for (i in 1:nreplacements){
			enc.i <- allenc[i]
			dec.i <- alldec[i]
			x <- gsub(enc.i, dec.i, x, fixed=TRUE)
		}
		
		return(x)
	}

