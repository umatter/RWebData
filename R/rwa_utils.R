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
    
    if (!is.null(p.url$query) & 0 < length(p.url$query)) {# url contains query parameters?
      
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
  
  # decode HTML-special characters occuring in xml document
  x <- html_decode_all(x)
  
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
# visualize.treedata.manual(x, charlim, ...)
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


#---------------------
# visualize.treedata.vertical(x, all, vertex.size, vertex.shape, vertex.label.cex)
# plot function for treedata objects based on manual scaling (optimized for trees with many branches/leafs, in order to make the node labels readable in the plot)
# x a nested list representing tree structured data (XML/JSON)
# all logical, indicating whether the edge-matrix should be generated for all elements (i.e. also duplicated parent-child combinations)
#     defaults to FALSE (passed on to edgeMatrix())
# further parameters --> igraph.plot parameters 
#Details: the core of this function is a wrapper around the layout.reingold.tilford() function from the igraph package.
#---------------------

visualize.treedata.vertical <-
     function(x, all=FALSE, check.nodenames=TRUE, vertex.size=15, vertex.shape="none", vertex.label.cex=0.7, vertex.label.family="sans", vertex.label.color="steelblue", ...) {
          stopifnot((is.list(x) | is.character(x)), is.logical(all), is.numeric(vertex.size), is.character(vertex.shape), is.numeric(vertex.label.cex))
          #require(igraph)
          
          # generate tree graph and get basic tree layout
          edge.matrix <- edgeMatrix(x,all=all, check.nodenames=check.nodenames)
          gtree <- graph.edgelist(el=edge.matrix, directed=FALSE)
          l <- layout.reingold.tilford(gtree, root=1)
          
          # actual plotting
          plot.igraph2(gtree,
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
		if (nreplacements > 0) {
		      for (i in 1:nreplacements){
		            enc.i <- allenc[i]
		            dec.i <- alldec[i]
		            x <- gsub(enc.i, dec.i, x, fixed=TRUE)
		      }
		}

		
		# decode and replace html-encoded accents etc
		allenc <- unlist(str_extract_all(x, "&[a-z]+;"))
		allenc <- unique(allenc[allenc!=""])
		alldec <- unlist(lapply(allenc, html_accent_decode))
		not_empty <- which(alldec!="")
		alldec <- alldec[not_empty]
		allenc <- allenc[not_empty]
		
		nreplacements <- length(allenc)
		if (nreplacements > 0) {
		      for (i in 1:nreplacements){
		            enc.i <- allenc[i]
		            dec.i <- alldec[i]
		            x <- gsub(enc.i, dec.i, x, fixed=TRUE)
		      }
		}

		return(x)
	}








######## THE FOLLOWING CODE IS TAKEN FROM THE igraph PACKAGE ########
# minor changes by UM, in order to plot vertex labels vertically, following
# the suggestion in https://github.com/igraph/rigraph/issues/106
# if the issue is resolved at some time by incorporating this feature in the igraph package,
# the following code can be removed and the visualize.treedata-functions can be updated accordingly in order
# to incorporate the option of vertical labels.
# The current implementation relies on loading the slighly modified code below (the plot.igraph2 -function and its dependencies)


#   IGraph R package
#   Copyright (C) 2003-2012  Gabor Csardi <csardi.gabor@gmail.com>
#   334 Harvard street, Cambridge, MA 02139 USA

#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc.,  51 Franklin Street, Fifth Floor, Boston, MA
#   02110-1301 USA
#
###################################################################



#' Plotting of graphs
#' 
#' \code{plot.igraph} is able to plot graphs to any R device. It is the
#' non-interactive companion of the \code{tkplot} function.
#' 
#' One convenient way to plot graphs is to plot with \code{\link{tkplot}}
#' first, handtune the placement of the vertices, query the coordinates by the
#' \code{\link{tk_coords}} function and use them with \code{plot} to
#' plot the graph to any R device.
#'
#' @aliases plot.graph
#' @param x The graph to plot.
#' @param axes Logical, whether to plot axes, defaults to FALSE.
#' @param add Logical scalar, whether to add the plot to the current device, or
#' delete the device's current contents first.
#' @param xlim The limits for the horizontal axis, it is unlikely that you want
#' to modify this.
#' @param ylim The limits for the vertical axis, it is unlikely that you want
#' to modify this.
#' @param mark.groups A list of vertex id vectors. It is interpreted as a set
#' of vertex groups. Each vertex group is highlighted, by plotting a colored
#' smoothed polygon around and \dQuote{under} it. See the arguments below to
#' control the look of the polygons.
#' @param mark.shape A numeric scalar or vector. Controls the smoothness of the
#' vertex group marking polygons. This is basically the \sQuote{shape}
#' parameter of the \code{\link[graphics]{xspline}} function, its possible
#' values are between -1 and 1. If it is a vector, then a different value is
#' used for the different vertex groups.
#' @param mark.col A scalar or vector giving the colors of marking the
#' polygons, in any format accepted by \code{\link[graphics]{xspline}}; e.g.
#' numeric color ids, symbolic color names, or colors in RGB.
#' @param mark.border A scalar or vector giving the colors of the borders of
#' the vertex group marking polygons. If it is \code{NA}, then no border is
#' drawn.
#' @param mark.expand A numeric scalar or vector, the size of the border around
#' the marked vertex groups. It is in the same units as the vertex sizes. If a
#' vector is given, then different values are used for the different vertex
#' groups.
#' @param \dots Additional plotting parameters. See \link{igraph.plotting} for
#' the complete list.
#' @return Returns \code{NULL}, invisibly.
#' @author Gabor Csardi \email{csardi.gabor@@gmail.com}
#' @seealso \code{\link{layout}} for different layouts,
#' \code{\link{igraph.plotting}} for the detailed description of the plotting
#' parameters and \code{\link{tkplot}} and \code{\link{rglplot}} for other
#' graph plotting functions.
#' @method plot igraph
#' @export
#' @export plot.igraph
#' @importFrom grDevices rainbow
#' @importFrom graphics plot polygon text par
#' @keywords graphs
#' @examples
#' 
#' g <- ring(10)
#' \dontrun{plot(g, layout=layout_with_kk, vertex.color="green")}
#' 
plot.igraph2 <- function(x, 
                        # SPECIFIC: #####################################
                        axes=FALSE, add=FALSE,
                        xlim=c(-1,1), ylim=c(-1,1),
                        mark.groups=list(), mark.shape=1/2,
                        mark.col=rainbow(length(mark.groups), alpha=0.3),
                        mark.border=rainbow(length(mark.groups), alpha=1),
                        mark.expand=15,
                        ...) {
     
     graph <- x
     if (!is_igraph(graph)) {
          stop("Not a graph object")
     }
     
     ################################################################
     ## Visual parameters
     params <- i.parse.plot.params(graph, list(...))
     vertex.size        <- 1/200 * params("vertex", "size")
     label.family       <- params("vertex", "label.family")
     label.font         <- params("vertex", "label.font")
     label.cex          <- params("vertex", "label.cex")
     label.degree       <- params("vertex", "label.degree")
     label.color        <- params("vertex", "label.color")
     label.dist         <- params("vertex", "label.dist")
     labels             <- params("vertex", "label")
     shape              <- igraph.check.shapes(params("vertex", "shape"))
     
     edge.color         <- params("edge", "color")
     edge.width         <- params("edge", "width")
     edge.lty           <- params("edge", "lty")
     arrow.mode         <- params("edge", "arrow.mode")
     edge.labels        <- params("edge", "label")
     loop.angle         <- params("edge", "loop.angle")
     edge.label.font    <- params("edge", "label.font")
     edge.label.family  <- params("edge", "label.family")
     edge.label.cex     <- params("edge", "label.cex")
     edge.label.color   <- params("edge", "label.color")
     elab.x             <- params("edge", "label.x")
     elab.y             <- params("edge", "label.y")
     arrow.size         <- params("edge", "arrow.size")[1]
     arrow.width        <- params("edge", "arrow.width")[1]
     curved             <- params("edge", "curved")
     if (is.function(curved)) { curved <- curved(graph) }
     
     layout             <- params("plot", "layout")
     margin             <- params("plot", "margin")
     margin <- rep(margin, length=4)
     rescale            <- params("plot", "rescale")
     asp                <- params("plot", "asp")
     frame              <- params("plot", "frame")
     main               <- params("plot", "main")
     sub                <- params("plot", "sub")
     xlab               <- params("plot", "xlab")
     ylab               <- params("plot", "ylab")
     
     palette            <- params("plot", "palette")
     if (!is.null(palette)) {
          old_palette <- palette(palette)
          on.exit(palette(old_palette), add = TRUE)
     }
     
     # the new style parameters can't do this yet
     arrow.mode         <- i.get.arrow.mode(graph, arrow.mode)
     
     ################################################################
     ## create the plot
     maxv <- max(vertex.size)
     if (rescale) {
          # norm layout to (-1, 1)
          layout <- norm_coords(layout, -1, 1, -1, 1)
          xlim <- c(xlim[1]-margin[2]-maxv, xlim[2]+margin[4]+maxv)
          ylim <- c(ylim[1]-margin[1]-maxv, ylim[2]+margin[3]+maxv)
     }
     if (!add) {
          plot(0, 0, type="n", xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
               axes=axes, frame=frame, asp=asp, main=main, sub=sub)
     }
     
     ################################################################
     ## Mark vertex groups
     if (!is.list(mark.groups) && is.numeric(mark.groups)) {
          mark.groups <- list(mark.groups)
     }
     
     mark.shape  <- rep(mark.shape,  length=length(mark.groups))
     mark.border <- rep(mark.border, length=length(mark.groups))
     mark.col    <- rep(mark.col,    length=length(mark.groups))
     mark.expand <- rep(mark.expand, length=length(mark.groups))
     
     for (g in seq_along(mark.groups)) {
          v <- V(graph)[mark.groups[[g]]]
          if (length(vertex.size)==1) {
               vs <- vertex.size
          } else {
               vs <- rep(vertex.size, length=vcount(graph))[v]
          }
          igraph.polygon(layout[v,,drop=FALSE],
                         vertex.size=vs,
                         expand.by=mark.expand[g]/200,
                         shape=mark.shape[g],
                         col=mark.col[g],
                         border=mark.border[g])
     }
     
     ################################################################
     ## calculate position of arrow-heads
     el <- as_edgelist(graph, names=FALSE)
     loops.e <- which(el[,1] == el[,2])
     nonloops.e <- which(el[,1] != el[,2])
     loops.v <- el[,1] [loops.e]
     loop.labels <- edge.labels[loops.e]
     loop.labx <- if (is.null(elab.x)) {
          rep(NA, length(loops.e))
     } else {
          elab.x[loops.e]
     }
     loop.laby <- if (is.null(elab.y)) {
          rep(NA, length(loops.e))
     } else {
          elab.y[loops.e]
     }
     edge.labels <- edge.labels[nonloops.e]
     elab.x <- if (is.null(elab.x)) NULL else elab.x[nonloops.e]
     elab.y <- if (is.null(elab.y)) NULL else elab.y[nonloops.e]
     el <- el[nonloops.e,,drop=FALSE]
     
     edge.coords <- matrix(0, nrow=nrow(el), ncol=4)
     edge.coords[,1] <- layout[,1][ el[,1] ]
     edge.coords[,2] <- layout[,2][ el[,1] ]
     edge.coords[,3] <- layout[,1][ el[,2] ]
     edge.coords[,4] <- layout[,2][ el[,2] ]
     if ( length(unique(shape)) == 1) {
          ## same vertex shape for all vertices
          ec <- .igraph.shapes[[ shape[1] ]]$clip(edge.coords, el,
                                                  params=params, end="both")
     } else {
          ## different vertex shapes, do it by "endpoint"
          shape <- rep(shape, length=vcount(graph))
          ec <- edge.coords
          ec[,1:2] <- t(sapply(seq(length=nrow(el)), function(x) {
               .igraph.shapes[[ shape[el[x,1]] ]]$clip(edge.coords[x,,drop=FALSE],
                                                       el[x,,drop=FALSE],
                                                       params=params, end="from")
          }))
          ec[,3:4] <- t(sapply(seq(length=nrow(el)), function(x) {
               .igraph.shapes[[ shape[el[x,2]] ]]$clip(edge.coords[x,,drop=FALSE],
                                                       el[x,,drop=FALSE],
                                                       params=params, end="to")
          }))
     }
     
     x0 <- ec[,1] ; y0 <- ec[,2] ; x1 <- ec[,3] ; y1 <- ec[,4]
     
     ################################################################
     ## add the loop edges
     if (length(loops.e) > 0) {
          ec <- edge.color
          if (length(ec)>1) { ec <- ec[loops.e] }
          
          point.on.cubic.bezier <- function(cp, t) {
               
               c <- 3 * (cp[2,] - cp[1,])
               b <- 3 * (cp[3,] - cp[2,]) - c
               a <- cp[4,] - cp[1,] - c - b
               
               t2 <- t*t;
               t3 <- t*t*t
               
               a*t3 + b*t2 + c*t + cp[1,]
          }
          
          compute.bezier <- function(cp, points) {
               dt <- seq(0, 1, by=1/(points-1))
               sapply(dt, function(t) point.on.cubic.bezier(cp, t))
          }
          
          plot.bezier <- function(cp, points, color, width, arr, lty, arrow.size, arr.w) {
               p <- compute.bezier( cp, points )
               polygon(p[1,], p[2,], border=color, lwd=width, lty=lty)
               if (arr==1 || arr==3) {
                    igraph.Arrows(p[1,ncol(p)-1], p[2,ncol(p)-1], p[1,ncol(p)], p[2,ncol(p)],
                                  sh.col=color, h.col=color, size=arrow.size,
                                  sh.lwd=width, h.lwd=width, open=FALSE, code=2, width=arr.w)
               }
               if (arr==2 || arr==3) {
                    igraph.Arrows(p[1,2], p[2,2], p[1,1], p[2,1],
                                  sh.col=color, h.col=color, size=arrow.size,
                                  sh.lwd=width, h.lwd=width, open=FALSE, code=2, width=arr.w)
               }
          }
          
          loop <- function(x0, y0, cx=x0, cy=y0, color, angle=0, label=NA,
                           width=1, arr=2, lty=1, arrow.size=arrow.size,
                           arr.w=arr.w, lab.x, lab.y) {
               
               rad <- angle
               center <- c(cx,cy)
               cp <- matrix( c(x0,y0, x0+.4,y0+.2, x0+.4,y0-.2, x0,y0),
                             ncol=2, byrow=TRUE)
               phi <- atan2(cp[,2]-center[2], cp[,1]-center[1])
               r <- sqrt((cp[,1]-center[1])**2 + (cp[,2]-center[2])**2)
               
               phi <- phi + rad
               
               cp[,1] <- cx+r*cos(phi)
               cp[,2] <- cy+r*sin(phi)
               
               plot.bezier(cp, 50, color, width, arr=arr, lty=lty, arrow.size=arrow.size, arr.w=arr.w)
               
               if (is.language(label) || !is.na(label)) {
                    lx <- x0+.3
                    ly <- y0
                    phi <- atan2(ly-center[2], lx-center[1])
                    r <- sqrt((lx-center[1])**2 + (ly-center[2])**2)
                    
                    phi <- phi + rad
                    
                    lx <- cx+r*cos(phi)
                    ly <- cy+r*sin(phi)
                    
                    if (!is.na(lab.x)) { lx <- lab.x }
                    if (!is.na(lab.y)) { ly <- lab.y }
                    
                    text(lx, ly, label, col=edge.label.color, font=edge.label.font,
                         family=edge.label.family, cex=edge.label.cex)
               }
          }
          
          ec <- edge.color
          if (length(ec)>1) { ec <- ec[loops.e] }
          vs <- vertex.size
          if (length(vertex.size)>1) { vs <- vs[loops.v] }
          ew <- edge.width
          if (length(edge.width)>1) { ew <- ew[loops.e] }
          la <- loop.angle
          if (length(loop.angle)>1) { la <- la[loops.e] }
          lty <- edge.lty
          if (length(edge.lty)>1) { lty <- lty[loops.e] }
          arr <- arrow.mode
          if (length(arrow.mode)>1) { arr <- arrow.mode[loops.e] }
          asize <- arrow.size
          if (length(arrow.size)>1) { asize <- arrow.size[loops.e] }
          xx0 <- layout[loops.v,1] + cos(la) * vs
          yy0 <- layout[loops.v,2] - sin(la) * vs
          mapply(loop, xx0, yy0,
                 color=ec, angle=-la, label=loop.labels, lty=lty,
                 width=ew, arr=arr, arrow.size=asize, arr.w=arrow.width,
                 lab.x=loop.labx, lab.y=loop.laby)
     }
     
     ################################################################
     ## non-loop edges
     if (length(x0) != 0) {
          if (length(edge.color)>1) { edge.color <- edge.color[nonloops.e] }
          if (length(edge.width)>1) { edge.width <- edge.width[nonloops.e] }
          if (length(edge.lty)>1) { edge.lty <- edge.lty[nonloops.e] }
          if (length(arrow.mode)>1) { arrow.mode <- arrow.mode[nonloops.e] }
          if (length(arrow.size)>1) { arrow.size <- arrow.size[nonloops.e] }
          if (length(curved)>1) { curved <- curved[nonloops.e] }
          if (length(unique(arrow.mode))==1) {
               lc <-igraph.Arrows(x0, y0, x1, y1, h.col=edge.color, sh.col=edge.color,
                                  sh.lwd=edge.width, h.lwd=1, open=FALSE, code=arrow.mode[1],
                                  sh.lty=edge.lty, h.lty=1, size=arrow.size,
                                  width=arrow.width, curved=curved)
               lc.x <- lc$lab.x
               lc.y <- lc$lab.y
          } else {
               ## different kinds of arrows drawn separately as 'arrows' cannot
               ## handle a vector as the 'code' argument
               curved <- rep(curved, length=ecount(graph))[nonloops.e]
               lc.x <- lc.y <- numeric(length(curved))
               for (code in 0:3) {
                    valid <- arrow.mode==code
                    if (!any(valid)) { next }
                    ec <- edge.color ; if (length(ec)>1) { ec <- ec[valid] }
                    ew <- edge.width ; if (length(ew)>1) { ew <- ew[valid] }
                    el <- edge.lty   ; if (length(el)>1) { el <- el[valid] }
                    lc <- igraph.Arrows(x0[valid], y0[valid], x1[valid], y1[valid],
                                        code=code, sh.col=ec, h.col=ec, sh.lwd=ew, h.lwd=1,
                                        h.lty=1, sh.lty=el, open=FALSE, size=arrow.size,
                                        width=arrow.width, curved=curved[valid])
                    lc.x[valid] <- lc$lab.x
                    lc.y[valid] <- lc$lab.y
               }
          }
          if (!is.null(elab.x)) { lc.x <- ifelse(is.na(elab.x), lc.x, elab.x) }
          if (!is.null(elab.y)) { lc.y <- ifelse(is.na(elab.y), lc.y, elab.y) }
          text(lc.x, lc.y, labels=edge.labels, col=edge.label.color,
               family=edge.label.family, font=edge.label.font, cex=edge.label.cex)
     }
     
     rm(x0, y0, x1, y1)
     
     ################################################################
     # add the vertices
     if (length(unique(shape)) == 1) {
          .igraph.shapes[[ shape[1] ]]$plot(layout, params=params)
     } else {
          sapply(seq(length=vcount(graph)), function(x) {
               .igraph.shapes[[ shape[x] ]]$plot(layout[x,,drop=FALSE], v=x,
                                                 params=params)
          })
     }
     
     ################################################################
     # add the labels
     par(xpd=TRUE)
     x <- layout[,1]+label.dist*cos(-label.degree)* 
          (vertex.size+6*8*log10(2))/200
     y <- layout[,2]+label.dist*sin(-label.degree)*
          (vertex.size+6*8*log10(2))/200
     if (length(label.family)==1) {
          text(x, y, labels=labels, col=label.color, family=label.family,
               font=label.font, cex=label.cex, srt=90) # UM: added srt=90, following the suggestion in https://github.com/igraph/rigraph/issues/106
          # NOTE: if the suggested feature is added to the igraph package at some point, this code should be simplified: igraph-internal code wil become obsolete, but simply adjust the visualize.treedata.vertical function
          
     } else {
          if1 <- function(vect, idx) if (length(vect)==1) vect else vect[idx]
          sapply(seq_len(vcount(graph)), function(v) {
               text(x[v], y[v], labels=if1(labels, v), col=if1(label.color, v),
                    family=if1(label.family, v), font=if1(label.font, v),
                    cex=if1(label.cex, v), srt=90) # UM: added srt=90, following the suggestion in https://github.com/igraph/rigraph/issues/106
               # NOTE: if the suggested feature is added to the igraph package at some point, this code should be simplified: igraph-internal code wil become obsolete, but simply adjust the visualize.treedata.vertical function
               
          })
     }
     rm(x, y)
     invisible(NULL)
}



#' 3D plotting of graphs with OpenGL
#' 
#' Using the \code{rgl} package, \code{rglplot} plots a graph in 3D. The plot
#' can be zoomed, rotated, shifted, etc. but the coordinates of the vertices is
#' fixed.
#' 
#' Note that \code{rglplot} is considered to be highly experimental. It is not
#' very useful either. See \code{\link{igraph.plotting}} for the possible
#' arguments.
#' 
#' @aliases rglplot rglplot.igraph
#' @param x The graph to plot.
#' @param \dots Additional arguments, see \code{\link{igraph.plotting}} for the
#' details
#' @return \code{NULL}, invisibly.
#' @author Gabor Csardi \email{csardi.gabor@@gmail.com}
#' @seealso \code{\link{igraph.plotting}}, \code{\link{plot.igraph}} for the 2D
#' version, \code{\link{tkplot}} for interactive graph drawing in 2D.
#' @export
#' @keywords graphs
#' @export
#' @examples
#' 
#' \dontrun{
#' g <- make_lattice( c(5,5,5) )
#' coords <- layout_with_fr(g, dim=3)
#' rglplot(g, layout=coords)
#' }
#' 
rglplot        <- function(x, ...)
     UseMethod("rglplot", x)

#' @method rglplot igraph
#' @export

rglplot.igraph <- function(x, ...) {
     
     graph <- x
     if (!is_igraph(graph)) {
          stop("Not a graph object")
     }
     
     create.edge <- function(v1, v2, r1, r2, ec, ew, am, as) {
          ## these could also be parameters:
          aw <- 0.005*3*as                      # arrow width
          al <- 0.005*4*as                      # arrow length    
          
          dist <- sqrt(sum((v2-v1)^2))   # distance of the centers
          
          if (am==0) {
               edge <- rgl::qmesh3d(c(-ew/2,-ew/2,dist,1, ew/2,-ew/2,dist,1, ew/2,ew/2,dist,1,
                                      -ew/2,ew/2,dist,1,  -ew/2,-ew/2,0,1, ew/2,-ew/2,0,1,
                                      ew/2,ew/2,0,1, -ew/2,ew/2,0,1),
                                    c(1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8))
          } else if (am==1) {
               edge <- rgl::qmesh3d(c(-ew/2,-ew/2,dist,1, ew/2,-ew/2,dist,1,
                                      ew/2,ew/2,dist,1, -ew/2,ew/2,dist,1,
                                      -ew/2,-ew/2,al+r1,1, ew/2,-ew/2,al+r1,1,
                                      ew/2,ew/2,al+r1,1, -ew/2,ew/2,al+r1,1,
                                      -aw/2,-aw/2,al+r1,1, aw/2,-aw/2,al+r1,1,
                                      aw/2,aw/2,al+r1,1, -aw/2,aw/2,al+r1,1, 0,0,r1,1),
                                    c(1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8,
                                      9,10,11,12, 9,12,13,13, 9,10,13,13, 10,11,13,13,
                                      11,12,13,13))
          } else if (am==2) {
               box <- dist-r2-al
               edge <- rgl::qmesh3d(c(-ew/2,-ew/2,box,1, ew/2,-ew/2,box,1, ew/2,ew/2,box,1,
                                      -ew/2,ew/2,box,1,  -ew/2,-ew/2,0,1, ew/2,-ew/2,0,1,
                                      ew/2,ew/2,0,1, -ew/2,ew/2,0,1,
                                      -aw/2,-aw/2,box,1, aw/2,-aw/2,box,1, aw/2,aw/2,box,1,
                                      -aw/2,aw/2,box,1, 0,0,box+al,1),
                                    c(1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8,
                                      9,10,11,12, 9,12,13,13, 9,10,13,13, 10,11,13,13,
                                      11,12,13,13))
          } else {
               edge <- rgl::qmesh3d(c(-ew/2,-ew/2,dist-al-r2,1, ew/2,-ew/2,dist-al-r2,1,
                                      ew/2,ew/2,dist-al-r2,1, -ew/2,ew/2,dist-al-r2,1,
                                      -ew/2,-ew/2,r1+al,1, ew/2,-ew/2,r1+al,1,
                                      ew/2,ew/2,r1+al,1, -ew/2,ew/2,r1+al,1,
                                      -aw/2,-aw/2,dist-al-r2,1, aw/2,-aw/2,dist-al-r2,1,
                                      aw/2,aw/2,dist-al-r2,1, -aw/2,aw/2,dist-al-r2,1,
                                      -aw/2,-aw/2,r1+al,1, aw/2,-aw/2,r1+al,1,
                                      aw/2,aw/2,r1+al,1, -aw/2,aw/2,r1+al,1,
                                      0,0,dist-r2,1, 0,0,r1,1),
                                    c(1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8,
                                      9,10,11,12, 9,12,17,17, 9,10,17,17, 10,11,17,17,
                                      11,12,17,17,
                                      13,14,15,16, 13,16,18,18, 13,14,18,18, 14,15,18,18,
                                      15,16,18,18))
          }
          
          
          ## rotate and shift it to its position
          phi<- -atan2(v2[2]-v1[2],v1[1]-v2[1])-pi/2
          psi<- acos((v2[3]-v1[3])/dist)    
          rot1 <- rbind(c(1,0,0),c(0,cos(psi),sin(psi)), c(0,-sin(psi),cos(psi)))
          rot2 <- rbind(c(cos(phi),sin(phi),0),c(-sin(phi),cos(phi),0), c(0,0,1))
          rot <- rot1 %*% rot2
          edge <- rgl::transform3d(edge, rgl::rotationMatrix(matrix=rot))
          edge <- rgl::transform3d(edge, rgl::translationMatrix(v1[1], v1[2], v1[3]))
          
          ## we are ready 
          rgl::shade3d(edge, col=ec)
     }
     
     create.loop <- function(v, r, ec, ew, am, la, la2, as) {
          aw <- 0.005*3*as
          al <- 0.005*4*as
          wi <- aw*2                          # size of the loop
          wi2 <- wi+aw-ew                     # size including the arrow heads
          hi <- al*2+ew*2
          gap <- wi-2*ew
          
          if (am==0) {
               edge <- rgl::qmesh3d(c(-wi/2,-ew/2,0,1, -gap/2,-ew/2,0,1,
                                      -gap/2,ew/2,0,1, -wi/2,ew/2,0,1,
                                      -wi/2,-ew/2,hi-ew+r,1, -gap/2,-ew/2,hi-ew+r,1,
                                      -gap/2,ew/2,hi-ew+r,1, -wi/2,ew/2,hi-ew+r,1,
                                      wi/2,-ew/2,0,1, gap/2,-ew/2,0,1,
                                      gap/2,ew/2,0,1, wi/2,ew/2,0,1,
                                      wi/2,-ew/2,hi-ew+r,1, gap/2,-ew/2,hi-ew+r,1,
                                      gap/2,ew/2,hi-ew+r,1, wi/2,ew/2,hi-ew+r,1,
                                      -wi/2,-ew/2,hi+r,1, -wi/2,ew/2,hi+r,1,
                                      wi/2,-ew/2,hi+r,1, wi/2,ew/2,hi+r,1
               ),
               c(1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7,
                 1,4,18,17,
                 9,10,11,12, 13,14,15,16, 9,10,14,13, 10,11,15,14,
                 11,12,16,15, 9,12,20,19,
                 5,13,19,17, 17,18,20,19, 8,16,20,18, 6,7,15,14
               ))
          } else if (am==1 || am==2) {
               edge <- rgl::qmesh3d(c(-wi/2,-ew/2,r+al,1, -gap/2,-ew/2,r+al,1,
                                      -gap/2,ew/2,r+al,1, -wi/2,ew/2,r+al,1,
                                      -wi/2,-ew/2,hi-ew+r,1, -gap/2,-ew/2,hi-ew+r,1,
                                      -gap/2,ew/2,hi-ew+r,1, -wi/2,ew/2,hi-ew+r,1,
                                      wi/2,-ew/2,0,1, gap/2,-ew/2,0,1,
                                      gap/2,ew/2,0,1, wi/2,ew/2,0,1,
                                      wi/2,-ew/2,hi-ew+r,1, gap/2,-ew/2,hi-ew+r,1,
                                      gap/2,ew/2,hi-ew+r,1, wi/2,ew/2,hi-ew+r,1,
                                      -wi/2,-ew/2,hi+r,1, -wi/2,ew/2,hi+r,1,
                                      wi/2,-ew/2,hi+r,1, wi/2,ew/2,hi+r,1,
                                      # the arrow
                                      -wi2/2,-aw/2,r+al,1, -wi2/2+aw,-aw/2,r+al,1,
                                      -wi2/2+aw,aw/2,r+al,1, -wi2/2,aw/2,r+al,1,
                                      -wi2/2+aw/2,0,r,1                   
               ),
               c(1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7,
                 1,4,18,17,
                 9,10,11,12, 13,14,15,16, 9,10,14,13, 10,11,15,14,
                 11,12,16,15, 9,12,20,19,
                 5,13,19,17, 17,18,20,19, 8,16,20,18, 6,7,15,14,
                 # the arrow
                 21,22,23,24, 21,22,25,25, 22,23,25,25, 23,24,25,25,
                 21,24,25,25
               ))
          } else if (am==3) {
               edge <- rgl::qmesh3d(c(-wi/2,-ew/2,r+al,1, -gap/2,-ew/2,r+al,1,
                                      -gap/2,ew/2,r+al,1, -wi/2,ew/2,r+al,1,
                                      -wi/2,-ew/2,hi-ew+r,1, -gap/2,-ew/2,hi-ew+r,1,
                                      -gap/2,ew/2,hi-ew+r,1, -wi/2,ew/2,hi-ew+r,1,
                                      wi/2,-ew/2,r+al,1, gap/2,-ew/2,r+al,1,
                                      gap/2,ew/2,r+al,1, wi/2,ew/2,r+al,1,
                                      wi/2,-ew/2,hi-ew+r,1, gap/2,-ew/2,hi-ew+r,1,
                                      gap/2,ew/2,hi-ew+r,1, wi/2,ew/2,hi-ew+r,1,
                                      -wi/2,-ew/2,hi+r,1, -wi/2,ew/2,hi+r,1,
                                      wi/2,-ew/2,hi+r,1, wi/2,ew/2,hi+r,1,
                                      # the arrows
                                      -wi2/2,-aw/2,r+al,1, -wi2/2+aw,-aw/2,r+al,1,
                                      -wi2/2+aw,aw/2,r+al,1, -wi2/2,aw/2,r+al,1,
                                      -wi2/2+aw/2,0,r,1,
                                      wi2/2,-aw/2,r+al,1, wi2/2-aw,-aw/2,r+al,1,
                                      wi2/2-aw,aw/2,r+al,1, wi2/2,aw/2,r+al,1,
                                      wi2/2-aw/2,0,r,1                   
               ),
               c(1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, 3,4,8,7,
                 1,4,18,17,
                 9,10,11,12, 13,14,15,16, 9,10,14,13, 10,11,15,14,
                 11,12,16,15, 9,12,20,19,
                 5,13,19,17, 17,18,20,19, 8,16,20,18, 6,7,15,14,
                 # the arrows
                 21,22,23,24, 21,22,25,25, 22,23,25,25, 23,24,25,25,
                 21,24,25,25,
                 26,27,28,29, 26,27,30,30, 27,28,30,30, 28,29,30,30,
                 26,29,30,30
               ))
          }
          
          # rotate and shift to its position
          rot1 <- rbind(c(1,0,0),c(0,cos(la2),sin(la2)), c(0,-sin(la2),cos(la2)))
          rot2 <- rbind(c(cos(la),sin(la),0),c(-sin(la),cos(la),0), c(0,0,1))
          rot <- rot1 %*% rot2
          edge <- rgl::transform3d(edge, rgl::rotationMatrix(matrix=rot))
          edge <- rgl::transform3d(edge, rgl::translationMatrix(v[1], v[2], v[3]))
          
          ## we are ready
          rgl::shade3d(edge, col=ec)
     }
     
     # Visual parameters
     params <- i.parse.plot.params(graph, list(...))
     labels <- params("vertex", "label")
     label.color <- params("vertex", "label.color")
     label.font <- params("vertex", "label.font")
     label.degree <- params("vertex", "label.degree")
     label.dist <- params("vertex", "label.dist")
     vertex.color <- params("vertex", "color")
     vertex.size <- (1/200) * params("vertex", "size")
     loop.angle <- params("edge", "loop.angle")
     loop.angle2 <- params("edge", "loop.angle2")
     
     edge.color <- params("edge", "color")
     edge.width <- (1/200) * params("edge", "width")
     edge.labels <- params("edge","label")
     arrow.mode <- params("edge","arrow.mode")
     arrow.size <- params("edge","arrow.size")
     
     layout <- params("plot", "layout")
     rescale <- params("plot", "rescale")
     
     # the new style parameters can't do this yet
     arrow.mode         <- i.get.arrow.mode(graph, arrow.mode)
     
     # norm layout to (-1, 1)
     if (ncol(layout)==2) { layout <- cbind(layout, 0) }
     if (rescale) {
          layout <- norm_coords(layout, -1, 1, -1, 1, -1, 1)
     }
     
     # add the edges, the loops are handled separately
     el <- as_edgelist(graph, names=FALSE)
     
     # It is faster this way
     rgl::par3d(skipRedraw=TRUE)
     
     # edges first
     for (i in seq(length=nrow(el))) {
          from <- el[i,1]
          to   <- el[i,2]
          v1 <- layout[from,]
          v2 <- layout[to,]
          am <- arrow.mode; if (length(am)>1) { am <- am[i] }
          ew <- edge.width; if (length(ew)>1) { ew <- ew[i] }
          ec <- edge.color; if (length(ec)>1) { ec <- ec[i] }
          r1 <- vertex.size; if (length(r1)>1) { r1 <- r1[from] }
          r2 <- vertex.size; if (length(r2)>1) { r2 <- r2[to] }
          
          if (from!=to) {
               create.edge(v1,v2,r1,r2,ec,ew,am,arrow.size)
          } else {
               la <- loop.angle; if (length(la)>1) { la <- la[i] }
               la2 <- loop.angle2; if (length(la2)>1) { la2 <- la2[i] }      
               create.loop(v1,r1,ec,ew,am,la,la2,arrow.size)
          }
          
     }
     
     # add the vertices
     if (length(vertex.size)==1) { vertex.size <- rep(vertex.size, nrow(layout)) }
     rgl::rgl.spheres(layout[,1], layout[,2], layout[,3], radius=vertex.size,
                      col=vertex.color)
     
     # add the labels, 'l1' is a stupid workaround of a mysterious rgl bug
     labels[is.na(labels)] <- ""
     x <- layout[,1]+label.dist*cos(-label.degree)* 
          (vertex.size+6*10*log10(2))/200
     y <- layout[,2]+label.dist*sin(-label.degree)*
          (vertex.size+6*10*log10(2))/200
     z <- layout[,3]
     l1 <- labels[1]
     labels[1] <- ""
     rgl::rgl.texts(x,y,z, labels, col=label.color, adj=0)
     rgl::rgl.texts(c(0,x[1]), c(0,y[1]), c(0,z[1]),
                    c("",l1), col=c(label.color[1],label.color[1]), adj=0)
     
     edge.labels[is.na(edge.labels)] <- ""
     if (any(edge.labels != "")) {
          x0 <- layout[,1][el[,1]]
          x1 <- layout[,1][el[,2]]
          y0 <- layout[,2][el[,1]]
          y1 <- layout[,2][el[,2]]
          z0 <- layout[,3][el[,1]]
          z1 <- layout[,4][el[,2]]
          rgl::rgl.texts((x0+x1)/2, (y0+y1)/2, (z0+z1)/2, edge.labels,
                         col=label.color)
     }
     
     # draw everything
     rgl::par3d(skipRedraw=FALSE)
     
     invisible(NULL)
}

# This is taken from the IDPmisc package,
# slightly modified: code argument added

#' @importFrom graphics par xyinch segments xspline lines polygon

igraph.Arrows <-
     function (x1, y1, x2, y2,
               code=2,
               size= 1,     
               width= 1.2/4/cin,
               open=TRUE,
               sh.adj=0.1, 
               sh.lwd=1,
               sh.col=if(is.R()) par("fg") else 1,
               sh.lty=1,
               h.col=sh.col,
               h.col.bo=sh.col,
               h.lwd=sh.lwd,
               h.lty=sh.lty,
               curved=FALSE)
## Author: Andreas Ruckstuhl, refined by Rene Locher
## Version: 2005-10-17
     {
          cin <- size * par("cin")[2]
          width <- width * (1.2/4/cin)
          uin <- if (is.R()) 
               1/xyinch()
          else par("uin")
          x <- sqrt(seq(0, cin^2, length = floor(35 * cin) + 2))
          delta <-  sqrt(h.lwd)*par("cin")[2]*0.005      ## has been 0.05
          x.arr <- c(-rev(x), -x)
          wx2 <- width * x^2
          y.arr <- c(-rev(wx2 + delta), wx2 + delta)
          deg.arr <- c(atan2(y.arr, x.arr), NA)
          r.arr <- c(sqrt(x.arr^2 + y.arr^2), NA)
          
          ## backup
          bx1 <- x1 ; bx2 <- x2 ; by1 <- y1 ; by2 <- y2
          
          ## shaft
          lx <- length(x1)
          r.seg <- rep(cin*sh.adj, lx)
          theta1 <- atan2((y1 - y2) * uin[2], (x1 - x2) * uin[1])
          th.seg1 <- theta1 + rep(atan2(0, -cin), lx)
          theta2 <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
          th.seg2 <- theta2 + rep(atan2(0, -cin), lx)
          x1d <- y1d <- x2d <- y2d <- 0
          if (code %in% c(1,3)) {
               x2d <- r.seg*cos(th.seg2)/uin[1]
               y2d <- r.seg*sin(th.seg2)/uin[2]
          }
          if (code %in% c(2,3)) {
               x1d <- r.seg*cos(th.seg1)/uin[1]
               y1d <- r.seg*sin(th.seg1)/uin[2]
          }
          if (is.logical(curved) && all(!curved) ||
              is.numeric(curved) && all(!curved)) {
               
               segments(x1+x1d, y1+y1d, x2+x2d, y2+y2d, lwd=sh.lwd, col=sh.col, lty=sh.lty)
               phi <- atan2(y1-y2, x1-x2)
               r <- sqrt( (x1-x2)^2 + (y1-y2)^2 )
               lc.x <- x2 + 2/3*r*cos(phi)
               lc.y <- y2 + 2/3*r*sin(phi)
               
          } else {
               if (is.numeric(curved)) {
                    lambda <- curved
               } else {
                    lambda <- as.logical(curved) * 0.5
               }
               lambda <- rep(lambda, length.out = length(x1))
               c.x1 <- x1+x1d
               c.y1 <- y1+y1d
               c.x2 <- x2+x2d
               c.y2 <- y2+y2d
               
               midx <- (x1+x2)/2
               midy <- (y1+y2)/2  
               spx <- midx - lambda * 1/2 * (c.y2-c.y1)
               spy <- midy + lambda * 1/2 * (c.x2-c.x1)
               sh.col <- rep(sh.col, length=length(c.x1))
               sh.lty <- rep(sh.lty, length=length(c.x1))
               sh.lwd <- rep(sh.lwd, length=length(c.x1))
               lc.x <- lc.y <- numeric(length(c.x1))
               
               for (i in seq_len(length(c.x1))) {
                    ## Straight line?
                    if (lambda[i] == 0) {
                         segments(c.x1[i], c.y1[i], c.x2[i], c.y2[i],
                                  lwd = sh.lwd[i], col = sh.col[i], lty = sh.lty[i])
                         phi <- atan2(y1[i] - y2[i], x1[i] - x2[i])
                         r <- sqrt( (x1[i] - x2[i])^2 + (y1[i] - y2[i])^2 )
                         lc.x[i] <- x2[i] + 2/3*r*cos(phi)
                         lc.y[i] <- y2[i] + 2/3*r*sin(phi)
                         
                    } else {
                         spl <- xspline(x=c(c.x1[i],spx[i],c.x2[i]),
                                        y=c(c.y1[i],spy[i],c.y2[i]), shape=1, draw=FALSE)
                         lines(spl, lwd=sh.lwd[i], col=sh.col[i], lty=sh.lty[i])
                         if (code %in% c(2,3)) {
                              x1[i] <- spl$x[3*length(spl$x)/4]
                              y1[i] <- spl$y[3*length(spl$y)/4]
                         }
                         if (code %in% c(1,3)) {
                              x2[i] <- spl$x[length(spl$x)/4]
                              y2[i] <- spl$y[length(spl$y)/4]
                         }
                         lc.x[i] <- spl$x[2/3 * length(spl$x)]
                         lc.y[i] <- spl$y[2/3 * length(spl$y)]
                    }
               }
          }
          
          ## forward arrowhead
          if (code %in% c(2,3)) {    
               theta <- atan2((by2 - y1) * uin[2], (bx2 - x1) * uin[1])
               Rep <- rep(length(deg.arr), lx)
               p.x2 <- rep(bx2, Rep)
               p.y2 <- rep(by2, Rep)
               ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
               r.arr <- rep(r.arr, lx)  
               if(open) lines((p.x2 + r.arr * cos(ttheta)/uin[1]),
                              (p.y2 + r.arr*sin(ttheta)/uin[2]), 
                              lwd=h.lwd, col = h.col.bo, lty=h.lty) else
                                   polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 + r.arr*sin(ttheta)/uin[2], 
                                           col = h.col, lwd=h.lwd,
                                           border=h.col.bo, lty=h.lty)
          }
          
          ## backward arrow head
          if (code %in% c(1,3)) {
               x1 <- bx1; y1 <- by1
               tmp <- x1 ; x1 <- x2 ; x2 <- tmp
               tmp <- y1 ; y1 <- y2 ; y2 <- tmp
               theta <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
               lx <- length(x1)
               Rep <- rep(length(deg.arr), lx)
               p.x2 <- rep(x2, Rep)
               p.y2 <- rep(y2, Rep)
               ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
               r.arr <- rep(r.arr, lx)
               
               if(open) lines((p.x2 + r.arr * cos(ttheta)/uin[1]),
                              (p.y2 + r.arr*sin(ttheta)/uin[2]), 
                              lwd=h.lwd, col = h.col.bo, lty=h.lty) else
                                   polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 + r.arr*sin(ttheta)/uin[2], 
                                           col = h.col, lwd=h.lwd,
                                           border=h.col.bo, lty=h.lty)
          }
          
          list(lab.x=lc.x, lab.y=lc.y)
          
     } # Arrows

#' @importFrom graphics xspline

igraph.polygon <- function(points, vertex.size=15/200, expand.by=15/200,
                           shape=1/2, col="#ff000033", border=NA) {
     
     by <- expand.by
     pp <- rbind(points,
                 cbind(points[,1]-vertex.size-by, points[,2]),
                 cbind(points[,1]+vertex.size+by, points[,2]),
                 cbind(points[,1], points[,2]-vertex.size-by),
                 cbind(points[,1], points[,2]+vertex.size+by))
     
     cl <- convex_hull(pp)
     xspline(cl$rescoords, shape=shape, open=FALSE, col=col, border=border)
}



#   IGraph R package
#   Copyright (C) 2003-2012  Gabor Csardi <csardi.gabor@gmail.com>
#   334 Harvard street, Cambridge, MA 02139 USA
#   
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc.,  51 Franklin Street, Fifth Floor, Boston, MA
#   02110-1301 USA
#
###################################################################

###################################################################
# Common functions for plot and tkplot
###################################################################

i.parse.plot.params <- function(graph, params) {
     
     ## store the arguments
     p <- list(vertex=list(), edge=list(), plot=list())
     for (n in names(params)) {
          if (substr(n, 1, 7)=="vertex.") {
               nn <- substring(n, 8)
               p[["vertex"]][[nn]] <- params[[n]]
          } else if (substr(n, 1, 5)=="edge.") {
               nn <- substring(n, 6)
               p[["edge"]][[nn]] <- params[[n]]
          } else {
               p[["plot"]][[n]] <- params[[n]]
          }
     }
     
     func <- function(type, name, range=NULL, dontcall=FALSE) {
          if (! type %in% names(p)) {
               stop("Invalid plot option type")
          }
          ret <- function() {
               v <- p[[type]][[name]]
               if (is.function(v) && !dontcall) {
                    v <- v(graph)
               }
               if (is.null(range)) {
                    return (v)        
               } else {
                    if (length(v)==1) {
                         return(rep(v, length(range)))
                    } else {
                         return (rep(v, length=max(range)+1)[[range+1]])
                    }
               }
          }
          if (name %in% names(p[[type]])) {
               ## we already have the parameter
               return(ret())
          } else {
               ## we don't have the parameter, check attributes first
               if (type=="vertex" && name %in% vertex_attr_names(graph)) {
                    p[[type]][[name]] <- vertex_attr(graph, name)
                    return(ret())
               } else if (type=="edge" && name %in% edge_attr_names(graph)) {
                    p[[type]][[name]] <- edge_attr(graph, name)
                    return(ret())
               } else if (type=="plot" && name %in% graph_attr_names(graph)) {
                    p[[type]][[name]] <- graph_attr(graph, name)
                    return(ret())
               } else {
                    ## no attributes either, check igraph parameters
                    n <- paste(sep="", type, ".", name)
                    v <- igraph_opt(n)
                    if (!is.null(v)) {
                         p[[type]][[name]] <- v
                         return(ret())
                    }
                    ## no igraph parameter either, use default value
                    p[[type]][[name]] <- i.default.values[[type]][[name]]
                    return(ret())
               }
          }
          
     }
     
     return (func)
}

i.get.edge.labels <- function(graph, edge.labels=NULL) {
     
     if (is.null(edge.labels)) {
          edge.labels <- rep(NA, ecount(graph))
     }
     
     edge.labels
}

i.get.labels <- function(graph, labels=NULL) {
     
     if (is.null(labels)) {
          if ("name" %in% vertex_attr_names(graph)) {
               labels <- vertex_attr(graph, "name")
          } else {
               labels <- seq_len(vcount(graph))
          }
     }
     labels
}

i.get.arrow.mode <- function(graph, arrow.mode=NULL) {
     
     if (is.character(arrow.mode) &&
         length(arrow.mode)==1 && substr(arrow.mode, 1, 2)=="a:") {
          arrow.mode <- vertex_attr(graph, substring(arrow.mode,3))
     }
     
     if (is.character(arrow.mode)) {
          tmp <- numeric(length(arrow.mode))
          tmp[ arrow.mode %in% c("<", "<-") ] <- 1
          tmp[ arrow.mode %in% c(">", "->") ] <- 2
          tmp[ arrow.mode %in% c("<>", "<->") ] <- 3
          arrow.mode <- tmp
     }
     
     if (is.null(arrow.mode)) {
          if (is_directed(graph)) {
               arrow.mode <- 2
          } else {
               arrow.mode <- 0
          }
     }
     
     arrow.mode
}

i.get.main <- function(graph) {
     if (igraph_opt("annotate.plot")) {
          n <- graph$name[1]
          n
     } else {
          ""
     }
}

i.get.xlab <- function(graph) {
     if (igraph_opt("annotate.plot")) {
          paste(vcount(graph), "vertices,", ecount(graph), "edges")
     } else {
          ""
     }
}

igraph.check.shapes <- function(x) {
     xx <- unique(x)
     bad.shapes <- ! xx %in% ls(.igraph.shapes)
     if (any(bad.shapes)) {
          bs <- paste(xx[bad.shapes], collapse=", ")
          stop("Bad vertex shape(s): ", bs, ".")
     }
     x
}



#' Optimal edge curvature when plotting graphs
#' 
#' If graphs have multiple edges, then drawing them as straight lines does not
#' show them when plotting the graphs; they will be on top of each other. One
#' solution is to bend the edges, with diffenent curvature, so that all of them
#' are visible.
#' 
#' \code{curve_multiple} calculates the optimal \code{edge.curved} vector for
#' plotting a graph with multiple edges, so that all edges are visible.
#'
#' @aliases autocurve.edges
#' @param graph The input graph.
#' @param start The curvature at the two extreme edges. All edges will have a
#' curvature between \code{-start} and \code{start}, spaced equally.
#' @return A numeric vector, its length is the number of edges in the graph.
#' @author Gabor Csardi \email{csardi.gabor@@gmail.com}
#' @seealso \code{\link{igraph.plotting}} for all plotting parameters,
#' \code{\link{plot.igraph}}, \code{\link{tkplot}} and \code{\link{rglplot}}
#' for plotting functions.
#' @export
#' @importFrom stats ave
#' @keywords graphs
#' @examples
#' 
#' g <- graph( c(0,1,1,0,1,2,1,3,1,3,1,3,
#'               2,3,2,3,2,3,2,3,0,1)+1 )
#' 
#' curve_multiple(g)
#' 
#' \dontrun{
#' set.seed(42)
#' plot(g)
#' }
#' 
curve_multiple <- function(graph, start=0.5) {
     el <- apply(as_edgelist(graph, names=FALSE), 1, paste, collapse=":")
     ave(rep(NA, length(el)), el, FUN=function(x) {
          if (length(x) == 1) {
               return(0)
          } else {
               return(seq(-start, start, length=length(x)))
          }
     })
}

.igraph.logo.raster <-
     structure(c(16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 311332508L, 
                 1217499541L, 1804702102L, 1066570390L, 211129749L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 379033495L, 1334940052L, -2104389227L, 
                 -1450012011L, -2087546218L, 1368494484L, 412456341L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 41975936L, 1905496981L, 
                 -141388906L, -7171435L, -7171435L, -7171435L, -325938283L, 1452380564L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 41975936L, 1905496981L, -158166379L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -7171435L, -141389163L, 1972540052L, 41975936L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, -2037148780L, -7171435L, -24798561L, -12009013L, 
                 -13250855L, -11616826L, -24340838L, -7171435L, 1586664085L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 311332508L, -963472747L, -7171435L, 
                 -7171435L, -7171435L, -7171435L, -7236971L, -7171435L, -7171435L, 
                 -7171435L, -7171435L, -946695531L, 361927314L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 798134930L, 
                 -40791403L, -25321308L, -16061704L, -16715521L, -16715521L, -16715521L, 
                 -15408144L, -24471653L, -258829418L, 344755353L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, -1483500650L, -7171435L, -7171435L, -7824996L, -12858668L, 
                 -15212050L, -16519427L, -15212050L, -12858668L, -7890531L, -7171435L, 
                 -7171435L, -1382903147L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 2056426132L, -7171435L, -13643043L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -12139572L, 
                 -7171435L, 1385337493L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 1452380564L, -7171435L, 
                 -7171435L, -8936279L, -15800587L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -15865867L, -9132373L, -7171435L, -7171435L, 
                 1485934996L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, -1433234795L, -7171435L, -15603981L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -14100510L, -7171435L, -2104389227L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, -812412011L, -7171435L, -7432808L, -15080979L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -15277585L, -7498344L, -7171435L, -694971499L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, -1919774060L, 
                 -7171435L, -14623768L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -13120041L, -7171435L, 1704104597L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 177838489L, 
                 -74280299L, -7171435L, -10439750L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -10701380L, -7171435L, -40725867L, 211129749L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 1368494484L, -7171435L, -10374471L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16584963L, -9067350L, 
                 -7171435L, 714248856L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 999527315L, -7171435L, -7171435L, 
                 -12270386L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -12531503L, -7171435L, 
                 -7171435L, 1033015958L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 33554431L, -1080913258L, -7171435L, -10701636L, -15277329L, 
                 -16519427L, -14885141L, -9720911L, -7171435L, -1718381676L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 1217499541L, -7171435L, -7171435L, -12793389L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -13054505L, -7171435L, -7171435L, 1251053972L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 479367826L, -929918315L, -7171435L, -7171435L, -7236971L, -7171435L, 
                 -7171435L, -1366060139L, 227117469L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 361927314L, 
                 -7171435L, -7171435L, -10962753L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -11289661L, -7171435L, -7171435L, 412456341L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 1536398230L, 
                 -7171435L, -778857580L, -1013804395L, -1752067691L, 1334940052L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, -544042347L, 
                 -7171435L, -8086625L, -16061704L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16126983L, -8217439L, 
                 -7171435L, -426601835L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, -1097690475L, -23948651L, 
                 579833750L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 76714645L, 1452446357L, -1986882923L, 
                 -1785556331L, 1720881813L, 361927317L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, -2070703211L, -7171435L, 
                 -7171435L, -10570822L, -16649985L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16649985L, -10636101L, -7171435L, -7171435L, 
                 -2020503147L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 596808338L, -23948651L, -1114467692L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 747803285L, -829255019L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -326004074L, 1418891925L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 127046290L, -728591723L, -7171435L, -7171435L, 
                 -9786446L, -15603981L, -16715521L, -16715521L, -16715521L, -15538958L, 
                 -9655375L, -7171435L, -7171435L, -661482859L, 144678815L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 -2053991786L, -7171435L, 1502778005L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 278041237L, -443444587L, 
                 -7171435L, -10963009L, -14492954L, -15015956L, -12335666L, -24340839L, 
                 -40725867L, 999461525L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 848598164L, -225275243L, -7171435L, -7171435L, -7171435L, 
                 -8347998L, -9720911L, -8348254L, -7171435L, -7171435L, -7171435L, 
                 -225275243L, 949129878L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 61516458L, -443379051L, -292384107L, 
                 127046290L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, -1835887979L, -7171435L, -12008757L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -14492954L, -24013930L, -745368939L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 546279319L, -1114467692L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -7171435L, -7171435L, -1064136043L, 546279319L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 1301451413L, -7171435L, -1835822188L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 -795700587L, -24340838L, -16519427L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -9917004L, -7171435L, 361927317L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 33554431L, 1469289365L, -1752067691L, -896363883L, -242052459L, 
                 -141389163L, -7171435L, -309095531L, 429496729L, 1301451413L, 
                 -2104389227L, -1215130987L, -879586667L, -1701670251L, 1704104597L, 
                 798134930L, 75530368L, 16777215L, -1332571499L, -7171435L, 798134930L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, -174943595L, -9067350L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -11420476L, -7171435L, 
                 999461525L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 -1986948715L, -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, -158166379L, 
                 -1517120875L, -74280299L, -879586667L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, -812477803L, -24340839L, -16519427L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -9851469L, -7171435L, 328372885L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 261724569L, -1248685419L, 
                 -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, -7566182L, 
                 -8355679L, -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -1869376618L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, -1902996843L, 
                 -7171435L, -11681849L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -14166045L, -7236714L, -208498027L, 882086803L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 1150456470L, -493710699L, -7171435L, -7171435L, -7303018L, 
                 -10789959L, -13026608L, -14934812L, -16513548L, -16645131L, -15921426L, 
                 -14013478L, -11973946L, -8618845L, -7171435L, -7171435L, -23948651L, 
                 -1768779114L, 144678815L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 227709589L, -544107883L, -7171435L, 
                 -10570822L, -13969951L, -14492954L, -11943478L, -24210280L, -23948651L, 
                 -7171435L, -23948651L, -1517186668L, 529831060L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 596808338L, -174943595L, 
                 -7171435L, -7171435L, -8684636L, -14605855L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16316174L, -11382080L, -7237226L, -7171435L, -7171435L, -1852665195L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 663917205L, -929918315L, -7171435L, -7171435L, 
                 -7171435L, -7171435L, -393112938L, 1284674197L, 1049661588L, 
                 -879586667L, -7171435L, -141389163L, -1986948715L, 261724569L, 
                 16777215L, 16777215L, 16777215L, 41975936L, -1013804395L, -7171435L, 
                 -7171435L, -11184706L, -16316174L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -14342690L, -8158305L, -7171435L, -23948651L, 
                 1066570390L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 59937429L, 1234342549L, 2140312213L, 
                 -1936551275L, 1486000789L, 294818453L, 16777215L, 16777215L, 
                 33554431L, 1519621014L, -527265131L, -7171435L, -342715755L, 
                 1821545109L, 93952409L, 16777215L, 1922142614L, -7171435L, -7171435L, 
                 -9868880L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -14210851L, -7237227L, -7171435L, 
                 -560819563L, 211129749L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 144678815L, 1989383061L, -258829675L, -7171435L, -644705643L, 
                 1804767894L, -141389163L, -7171435L, -7829349L, -15658261L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -11184706L, -7171435L, -7171435L, -1785622123L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 344755353L, -1835822188L, -91057515L, -7171435L, -7171435L, 
                 -7171435L, -13289772L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16250383L, -8421470L, -7171435L, -292384107L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 647271572L, -409824619L, -7171435L, -7566183L, -16513548L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -11447872L, 
                 -7171435L, -7171435L, 613782933L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 -460090475L, -7171435L, -9342293L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -13421357L, -7171435L, -7171435L, 
                 1502778005L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 965907093L, -1785556331L, -879586667L, -158166379L, -695037291L, 
                 -1584229739L, 1435669141L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 412456341L, -7171435L, 
                 -7171435L, -11184706L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -15263513L, -7171435L, -7171435L, -1903062635L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 143823509L, -1936551275L, -40725867L, 
                 -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -1299017067L, 412258965L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 1200853907L, -7171435L, -7171435L, -12895025L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16579339L, -7566183L, -7171435L, -1114467692L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, -1231908203L, -7171435L, -7171435L, -7171435L, -8282719L, 
                 -9655375L, -8544092L, -7236714L, -7171435L, -7171435L, -577596779L, 
                 194155157L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 747737495L, -7171435L, -7171435L, -11908411L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -15987217L, -7171435L, 
                 -7171435L, -1483566443L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 1720881813L, -7171435L, 
                 -7171435L, -8348254L, -14231324L, -16715521L, -16715521L, -16715521L, 
                 -15212050L, -9263188L, -7171435L, -7171435L, -1768779115L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 61516458L, -158166379L, 
                 -7171435L, -10000462L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -14145060L, -7171435L, -7171435L, -91057515L, 
                 -1315794284L, 1603375510L, 295081622L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 127046293L, -242052459L, -7171435L, -7629158L, 
                 -15538958L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16519427L, -8740442L, -7171435L, -23948651L, 747803285L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, -963472747L, -7171435L, 
                 -8158305L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -12237111L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -74280299L, -1164865131L, 1754502038L, 412456341L, 
                 16777215L, 915575445L, -7171435L, -7171435L, -12008757L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -13773857L, -7171435L, -7171435L, 1720881813L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, -1819110763L, -7171435L, -7171435L, 
                 -15263513L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -9868879L, -7171435L, -74280299L, 1368560277L, -1651338603L, 
                 -325938539L, -7171435L, -7171435L, -7171435L, -40725867L, -1013804395L, 
                 -1382903147L, -7171435L, -7171435L, -14100510L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16061960L, 
                 -7171435L, -7171435L, -1668115819L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 1402180499L, -7171435L, -7171435L, -9539923L, 
                 -16579339L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -13816104L, -7171435L, 
                 -7171435L, -946695531L, 16777215L, 16777215L, 61516458L, 1116967831L, 
                 -1802333548L, -460090475L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -14558233L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16388613L, -7302250L, -7171435L, 
                 -1433234795L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, -1198353770L, -7171435L, -7171435L, -12500278L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -15987217L, -8092514L, -7171435L, -74280299L, 898666645L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 33554431L, 
                 949129878L, -1970105706L, -443379050L, -7171435L, -7171435L, 
                 -12793389L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -14558233L, -7171435L, -7171435L, 1972540053L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 479367826L, -258829675L, -7171435L, -7500391L, -14737438L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16447757L, 
                 -10263627L, -7171435L, -7171435L, -2070703211L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 294818453L, -23948651L, -7171435L, -8478812L, -16323334L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -9917005L, -7171435L, -7171435L, 1083347605L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 1603375510L, 
                 -7171435L, -7171435L, -7434600L, -12237111L, -16513548L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, -16645131L, 
                 -16645131L, -16645131L, -15000603L, -9013337L, -7171435L, -7171435L, 
                 -778923371L, 109084842L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 -1768779115L, -7171435L, -7171435L, -10178634L, -16061960L, -16715521L, 
                 -16715521L, -16715521L, -16388612L, -11224382L, -7171435L, -7171435L, 
                 -997027179L, 43160213L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 33554431L, -728591723L, -7171435L, 
                 -7171435L, -7171435L, -9276502L, -14605855L, -16513549L, -16645131L, 
                 -16645131L, -16645131L, -16645131L, -16645131L, -15789843L, -12171320L, 
                 -7368809L, -7171435L, -7171435L, -376270187L, 781226134L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 194155157L, -577596779L, 
                 -7171435L, -7171435L, -7890531L, -10636100L, -12335666L, -11028288L, 
                 -8413533L, -7171435L, -7171435L, -174943595L, 613585557L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 33554431L, 579833750L, 261724569L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 160337550L, -1416457579L, -7171435L, -7171435L, -124611948L, 
                 -7171435L, -7171435L, -7171435L, -7500391L, -9342293L, -11316288L, 
                 -12171320L, -10263627L, -8355679L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -1416457579L, 344755353L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 647139989L, -913141099L, 
                 -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -476933483L, 1150456469L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 1184010901L, -1131244907L, 
                 -275606891L, -7171435L, -23948651L, -644705643L, -1768779114L, 
                 311332508L, 16777215L, 16777215L, 16777215L, 16777215L, 379033495L, 
                 -929852523L, -7171435L, -23948651L, 2056426132L, 428838809L, 
                 -1282305642L, -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, 
                 -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, -325938539L, 
                 1485934996L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 59937429L, 
                 2039648917L, -711814507L, -40725867L, -7171435L, -7171435L, -510487915L, 
                 -1752001899L, 261264021L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 211129749L, -1701670251L, -7171435L, -7171435L, 
                 -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, -426601835L, 
                 1234408342L, 16777215L, 16777215L, 697274261L, -544042347L, -7171435L, 
                 -124611947L, 1485934996L, 16777215L, 16777215L, 16777215L, 1167365268L, 
                 -2137943659L, -1248619627L, -376270187L, -7171435L, -7171435L, 
                 -91057515L, -846032235L, -1752067691L, 1653772948L, 395350160L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 227709589L, 949129877L, 378704533L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, -1550741099L, -7171435L, -7171435L, -8021089L, 
                 -11616570L, -13446949L, -12662830L, -10178634L, -7171435L, -7171435L, 
                 -91057515L, 831689367L, 1133613460L, -275606891L, -7171435L, 
                 -342715755L, 999527315L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 529831060L, 865178006L, 
                 144678815L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 1368494484L, -7171435L, -7171435L, -9851725L, -15996425L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -13904672L, -7563622L, -7171435L, 
                 -476933483L, -91057514L, -7171435L, -644705643L, 613782933L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, -846032235L, 
                 -7171435L, -8217439L, -16061704L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -12727853L, -7171435L, -7171435L, 
                 -7171435L, -1030581611L, 311332508L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 109084842L, -91057515L, -7171435L, -12139828L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16649985L, -7890531L, -7171435L, -695037291L, 109084842L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 831689367L, -7171435L, -7171435L, -13970208L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -9720911L, -7171435L, -1080913258L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 596808338L, -7171435L, 
                 -7171435L, -13512485L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -9197652L, -7171435L, 
                 -1299017067L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 33554431L, -258829675L, -7171435L, -11355453L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, 
                 -16715521L, -16192519L, -7498343L, -7171435L, 2089980564L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, -1265462635L, -7171435L, -7367273L, -14950677L, -16715521L, 
                 -16715521L, -16715521L, -16715521L, -16715521L, -16715521L, -10897730L, 
                 -7171435L, -7171435L, 1049661588L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 630296984L, 
                 -174943595L, -7171435L, -8086625L, -14100766L, -16715521L, -16715521L, 
                 -16715521L, -16323077L, -11028288L, -7171435L, -7171435L, -1550741099L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 1821545109L, -7171435L, 
                 -7171435L, -7236971L, -8740186L, -10439750L, -9655375L, -7825252L, 
                 -7171435L, -7171435L, -476933483L, 277843855L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 1385337493L, -376270187L, -7171435L, 
                 -7171435L, -7171435L, -7171435L, -7171435L, -7171435L, -1332571499L, 
                 395350160L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 344755353L, 1922142614L, -1533898091L, 
                 -728591723L, -1080913258L, -1903062635L, 1284805780L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 16777215L, 
                 16777215L, 16777215L, 16777215L), .Dim = c(64L, 64L), class = "nativeRaster", channels = 4L)


i.vertex.default <- list(color=1,
                         size=15,
                         size2=15,
                         label=i.get.labels,
                         label.degree=-pi/4,
                         label.color="darkblue",
                         label.dist=0,
                         label.family="serif",
                         label.font=1,
                         label.cex=1,
                         frame.color="black",
                         shape="circle",
                         pie=1,
                         pie.color=list(c("white", "lightblue", "mistyrose",
                                          "lightcyan", "lavender", "cornsilk")),
                         pie.border=list(c("white", "lightblue","mistyrose",
                                           "lightcyan", "lavender", "cornsilk")),
                         pie.angle=45,
                         pie.density=-1,
                         pie.lty=1,
                         raster=.igraph.logo.raster)

i.edge.default <- list(color="darkgrey",
                       label=i.get.edge.labels,
                       lty=1,
                       width=1,
                       loop.angle=0,
                       loop.angle2=0,
                       label.family="serif",
                       label.font=1,
                       label.cex=1,
                       label.color="darkblue",
                       label.x=NULL,
                       label.y=NULL,
                       arrow.size=1,
                       arrow.mode=i.get.arrow.mode,
                       curved=curve_multiple,
                       arrow.width=1)

i.plot.default <- list(palette=categorical_pal(8),
                       layout=layout_nicely,
                       margin=c(0,0,0,0),
                       rescale=TRUE,
                       asp=1,
                       frame=FALSE,
                       main=i.get.main,
                       sub="",
                       xlab=i.get.xlab,
                       ylab="")

i.default.values <- new.env()

i.default.values[["vertex"]] <- i.vertex.default
i.default.values[["edge"]]   <- i.edge.default
i.default.values[["plot"]]   <- i.plot.default





#   IGraph R package
#   Copyright (C) 2003-2012  Gabor Csardi <csardi.gabor@gmail.com>
#   334 Harvard street, Cambridge, MA 02139 USA
#   
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc.,  51 Franklin Street, Fifth Floor, Boston, MA
#   02110-1301 USA
#
###################################################################

###################################################################
## API design
##
## A vertex shape is defined by two functions: the clipping function and
## the plotting function.
##
## The clipping function is called to determine where to put the
## arrowhead of a potential (incoming) incident edge. Its signature is
##    function(coords, el, params, end=c("both", "from", "to"))
## where the arguments are:
##    coords    A matrix with one row for each edge, and four columns.
##              It contains the coordinates of the end points of all
##              edges. The first two columns are the coordinates of the
##              first end points (sources, if the graph is directed),
##              the last two columns are for the other end points
##              (targets if the graph is directed).
##    el        The edge list itself, with vertex ids.
##    params    A function object to query plotting parameters.
##    end       Which end points to calculate. "both" means both,
##              "from" means the first end point, "to" the second.
## The clipping function must return the new version of "coords",
## modified according to the vertex sizes/shapes, with proper positions
## for the potential arrow heads. The positions are for the tips of the
## arrows.
##
## The plotting function plots the vertex. Its signature is
##    function(coords, v=NULL, params)
## where the arguments are
##    coords    Two column matrix, the coordinates for the vertices to draw.
##    v         The vertex ids of the vertices to draw. If NULL, then all
##              vertices are drawn.
##    params    A function object to query plotting parameters.
## 
## shapes()         - lists all vertex shapes
## shapes(shape)    - returns the clipping and plotting functions
##                           for a given vertex shape
## add_shape()             - adds a new vertex shape, the clipping and
##                           plotting functions must be given, and
##                           optionally the newly introduced plotting
##                           parameters. This function can also be used
##                           to overwrite a given vertex shape.
##
## Examples:
## add_shape("image", clip=image.clip, plot=image.plot,
##                   parameters=list(filename=NA))
##
## add_shape("triangle", clip=shapes("circle")$clip,
##                   plot=triangle.plot)
##
## add_shape("polygon", clip=shapes("circle")$clip,
##                   plot=polygon.plot)
##
###################################################################

#' Various vertex shapes when plotting igraph graphs
#'
#' Starting from version 0.5.1 igraph supports different
#' vertex shapes when plotting graphs.
#'
#' @details
#' In igraph a vertex shape is defined by two functions: 1) provides
#' information about the size of the shape for clipping the edges and 2)
#' plots the shape if requested. These functions are called \dQuote{shape
#'   functions} in the rest of this manual page. The first one is the
#' clipping function and the second is the plotting function.
#'
#' The clipping function has the following arguments:
#' \describe{
#'   \item{coords}{A matrix with four columns, it contains the
#'     coordinates of the vertices for the edge list supplied in the
#'     \code{el} argument.}
#'   \item{el}{A matrix with two columns, the edges of which some end
#'     points will be clipped. It should have the same number of rows as
#'     \code{coords}.}
#'   \item{params}{This is a function object that can be called to query
#'     vertex/edge/plot graphical parameters. The first argument of the
#'     function is \dQuote{\code{vertex}}, \dQuote{\code{edge}} or
#'     \dQuote{\code{plot}} to decide the type of the parameter, the
#'     second is a character string giving the name of the
#'     parameter. E.g.
#'     \preformatted{
#'	params("vertex", "size")
#'     }
#'   }
#'   \item{end}{Character string, it gives which end points will be
#'     used. Possible values are \dQuote{\code{both}},
#'     \dQuote{\code{from}} and \dQuote{\code{to}}. If
#'     \dQuote{\code{from}} the function is expected to clip the
#'     first column in the \code{el} edge list, \dQuote{\code{to}}
#'     selects the second column, \dQuote{\code{both}} selects both.}
#' }
#'
#' The clipping function should return a matrix
#' with the same number of rows as the \code{el} arguments.
#' If \code{end} is \code{both} then the matrix must have four
#' columns, otherwise two. The matrix contains the modified coordinates,
#' with the clipping applied.
#'
#' The plotting function has the following arguments:
#' \describe{
#'   \item{coords}{The coordinates of the vertices, a matrix with two
#'     columns.}
#'   \item{v}{The ids of the vertices to plot. It should match the number
#'     of rows in the \code{coords} argument.}
#'   \item{params}{The same as for the clipping function, see above.}
#' }
#'
#' The return value of the plotting function is not used.
#'
#' \code{shapes} can be used to list the names of all installed
#' vertex shapes, by calling it without arguments, or setting the
#' \code{shape} argument to \code{NULL}. If a shape name is given, then
#' the clipping and plotting functions of that shape are returned in a
#' named list.
#'
#' \code{add_shape} can be used to add new vertex shapes to
#' igraph. For this one must give the clipping and plotting functions of
#' the new shape. It is also possible to list the plot/vertex/edge
#' parameters, in the \code{parameters} argument, that the clipping
#' and/or plotting functions can make use of. An example would be a
#' generic regular polygon shape, which can have a parameter for the
#' number of sides.
#'
#' \code{shape_noclip} is a very simple clipping function that the
#' user can use in their own shape definitions. It does no clipping, the
#' edges will be drawn exactly until the listed vertex position
#' coordinates.
#'
#' \code{shape_noplot} is a very simple (and probably not very
#' useful) plotting function, that does not plot anything.
#'
#' @aliases add.vertex.shape igraph.shape.noclip igraph.shape.noplot
#'   vertex.shapes igraph.vertex.shapes
#'
#' @param shape Character scalar, name of a vertex shape. If it is
#'    \code{NULL} for \code{shapes}, then the names of all defined
#'    vertex shapes are returned.
#' @param clip An R function object, the clipping function.
#' @param plot An R function object, the plotting function.
#' @param parameters Named list, additional plot/vertex/edge
#'    parameters. The element named define the new parameters, and the
#'    elements themselves define their default values.
#'    Vertex parameters should have a prefix
#'    \sQuote{\code{vertex.}}, edge parameters a prefix
#'    \sQuote{\code{edge.}}. Other general plotting parameters should have
#'    a prefix \sQuote{\code{plot.}}. See Details below.
#' @param coords,el,params,end,v See parameters of the clipping/plotting
#'    functions below.
#' @return \code{shapes} returns a character vector if the
#'    \code{shape} argument is \code{NULL}. It returns a named list with
#'    entries named \sQuote{clip} and \sQuote{plot}, both of them R
#'    functions.
#'
#'    \code{add_shape} returns \code{TRUE}, invisibly.
#'
#'    \code{shape_noclip} returns the appropriate columns of its
#'    \code{coords} argument.
#' @export
#'
#' @examples
#' # all vertex shapes, minus "raster", that might not be available
#' shapes <- setdiff(shapes(), "")
#' g <- make_ring(length(shapes))
#' set.seed(42)
#' plot(g, vertex.shape=shapes, vertex.label=shapes, vertex.label.dist=1,
#'      vertex.size=15, vertex.size2=15,
#'      vertex.pie=lapply(shapes, function(x) if (x=="pie") 2:6 else 0),
#'      vertex.pie.color=list(heat.colors(5)))
#'
#' # add new vertex shape, plot nothing with no clipping
#' add_shape("nil")
#' plot(g, vertex.shape="nil")
#'
#' #################################################################
#' # triangle vertex shape
#' mytriangle <- function(coords, v=NULL, params) {
#'   vertex.color <- params("vertex", "color")
#'   if (length(vertex.color) != 1 && !is.null(v)) {
#'     vertex.color <- vertex.color[v]
#'   }
#'   vertex.size <- 1/200 * params("vertex", "size")
#'   if (length(vertex.size) != 1 && !is.null(v)) {
#'     vertex.size <- vertex.size[v]
#'   }
#'
#'   symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
#'           stars=cbind(vertex.size, vertex.size, vertex.size),
#'           add=TRUE, inches=FALSE)
#' }
#' # clips as a circle
#' add_shape("triangle", clip=shapes("circle")$clip,
#'                  plot=mytriangle)
#' plot(g, vertex.shape="triangle", vertex.color=rainbow(vcount(g)),
#'      vertex.size=seq(10,20,length=vcount(g)))
#'
#' #################################################################
#' # generic star vertex shape, with a parameter for number of rays
#' mystar <- function(coords, v=NULL, params) {
#'   vertex.color <- params("vertex", "color")
#'   if (length(vertex.color) != 1 && !is.null(v)) {
#'     vertex.color <- vertex.color[v]
#'   }
#'   vertex.size  <- 1/200 * params("vertex", "size")
#'   if (length(vertex.size) != 1 && !is.null(v)) {
#'     vertex.size <- vertex.size[v]
#'   }
#'   norays <- params("vertex", "norays")
#'   if (length(norays) != 1 && !is.null(v)) {
#'     norays <- norays[v]
#'   }
#'
#'   mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
#'          FUN=function(x, y, bg, size, nor) {
#'            symbols(x=x, y=y, bg=bg,
#'                    stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
#'                    add=TRUE, inches=FALSE)
#'          })
#' }
#' # no clipping, edges will be below the vertices anyway
#' add_shape("star", clip=shape_noclip,
#'                  plot=mystar, parameters=list(vertex.norays=5))
#' plot(g, vertex.shape="star", vertex.color=rainbow(vcount(g)),
#'      vertex.size=seq(10,20,length=vcount(g)))
#' plot(g, vertex.shape="star", vertex.color=rainbow(vcount(g)),
#'      vertex.size=seq(10,20,length=vcount(g)),
#'      vertex.norays=rep(4:8, length=vcount(g)))
#'
#' #################################################################
#' # Pictures as vertices.
#' # Similar musicians from last.fm, we start from an artist and
#' # will query two levels. We will use the XML, png and jpeg packages
#' # for this, so these must be available. Otherwise the example is
#' # skipped
#'
#' loadIfYouCan <- function(pkg) suppressWarnings(do.call(require, list(pkg)))
#'
#' if (loadIfYouCan("XML") && loadIfYouCan("png") &&
#'     loadIfYouCan("jpeg")) {
#'   url <- paste(sep="",
#'                'http://ws.audioscrobbler.com/',
#'                '2.0/?method=artist.getinfo&artist=%s',
#'                '&api_key=1784468ada3f544faf9172ee8b99fca3')
#'   getartist <- function(artist) {
#'     cat("Downloading from last.fm. ... ")
#'     txt <- readLines(sprintf(url, URLencode(artist)))
#'     xml <- xmlTreeParse(txt, useInternal=TRUE)
#'     img <- xpathSApply(xml, "/lfm/artist/image[@@size='medium'][1]",
#'                        xmlValue)
#'     if (img != "") {
#'       con <- url(img, open="rb")
#'       bin <- readBin(con, what="raw", n=10^6)
#'       close(con)
#'       if (grepl("\\\\.png$", img)) {
#'         rast <- readPNG(bin, native=TRUE)
#'       } else if (grepl("\\\\.jpe?g$", img)) {
#'         rast <- readJPEG(bin, native=TRUE)
#'       } else {
#'         rast <- as.raster(matrix())
#'       }
#'     } else {
#'       rast <- as.raster(numeric())
#'     }
#'     sim <- xpathSApply(xml, "/lfm/artist/similar/artist/name", xmlValue)
#'     cat("done.\\n")
#'     list(name=artist, image=rast, similar=sim)
#'   }
#'
#'   ego <- getartist("Placebo")
#'   similar <- lapply(ego$similar, getartist)
#'
#'   edges1 <- cbind(ego$name, ego$similar)
#'   edges2 <- lapply(similar, function(x) cbind(x$name, x$similar))
#'   edges3 <- rbind(edges1, do.call(rbind, edges2))
#'   edges <- edges3[ edges3[,1] %in% c(ego$name, ego$similar) &
#'                    edges3[,2] %in% c(ego$name, ego$similar), ]
#'
#'   musnet <- simplify(graph_from_data_frame(edges, dir=FALSE,
#'                      vertices=data.frame(name=c(ego$name, ego$similar))))
#'   print_all(musnet)
#'
#'   V(musnet)$raster <- c(list(ego$image), lapply(similar, "[[", "image"))
#'   plot(musnet, layout=layout_as_star, vertex.shape="raster",
#'        vertex.label=V(musnet)$name, margin=.2,
#'        vertex.size=50, vertex.size2=50,
#'        vertex.label.dist=2, vertex.label.degree=0)
#' } else {
#'   message("You need the `XML', `png' and `jpeg' packages to run this")
#' }

shapes <- function(shape=NULL) {
     if (is.null(shape)) {
          ls(.igraph.shapes)
     } else {
          ## checkScalarString(shape)
          .igraph.shapes[[shape]]
     }
}

#' @rdname shapes
#' @export

shape_noclip <- function(coords, el, params,
                         end=c("both", "from", "to")) {
     end <- igraph.match.arg(end)
     
     if (end=="both") {
          coords
     } else if (end=="from") {
          coords[,1:2,drop=FALSE]
     } else {
          coords[,3:4,drop=FALSE]
     }
}

#' @rdname shapes
#' @export

shape_noplot <- function(coords, v=NULL, params) {
     invisible(NULL)
}

#' @rdname shapes
#' @export

add_shape <- function(shape, clip=shape_noclip,
                      plot=shape_noplot,
                      parameters=list()) {
     
     ## TODO
     ## checkScalarString(shape)
     ## checkFunction(clip)
     ## checkFunction(plot)
     ## checkList(parameters, named=TRUE)
     
     assign(shape, value=list(clip=clip, plot=plot), envir=.igraph.shapes)
     do.call(igraph.options, parameters)
     invisible(TRUE)
}

## These are the predefined shapes

.igraph.shape.circle.clip <- function(coords, el, params,
                                      end=c("both", "from", "to")) {
     
     end <- match.arg(end)
     
     if (length(coords)==0) { return (coords) }     
     
     vertex.size <- 1/200 * params("vertex", "size")
     
     if (end=="from") {
          phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
          vsize.from <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          res <- cbind(coords[,1] + vsize.from*cos(phi),
                       coords[,2] + vsize.from*sin(phi) )
     } else if (end=="to") {
          phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
          r <- sqrt( (coords[,3] - coords[,1])^2 + (coords[,4] - coords[,2])^2 )
          vsize.to <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          res <- cbind(coords[,1] + (r-vsize.to)*cos(phi),
                       coords[,2] + (r-vsize.to)*sin(phi) )
     } else if (end=="both") {
          phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
          r <- sqrt( (coords[,3] - coords[,1])^2 + (coords[,4] - coords[,2])^2 )
          vsize.from <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          vsize.to <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          res <- cbind(coords[,1] + vsize.from*cos(phi),
                       coords[,2] + vsize.from*sin(phi),
                       coords[,1] + (r-vsize.to)*cos(phi),
                       coords[,2] + (r-vsize.to)*sin(phi) )
     }
     res
}

#' @importFrom graphics symbols

.igraph.shape.circle.plot <- function(coords, v=NULL, params) {
     
     vertex.color       <- params("vertex", "color")
     if (length(vertex.color) != 1 && !is.null(v)) {
          vertex.color <- vertex.color[v]
     }
     vertex.frame.color <- params("vertex", "frame.color")
     if (length(vertex.frame.color) != 1 && !is.null(v)) {
          vertex.frame.color <- vertex.frame.color[v]
     }
     vertex.size        <- 1/200 * params("vertex", "size")
     if (length(vertex.size) != 1 && !is.null(v)) {
          vertex.size <- vertex.size[v]
     }
     vertex.size <- rep(vertex.size, length=nrow(coords))
     
     symbols(x=coords[,1], y=coords[,2], bg=vertex.color, fg=vertex.frame.color,
             circles=vertex.size, add=TRUE, inches=FALSE)
}

.igraph.shape.square.clip <- function(coords, el, params,
                                      end=c("both", "from", "to")) {
     end <- match.arg(end)
     
     if (length(coords)==0) { return (coords) }     
     
     vertex.size <- 1/200 * params("vertex", "size")
     
     square.shift <- function(x0, y0, x1, y1, vsize) {
          m <- (y0-y1)/(x0-x1)
          l <- cbind(x1-vsize/m , y1-vsize,
                     x1-vsize , y1-vsize*m,
                     x1+vsize/m, y1+vsize,
                     x1+vsize , y1+vsize*m )
          
          v <- cbind(x1-vsize <= l[,1] & l[,1] <= x1+vsize &
                          y1-vsize <= l[,2] & l[,2] <= y1+vsize,
                     x1-vsize <= l[,3] & l[,3] <= x1+vsize &
                          y1-vsize <= l[,4] & l[,4] <= y1+vsize,
                     x1-vsize <= l[,5] & l[,5] <= x1+vsize &
                          y1-vsize <= l[,6] & l[,6] <= y1+vsize,
                     x1-vsize <= l[,7] & l[,7] <= x1+vsize &
                          y1-vsize <= l[,8] & l[,8] <= y1+vsize)
          
          d <- cbind((l[,1]-x0)^2 + (l[,2]-y0)^2,
                     (l[,3]-x0)^2 + (l[,4]-y0)^2,
                     (l[,5]-x0)^2 + (l[,6]-y0)^2,
                     (l[,7]-x0)^2 + (l[,8]-y0)^2)
          
          t(sapply(seq(length=nrow(l)), function(x) {
               d[x,][!v[x,]] <- Inf
               m <- which.min(d[x,])
               l[x, c(m*2-1, m*2)]
          }))
     }
     
     if (end %in% c("from", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          res <- res1 <- square.shift(coords[,3], coords[,4], coords[,1], coords[,2],
                                      vsize)
     }
     if (end %in% c("to", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          res <- res2 <- square.shift(coords[,1], coords[,2], coords[,3], coords[,4],
                                      vsize)
     }
     if (end=="both") {
          res <- cbind(res1, res2)
     }
     
     res
}

#' @importFrom graphics symbols

.igraph.shape.square.plot <- function(coords, v=NULL, params) {
     
     vertex.color       <- params("vertex", "color")
     if (length(vertex.color) != 1 && !is.null(v)) {
          vertex.color <- vertex.color[v]
     }
     vertex.frame.color <- params("vertex", "frame.color")
     if (length(vertex.frame.color) != 1 && !is.null(v)) {
          vertex.frame.color <- vertex.frame.color[v]
     }
     vertex.size        <- 1/200 * params("vertex", "size")
     if (length(vertex.size) != 1 && !is.null(v)) {
          vertex.size <- vertex.size[v]
     }
     vertex.size <- rep(vertex.size, length=nrow(coords))
     
     symbols(x=coords[,1], y=coords[,2], bg=vertex.color, fg=vertex.frame.color,
             squares=2*vertex.size, add=TRUE, inches=FALSE)
}  

.igraph.shape.csquare.clip <- function(coords, el, params,
                                       end=c("both", "from", "to")) {
     
     end <- match.arg(end)
     
     if (length(coords)==0) { return (coords) }     
     
     vertex.size <- 1/200 * params("vertex", "size")
     
     square.shift <- function(x0, y0, x1, y1, vsize) {
          
          l <- cbind(x1,       y1-vsize,
                     x1-vsize, y1,
                     x1,       y1+vsize,
                     x1+vsize, y1)
          
          d <- cbind((l[,1]-x0)^2 + (l[,2]-y0)^2,
                     (l[,3]-x0)^2 + (l[,4]-y0)^2,
                     (l[,5]-x0)^2 + (l[,6]-y0)^2,
                     (l[,7]-x0)^2 + (l[,8]-y0)^2)
          
          t(sapply(seq(length=nrow(l)), function(x) {
               m <- which.min(d[x,])
               l[x, c(m*2-1, m*2)]
          }))
     }
     
     if (end %in% c("from", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          res <- res1 <- square.shift(coords[,3], coords[,4], coords[,1], coords[,2],
                                      vsize)
     }
     if (end %in% c("to", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          res <- res2 <- square.shift(coords[,1], coords[,2], coords[,3], coords[,4],
                                      vsize)
     }
     if (end=="both") {
          res <- cbind(res1, res2)
     }
     
     res
}

.igraph.shape.csquare.plot <- .igraph.shape.square.plot

.igraph.shape.rectangle.clip <- function(coords, el, params,
                                         end=c("both", "from", "to")) {
     
     end <- match.arg(end)
     
     if (length(coords)==0) { return (coords) }     
     
     vertex.size <- 1/200 * params("vertex", "size")
     vertex.size2 <- 1/200 * params("vertex", "size2")
     
     rec.shift <- function(x0, y0, x1, y1, vsize, vsize2) {
          m <- (y0-y1)/(x0-x1)
          l <- cbind(x1-vsize/m,  y1-vsize2,
                     x1-vsize,    y1-vsize*m,
                     x1+vsize2/m, y1+vsize2,
                     x1+vsize,    y1+vsize*m )
          
          v <- cbind(x1-vsize <= l[,1] & l[,1] <= x1+vsize &
                          y1-vsize2 <= l[,2] & l[,2] <= y1+vsize2,
                     x1-vsize <= l[,3] & l[,3] <= x1+vsize &
                          y1-vsize2 <= l[,4] & l[,4] <= y1+vsize2,
                     x1-vsize <= l[,5] & l[,5] <= x1+vsize &
                          y1-vsize2 <= l[,6] & l[,6] <= y1+vsize2,
                     x1-vsize <= l[,7] & l[,7] <= x1+vsize &
                          y1-vsize2 <= l[,8] & l[,8] <= y1+vsize2)
          
          d <- cbind((l[,1]-x0)^2 + (l[,2]-y0)^2,
                     (l[,3]-x0)^2 + (l[,4]-y0)^2,
                     (l[,5]-x0)^2 + (l[,6]-y0)^2,
                     (l[,7]-x0)^2 + (l[,8]-y0)^2)
          
          t(sapply(seq(length=nrow(l)), function(x) {
               d[x,][!v[x,]] <- Inf
               m <- which.min(d[x,])
               l[x, c(m*2-1, m*2)]
          }))
     }
     
     if (end %in% c("from", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          vsize2 <- if (length(vertex.size2)==1) {
               vertex.size2
          } else {
               vertex.size2[ el[,1] ]
          }
          res <- res1 <- rec.shift(coords[,3], coords[,4], coords[,1], coords[,2],
                                   vsize, vsize2)
     }
     if (end %in% c("to", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          vsize2 <- if (length(vertex.size2)==1) {
               vertex.size2
          } else {
               vertex.size2[ el[,2] ]
          }
          res <- res2 <- rec.shift(coords[,1], coords[,2], coords[,3], coords[,4],
                                   vsize, vsize2)
     }
     if (end=="both") {
          res <- cbind(res1, res2)
     }
     
     res
}

#' @importFrom graphics symbols

.igraph.shape.rectangle.plot <- function(coords, v=NULL, params) {
     
     vertex.color       <- params("vertex", "color")
     if (length(vertex.color) != 1 && !is.null(v)) {
          vertex.color <- vertex.color[v]
     }
     vertex.frame.color <- params("vertex", "frame.color")
     if (length(vertex.frame.color) != 1 && !is.null(v)) {
          vertex.frame.color <- vertex.frame.color[v]
     }
     vertex.size        <- 1/200 * params("vertex", "size")
     if (length(vertex.size) != 1 && !is.null(v)) {
          vertex.size <- vertex.size[v]
     }
     vertex.size <- rep(vertex.size, length=nrow(coords))   
     vertex.size2       <- 1/200 * params("vertex", "size2")
     if (length(vertex.size2) != 1 && !is.null(v)) {
          vertex.size2 <- vertex.size2[v]
     }
     vertex.size <- cbind(vertex.size, vertex.size2)
     
     symbols(x=coords[,1], y=coords[,2], bg=vertex.color, fg=vertex.frame.color,
             rectangles=2*vertex.size, add=TRUE, inches=FALSE)
}

.igraph.shape.crectangle.clip <- function(coords, el, params,
                                          end=c("both", "from", "to")) {
     
     end <- match.arg(end)
     
     if (length(coords)==0) { return (coords) }     
     
     vertex.size <- 1/200 * params("vertex", "size")
     vertex.size2 <- 1/200 * params("vertex", "size2")
     
     rec.shift <- function(x0, y0, x1, y1, vsize, vsize2) {
          
          l <- cbind(x1,       y1-vsize2,
                     x1-vsize, y1,
                     x1,       y1+vsize2,
                     x1+vsize, y1)
          
          d <- cbind((l[,1]-x0)^2 + (l[,2]-y0)^2,
                     (l[,3]-x0)^2 + (l[,4]-y0)^2,
                     (l[,5]-x0)^2 + (l[,6]-y0)^2,
                     (l[,7]-x0)^2 + (l[,8]-y0)^2)
          
          t(sapply(seq(length=nrow(l)), function(x) {
               m <- which.min(d[x,])
               l[x, c(m*2-1, m*2)]
          }))
     }
     
     if (end %in% c("from", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          vsize2 <- if (length(vertex.size2)==1) {
               vertex.size2
          } else {
               vertex.size2[ el[,1] ]
          }
          res <- res1 <- rec.shift(coords[,3], coords[,4], coords[,1], coords[,2],
                                   vsize, vsize2)
     }
     if (end %in% c("to", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          vsize2 <- if (length(vertex.size2)==1) {
               vertex.size2
          } else {
               vertex.size2[ el[,2] ]
          }
          res <- res2 <- rec.shift(coords[,1], coords[,2], coords[,3], coords[,4],
                                   vsize, vsize2)
     }
     if (end=="both") {
          res <- cbind(res1, res2)
     }
     
     res
}

.igraph.shape.crectangle.plot <- .igraph.shape.rectangle.plot

.igraph.shape.vrectangle.clip <- function(coords, el, params,
                                          end=c("both", "from", "to")) {
     
     end <- match.arg(end)
     
     if (length(coords)==0) { return (coords) }     
     
     vertex.size <- 1/200 * params("vertex", "size")
     vertex.size2 <- 1/200 * params("vertex", "size2")
     
     rec.shift <- function(x0, y0, x1, y1, vsize, vsize2) {
          
          l <- cbind(x1-vsize, y1, x1+vsize, y1)
          
          d <- cbind((l[,1]-x0)^2 + (l[,2]-y0)^2,
                     (l[,3]-x0)^2 + (l[,4]-y0)^2)
          
          t(sapply(seq(length=nrow(l)), function(x) {
               m <- which.min(d[x,])
               l[x, c(m*2-1, m*2)]
          }))
     }
     
     if (end %in% c("from", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          vsize2 <- if (length(vertex.size2)==1) {
               vertex.size2
          } else {
               vertex.size2[ el[,1] ]
          }
          res <- res1 <- rec.shift(coords[,3], coords[,4], coords[,1], coords[,2],
                                   vsize, vsize2)
     }
     if (end %in% c("to", "both")) {
          vsize <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          vsize2 <- if (length(vertex.size2)==1) {
               vertex.size2
          } else {
               vertex.size2[ el[,2] ]
          }
          res <- res2 <- rec.shift(coords[,1], coords[,2], coords[,3], coords[,4],
                                   vsize, vsize2)
     }
     if (end=="both") {
          res <- cbind(res1, res2)
     }
     
     res
}    

.igraph.shape.vrectangle.plot <- .igraph.shape.rectangle.plot

.igraph.shape.none.clip <- .igraph.shape.circle.clip

.igraph.shape.none.plot <- function(coords, v=NULL, params) {
     ## does not plot anything at all
     invisible(NULL)
}

#' @importFrom graphics par polygon

mypie <- function(x, y, values, radius, edges=200, col=NULL, angle=45,
                  density=NULL, border=NULL, lty=NULL, init.angle=90, ...) {
     values <- c(0, cumsum(values)/sum(values))
     dx <- diff(values)
     nx <- length(dx)
     twopi <- 2 * pi
     if (is.null(col)) 
          col <- if (is.null(density)) 
               c("white", "lightblue", "mistyrose", "lightcyan", 
                 "lavender", "cornsilk")
     else par("fg")
     col <- rep(col, length.out = nx)
     border <- rep(border, length.out = nx)
     lty <- rep(lty, length.out = nx)
     angle <- rep(angle, length.out = nx)
     density <- rep(density, length.out = nx)
     t2xy <- function(t) {
          t2p <- twopi * t + init.angle * pi/180
          list(x = radius * cos(t2p), y = radius * sin(t2p))
     }
     for (i in 1:nx) {
          n <- max(2, floor(edges * dx[i]))
          P <- t2xy(seq.int(values[i], values[i + 1], length.out = n))
          polygon(x+c(P$x, 0), y+c(P$y, 0), density = density[i], angle = angle[i], 
                  border = border[i], col = col[i], lty = lty[i], ...)
     }
}

.igraph.shape.pie.clip <- function(coords, el, params,
                                   end=c("both", "from", "to")) {
     
     end <- match.arg(end)
     
     if (length(coords)==0) { return (coords) }     
     
     vertex.size <- 1/200 * params("vertex", "size")
     
     if (end=="from") {
          phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
          vsize.from <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          res <- cbind(coords[,1] + vsize.from*cos(phi),
                       coords[,2] + vsize.from*sin(phi) )
     } else if (end=="to") {
          phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
          r <- sqrt( (coords[,3] - coords[,1])^2 + (coords[,4] - coords[,2])^2 )
          vsize.to <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          res <- cbind(coords[,1] + (r-vsize.to)*cos(phi),
                       coords[,2] + (r-vsize.to)*sin(phi) )
     } else if (end=="both") {
          phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
          r <- sqrt( (coords[,3] - coords[,1])^2 + (coords[,4] - coords[,2])^2 )
          vsize.from <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,1] ]
          }
          vsize.to <- if (length(vertex.size)==1) {
               vertex.size
          } else {
               vertex.size[ el[,2] ]
          }
          res <- cbind(coords[,1] + vsize.from*cos(phi),
                       coords[,2] + vsize.from*sin(phi),
                       coords[,1] + (r-vsize.to)*cos(phi),
                       coords[,2] + (r-vsize.to)*sin(phi) )
     }
     
     res
}

#' @importFrom stats na.omit

.igraph.shape.pie.plot <- function(coords, v=NULL, params) {
     
     getparam <- function(pname) {
          p <- params("vertex", pname)
          if (length(p) != 1 && !is.null(v)) {
               p <- p[v]
          }
          p
     }
     vertex.color       <- getparam("color")
     vertex.frame.color <- getparam("frame.color")
     vertex.size        <- rep(1/200 * getparam("size"), length=nrow(coords))
     vertex.pie         <- getparam("pie")
     vertex.pie.color   <- getparam("pie.color")
     vertex.pie.angle   <- getparam("pie.angle")
     vertex.pie.density <- getparam("pie.density")
     vertex.pie.lty     <- getparam("pie.lty")
     
     for (i in seq_len(nrow(coords))) {
          pie <- if(length(vertex.pie)==1) {
               vertex.pie[[1]]
          } else {
               vertex.pie[[i]]
          }
          col <- if (length(vertex.pie.color)==1) {
               vertex.pie.color[[1]]
          } else {
               vertex.pie.color[[i]]
          }
          mypie(x=coords[i,1], y=coords[i,2], pie,
                radius=vertex.size[i], edges=200, col=col,
                angle=na.omit(vertex.pie.angle[c(i,1)])[1],
                density=na.omit(vertex.pie.density[c(i,1)])[1],
                border=na.omit(vertex.frame.color[c(i,1)])[1],
                lty=na.omit(vertex.pie.lty[c(i,1)])[1])
     }
}

.igraph.shape.sphere.clip <- .igraph.shape.circle.clip

#' @importFrom graphics rasterImage
#' @importFrom grDevices col2rgb as.raster

.igraph.shape.sphere.plot <- function(coords, v=NULL, params) {
     
     getparam <- function(pname) {
          p <- params("vertex", pname)
          if (length(p) != 1 && !is.null(v)) {
               p <- p[v]
          }
          p
     }
     vertex.color       <- rep(getparam("color"), length=nrow(coords))
     vertex.size        <- rep(1/200 * getparam("size"), length=nrow(coords))
     
     ## Need to create a separate image for every different vertex color
     allcols <- unique(vertex.color)
     images <- lapply(allcols, function(col) {
          img <- .Call(C_R_igraph_getsphere, pos=c(0.0,0.0,10.0), radius=7.0,
                       color=col2rgb(col)/255, bgcolor=c(0,0,0),
                       lightpos=list(c(-2,2,2)), lightcolor=list(c(1,1,1)),
                       width=100L, height=100L)
          as.raster(img)
     })
     whichImage <- match(vertex.color, allcols)  
     
     for (i in seq_len(nrow(coords))) {
          vsp2 <- vertex.size[i]
          rasterImage(images[[ whichImage[i] ]],
                      coords[i,1]-vsp2, coords[i,2]-vsp2,
                      coords[i,1]+vsp2, coords[i,2]+vsp2)
     }
}

.igraph.shape.raster.clip <- .igraph.shape.rectangle.clip

#' @importFrom graphics rasterImage

.igraph.shape.raster.plot <- function(coords, v=NULL, params) {
     
     getparam <- function(pname) {
          p <- params("vertex", pname)
          if (is.list(p) && length(p) != 1 && !is.null(v)) {
               p <- p[v]
          }
          p
     }
     
     size   <- rep(1/200 * getparam("size"), length=nrow(coords))
     size2  <- rep(1/200 * getparam("size2"), length=nrow(coords))
     raster <- getparam("raster")
     
     for (i in seq_len(nrow(coords))) {
          ras <- if (!is.list(raster) || length(raster)==1) raster else raster[[i]]
          rasterImage(ras, coords[i,1]-size[i], coords[i,2]-size2[i],
                      coords[i,1]+size[i], coords[i,2]+size2[i])
     }
}

.igraph.shapes <- new.env()
.igraph.shapes[["circle"]] <- list(clip=.igraph.shape.circle.clip,
                                   plot=.igraph.shape.circle.plot)
.igraph.shapes[["square"]] <- list(clip=.igraph.shape.square.clip,
                                   plot=.igraph.shape.square.plot)
.igraph.shapes[["csquare"]] <- list(clip=.igraph.shape.csquare.clip,
                                    plot=.igraph.shape.csquare.plot)
.igraph.shapes[["rectangle"]] <- list(clip=.igraph.shape.rectangle.clip,
                                      plot=.igraph.shape.rectangle.plot)
.igraph.shapes[["crectangle"]] <- list(clip=.igraph.shape.crectangle.clip,
                                       plot=.igraph.shape.crectangle.plot)
.igraph.shapes[["vrectangle"]] <- list(clip=.igraph.shape.vrectangle.clip,
                                       plot=.igraph.shape.vrectangle.plot)
.igraph.shapes[["none"]] <- list(clip=.igraph.shape.none.clip,
                                 plot=.igraph.shape.none.plot)
.igraph.shapes[["pie"]] <- list(clip=.igraph.shape.pie.clip,
                                plot=.igraph.shape.pie.plot)
.igraph.shapes[["sphere"]] <- list(clip=.igraph.shape.sphere.clip,
                                   plot=.igraph.shape.sphere.plot)
.igraph.shapes[["raster"]] <- list(clip=.igraph.shape.raster.clip,
                                   plot=.igraph.shape.raster.plot)

