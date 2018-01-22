##########################
# RWebData METHODS
##########################


#------------------------------------
# print method for class APIREQUEST
#------------------------------------


# definition of the print function
print.apirequest <-
      function(x) {
            url <- x@URL
            ra <- x@request.arguments
            
            urlstring <- paste0(url, "\n\n")
            rastring <- "Request arguments:\n"
            
            cat("\nAPI request \n============\n\n")
            cat(urlstring)
            cat(rastring)
            print(x@request.arguments)
      }

# print method
setMethod(f = "print",
          signature = "apirequest",
          definition = print.apirequest
          )




#------------------------------------
# show method for class APIREQUEST
#------------------------------------


# definition of the print function
show.apirequest <-
      function(object) {
            url <- object@URL
            ra <- object@request.arguments
            
            urlstring <- paste0(url, "\n\n")
            rastring <- "Request arguments:\n"
            
            cat("\nAPI request \n============\n\n")
            cat(urlstring)
            cat(rastring)
            print(object@request.arguments)
      }


# print method
setMethod(f = "show",
          signature = "apirequest",
          definition = show.apirequest
          )


#------------------------------------
# print method for class APIRESPONSE
#------------------------------------


# definition of the print function
print.apiresponse <-
      function(x) {
            type <- x@type
            sm <- x@statusMessage
            
            typestring <- paste0("Raw-data type: ", type, ".\n")
            smstring <- paste0("HTTP status message: ", sm, ".")
            
            cat("\nAPI response \n=============\n\n")
            cat(typestring)
            cat(smstring)
      }

# print method
setMethod(f = "print",
          signature = "apiresponse",
          definition = print.apiresponse
          )




#------------------------------------
# show method for class apiresponse
#------------------------------------


# definition of the print function
show.apiresponse <-
      function(object) {
            type <- object@type
            sm <- object@statusMessage
            
            typestring <- paste0("Raw-data type: ", type, ".\n")
            smstring <- paste0("HTTP status message: ", sm, ".")
            
            cat("\nAPI response \n=============\n\n")
            cat(typestring)
            cat(smstring)
      }


# print method
setMethod(f = "show",
          signature = "apiresponse",
          definition = show.apiresponse
          )





#------------------------------------
# summary method for class apiresponse
#------------------------------------


# definition of the print function
summary.apiresponse <-
      function(object){
            x.list <- content2list(object)
            elementlist <- listElements(x.list, na.omit=TRUE)
            levelnames <- c("Toplevel:", paste("Level ", 2:length(elementlist),": ", sep=""))
            
            levelsummaries <- lapply(elementlist, FUN=function(i){
                  sumi <- paste(names(i), " (", as.numeric(i),")", sep="")
                  sumi_string <- paste(sumi, collapse=", ")
                  sumi_string
            }
            )
            
            levelsummaries <- paste(levelsummaries, "\n")
            ind <- 2 # indentation after "Level..."
            exd <- max(nchar(levelnames)) + ind +1 # indentation next paragraph
            levelsummaries <- str_wrap(levelsummaries, width=100, indent=ind, exdent=exd)
            
            s <- new(Class="sapiresponse",
                     summary=list(levelsummaries, levelnames),
                     raw.type=object@type,
                     statusMessage=object@statusMessage)
            
            return(s)
        
        
  }


# summary method
setMethod(f = "summary", signature="apiresponse",
          definition = summary.apiresponse
)




#----------------------------------------
# print method for class SUMMARY.apiresponse
#----------------------------------------


# definition of the print function
print.summary.apiresponse <- 
      function(x) {
            type <- x@raw.type
            sm <- x@statusMessage
            typestring <- paste0("\nRaw-data type: ", type, ".\n")
            smstring <- paste0("HTTP status message: ", sm, ".")
            
            cat("\nAPI response summary: \n====================\n\n")
            
            levelsummaries <- x@summary[[1]]
            levelnames <- x@summary[[2]]
            
            for (j in 1:length(levelsummaries)) {
                  cat(levelsummaries[j], labels=levelnames[j], fill=TRUE)
            }
            
            cat(typestring)
            cat(smstring) 
      }


# print method
setMethod(f = "print",
          signature = "sapiresponse",
          definition = print.summary.apiresponse
          )


#------------------------------------
# show method for class SUMMARY.apiresponse
#------------------------------------


# definition of the print function
show.summary.apiresponse <- 
      function(object) {
            type <- object@raw.type
            sm <- object@statusMessage
            typestring <- paste0("\nRaw-data type: ", type, ".\n")
            smstring <- paste0("HTTP status message: ", sm, ".")
            
            cat("\nAPI response summary: \n====================\n\n")
            
            levelsummaries <- object@summary[[1]]
            levelnames <- object@summary[[2]]
            
            for (j in 1:length(levelsummaries)) {
                  cat(levelsummaries[j], labels=levelnames[j], fill=TRUE)
            }
            
            cat(typestring)
            cat(smstring)
      }



# set show method
setMethod(f = "show",
          signature = "sapiresponse",
          definition = show.summary.apiresponse
          )



#------------------------------------
# plot method for class apiresponse
#------------------------------------

# method definition
plot.apiresponse <-
      function(x, type="normal", char.lim=8, all=FALSE, leveldist=0.15, vertex.size=16,
           vertex.shape="none", vertex.label.cex=0.7, ...) {
            stopifnot((type=="normal" | type=="jitter" | type=="manualscale" | type=="vertical"))
            
            x <- content2list(x)    
            
            if (type=="normal") {
                  visualize.treedata(x=x,
                                     all=all,
                                     vertex.size=vertex.size,
                                     vertex.shape=vertex.shape,
                                     vertex.label.cex=vertex.label.cex, 
                                     ...)
            }
            
            if (type=="jitter") {
                  visualize.treedata.jitter(x=x,
                                            char.lim=char.lim,
                                            leveldist=leveldist,
                                            all=all, vertex.size=vertex.size,
                                            vertex.shape=vertex.shape,
                                            vertex.label.cex=vertex.label.cex,
                                            ...)
                  
            }
            
            if (type=="manualscale") {
                  visualize.treedata.manual(x=x,
                                            all=all,
                                            vertex.size=vertex.size,
                                            vertex.shape=vertex.shape,
                                            vertex.label.cex=vertex.label.cex,
                                            ...)
            }
            
            if (type=="vertical") {
                 visualize.treedata.vertical(x=x,
                                    all=all,
                                    vertex.size=vertex.size,
                                    vertex.shape=vertex.shape,
                                    vertex.label.cex=vertex.label.cex, 
                                    ...)
            }
            
      }

# set plot method
setMethod(f = "plot",
          signature = "apiresponse",
          definition = plot.apiresponse
)





#------------------------------------
# print method for class APIDATA
#------------------------------------


# definition of the print function
print.apidata <- 
      function(x) {
            print(x@data)
      }

# print method
setMethod(f = "print",
          signature = "apidata",
          definition = print.apidata
          )




#------------------------------------
# show method for class APIDATA
#------------------------------------


# definition of the print function
show.apidata <- 
      function(object) {
            print(object@data)
}

# print method
setMethod(f = "show",
          signature = "apidata",
          definition = show.apidata
          )


#------------------------------------
# summary method for class APIDATA
#------------------------------------


# definition of the summary function
summary.apidata <-
      function(object){
            data <- object@data
            totalsummary <- summary(data)
            s.list <- lapply(data,names)
            
            s <- new(Class="sapidata",
                     summary=totalsummary,
                     varsummary=s.list)
            
            return(s)
      }




# summary method
setMethod(f = "summary", signature="apidata",
          definition = summary.apidata
)




#------------------------------------
# print method for class SUMMARY.apidata
#------------------------------------


# definition of the print function
print.summary.apidata <-
      function(x) {
            s <- x@summary
            s.list <- x@varsummary
            ns <- names(s.list)
            ls <- length(s.list)
            
            lsstr <- paste0("The API data has been split into the following ", ls, " data frames:\n\n")
            varstr <- paste0("\nThe respective data frame(s) contain the following variables:\n\n")
            
            cat("\nAPI data summary: \n=================\n\n")
            cat(lsstr)
            print(s)
            cat(varstr)
            
            for (j in 1:length(s.list)) {
                  nsstr <- paste0(j, ". ", ns[j], ":\n")
                  cat(nsstr)
                  cat(paste0(s.list[[j]], sep=", "), fill=TRUE)
                  cat("\n")
            }
      }




# print method
setMethod(f = "print",
          signature = "sapidata",
          definition = print.summary.apidata
)


#------------------------------------
# show method for class SUMMARY.apidata
#------------------------------------


# definition of the print function
show.summary.apidata <- 
      function(object) {
            s <- object@summary
            s.list <- object@varsummary
            ns <- names(s.list)
            ls <- length(s.list)
            
            lsstr <- paste0("The API data has been split into the following ", ls, " data frames:\n\n")
            varstr <- paste0("\nThe respective data frame(s) contain the following variables:\n\n")
            
            
            cat("\nAPI data summary: \n=================\n\n")
            cat(lsstr)
            print(s)
            cat(varstr)
            
            for (j in 1:length(s.list)) {
                  nsstr <- paste0(j, ". ", ns[j], ":\n")
                  cat(nsstr)
                  cat(paste0(s.list[[j]], sep=", "), fill=TRUE)
                  cat("\n")
            }
      }


# set show method
setMethod(f = "show",
          signature = "sapidata",
          definition = show.summary.apidata
          )



#------------------------------------
# plot method for class APIDATA
#------------------------------------

# method definition
plot.apidata <-
      function(x, type="normal", char.lim=8, all=FALSE, leveldist=0.15, vertex.size=16,
           vertex.shape="none", vertex.label.cex=0.7) {
            
            stopifnot((type=="normal" | type=="jitter" | type=="manualscale"))
            
            x <- content2list(x)    
            
            if (type=="normal") {
                  visualize.treedata(x=x,
                                     all=all,
                                     vertex.size=vertex.size,
                                     vertex.shape=vertex.shape,
                                     vertex.label.cex=vertex.label.cex)
            }
            
            if (type=="jitter") {
                  visualize.treedata.jitter(x=x,
                                            char.lim=char.lim,
                                            leveldist=leveldist,
                                            all=all,
                                            vertex.size=vertex.size,
                                            vertex.shape=vertex.shape,
                                            vertex.label.cex=vertex.label.cex)
            }
            
            if (type=="manualscale") {
                  visualize.treedata.manual(x=x,
                                            all=all,
                                            vertex.size=vertex.size,
                                            vertex.shape=vertex.shape,
                                            vertex.label.cex=vertex.label.cex)
            }
      }


# set plot method
setMethod(f = "plot",
          signature = "apidata",
          definition = plot.apidata
          )
            

