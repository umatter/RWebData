##' Extract the data from a XML document as one (or several) data frame(s)
##' 
##' Given a XML document, this function maps the potentially nested XML data to one or several data frames.
##' The data extraction algorithm applied in this function is based on the assumption that the XML document describes either one or several entity types containing a set of observations described by different variables.   
##' @usage XMLtoDataFrame(x)
##' @param x a string with the path of a XML document
##' @return one data frame or a list of several data frames
##' @details The data extraction algorithm applied in this function partly relies on a nested (tree-structured) data representation \cr
##'          it is favorable for element based XML. XML documents that largely build on attributes can also be processed. \cr
##'          Several attributes of the same tag will, however, be collected in the same data-frame column.
##' @export
##' @examples
##' XML.ex <- system.file("exdata", "XML_fiction.xml", package = "RwebAPI")
##' XMLtoDataFrame(XML.ex)



XMLtoDataFrame <-
      function(x) {
            if (file.exists(x)){
                  body <- readLines(x)
                  } else {
                        body <- x
                  }
            x.list <- xml2list(body)
            x.vector <- flattenTree(x.list)
            x.df <- auto.tree2flat(x.vector)
            return(x.df)
      }

