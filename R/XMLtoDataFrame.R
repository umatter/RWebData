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


##' Extract the data from a XML document as one (or several) data frame(s)
##' 
##' Given a XML document, this function maps the potentially nested XML data to one or several data frames.
##' The data extraction algorithm applied in this function is based on the assumption that the XML document describes either one or several entity types containing a set of observations described by different variables.   
##' @usage XMLtoDataFrame(x, alignVariables=FALSE)
##' @param x a string with the path of a XML document
##' @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
##' @return one data frame or a list of several data frames
##' @details The data extraction algorithm applied in this function partly relies on a nested (tree-structured) data representation \cr
##'          it is favorable for element based XML. XML documents that largely build on attributes can also be processed. \cr
##'          Several attributes of the same tag will, however, be collected in the same data-frame column.
##' @export
##' @examples
##' XML.ex <- system.file("exdata", "microcapital.xml", package = "RWebData")
##' XMLtoDataFrame(XML.ex, alignVariables=FALSE)



XMLtoDataFrame <-
      function(x, alignVariables=FALSE) {
            if (file.exists(x)){
                  body <- readLines(x)
                  } else {
                        body <- x
                  }
            x.list <- xml2list(body)
            x.df <- listToDataFrame(x.list, alignVariables=FALSE)
            return(x.df)
      }

