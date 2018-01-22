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

##' Extract the data from a YAML document as one (or several) data frame(s)
##' 
##' Given YAML document, this function maps the potentially nested YAML data to one or several data frames.
##' The data extraction algorithm applied in this function is based on the assumption that the YAML document describes either one or several entity types containing a set of observations described by different variables.   
##' @usage YAMLtoDataFrame(x, alignVariables=FALSE)
##' @param x either a string containing YAML or the name of a file containing the YAML
##' @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
##' @return one data frame or a list of several data frames
##' @export
##' @examples
##' yaml.ex <- system.file("exdata", "microcapital.yaml", package = "RWebData")
##' YAMLtoDataFrame(yaml.ex, alignVariables=FALSE)



YAMLtoDataFrame <-
      function(x, alignVariables=FALSE) {
            if (file.exists(x)){
                  x.list <- yaml.load_file(x)
            } else {
                  x.list <- yaml.load(x)
            }
            x.df <- listToDataFrame(x.list, alignVariables=FALSE)
            
            return(x.df)
      }
