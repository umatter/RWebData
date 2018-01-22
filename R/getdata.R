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


##' Extract API Data
##' 
##'  Extracts the converted data from an apidata object.
##' @usage getdata(x)
##' @param x an object of class apidata
##' @return either a data frame or a list containing data frames.
##' @export
##' @examples
##' \dontrun{apidata <- apiData(x) # only works with a proper PVS API key}
##' \dontrun{getdata(apidata)}


getdata <-
      function(x) {
            stopifnot(is.apidata(x))
            
            return(x@data)
      }