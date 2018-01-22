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



##' Check if object is an apidata
##' 
##'  A function to check if an object is of class "apidata".
##' @usage is.apidata(x)
##' @param x any R object
##' @return logical, TRUE if x is of class apidata.
##' @export


is.apidata <-
      function(x) {
            inherits(x, "apidata")
      }
