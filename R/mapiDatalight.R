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


## Query data from an API
## 
##  A vectorized version of apiDatalight(), wrapped around apiDatalight()
## @usage mapiDatalight(x)
## @param x list of apirequest objects or urls
## @param alignVariables logical, indicating whether variables/values should be rearranged in case the raw data was malformed (missing variable names)
## @return a (nested) list containing the returned data in a flat representation
## @export
## @examples
## \dontrun{apidata <- mapiDatalight(list(mr,mr2)) # only works with a proper PVS API key}


mapiDatalight <-
      function(x, alignVariables=FALSE, ...) {
            resp.list <- lapply(x, apiDatalight, alignVariables, ...)
            
            return(resp.list)
      }
