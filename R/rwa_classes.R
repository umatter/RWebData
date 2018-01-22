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

###############################
# RWebData: Classes and Methods
###############################

# apirequest, a class for API requests
setClass("apirequest",
         slots= c(URL = "character",
                  request.arguments = "data.frame",
                  nodefault.parameters = "data.frame",
                  server = "character"
                  ))


# apiresponse, a class for API responses
setClass("apiresponse",
         slots= c(body = "character",
                  header = "character",
                  type = "character",
                  statusMessage = "character",
                  request.arguments = "data.frame",
                  nodefault.parameters = "data.frame"
                  ))


# sapiresponse, a class for API responses summaries
setClass("sapiresponse",
         slots= c(summary="list",
                  raw.type= "character",
                  statusMessage ="character"
                  ))



# apidata, a class for API treestructured data
setClass("apidata",
         slots= c(data = "list",
                  raw.data = "character",
                  raw.type = "character",
                  request.statusMessage = "character",
                  request.arguments = "data.frame"
                  ))



# sapidata, a class for API data summaries
setClass("sapidata",
         slots= c(summary="summaryDefault",
                  varsummary="list"
                  ))
