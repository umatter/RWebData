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


##' Save API keys
##' 
##'  A function that saves the api key as character in a specific variable in the API keys environment.
##' @usage saveAPIkey(key.var, key)
##' @param key.var a character string with the variable name the key should be saved in.
##' @param key the api-key as character string
##' @return either one  data frame or a list containing several data.frames into which the tree structured web-data has been transformed.
##' @details The name of the environment the key is assigned to defaults to "apikeys".
##' @export
##' @examples
##' saveAPIkey(key.var="disq", key="1234" )
##' get("disq", pos="apikeys")


saveAPIkey <-
      function(key.var, key) {
            stopifnot(is.character(key.var), is.character(key))
            
            if ("apikeys" %in% search()) {
                  
                  assign(key.var, key, pos="apikeys")
                  
                  } else {
                        
                        # apikeys <- new.env(parent=.GlobalEnv) # does not seem to work from within a function, alternative:
                        apikeys <- list()
                        attach(apikeys)
                        assign(key.var, key, pos="apikeys")
                  }
            
            cat(paste0("API-key saved in ", key.var, "."))
      }