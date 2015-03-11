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