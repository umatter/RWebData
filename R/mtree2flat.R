# 
# ' Vectorized version of tree2flat()
# ' 
# '  A function that transforms n tree structured data sets (represented as a nested list) to a list of n R-data frames. (vectorized verion of tree2flat)
# ' @usage mtree2flat(x, ids)
# ' @param x a list with nested lists, each representing tree structured data from an API response (originally JSON or XML).
# ' @param ids a character vector with id-variables to separate individual observations in the entities of x. each character string is the id of the respective list entry in x (i.e. ids[1] is the id of x[[1]]).
# ' @return a list containing one or several data frame(s)
# ' # First, make sure the necessary API key is saved in your R session:
# ' # (This example is based on the Google Places API)
# ' saveAPIkey(key.var="places", key="YOUR-KEY-HERE")
# ' # first create a request function:
# ' server <- "https://maps.googleapis.com/maps/api/place/details/json?"
# ' param <- c("reference", "sensor")
# ' val <- c(NA, "false")
# ' parameters <- data.frame(parameter=param, value=val)
# ' getPlacesRequest <- apiRequestFunction(x=parameters, base.url = server, key.param = "key", key.object = "places")
# ' preq <- getPlacesRequest(reference="CmRYAAAAciqGsTRX1mXRvuXSH2ErwW-jCINE1aLiwP64MCWDN5vkXvXoQGPKldMfmdGyqWSpm7BEYCgDm-iv7Kc2PF7QA7brMAwBbAcqMr5i1f4PwTpaovIZjysCEZTry8Ez30wpEhCNCXpynextCld2EBsDkRKsGhSLayuRyFsex6JA6NPh9dyupoTH3g") 
# ' \dontrun{x <- apiGET(preq)}
# ' \dontrun{x.list <- content2list(x)}
# ' \dontrun{st.list <- msubTree(x=x.list, c("address_components", "photos", "reviews"))}
# ' \dontrun{dfs <- mtree2flat(st.list, c("types", "photo_reference", "author_url"))}


#-------------------------------------------
# 
#     
# # example:
# purl <- "https://maps.googleapis.com/maps/api/place/details/json?reference=CmRYAAAAciqGsTRX1mXRvuXSH2ErwW-jCINE1aLiwP64MCWDN5vkXvXoQGPKldMfmdGyqWSpm7BEYCgDm-iv7Kc2PF7QA7brMAwBbAcqMr5i1f4PwTpaovIZjysCEZTry8Ez30wpEhCNCXpynextCld2EBsDkRKsGhSLayuRyFsex6JA6NPh9dyupoTH3g&sensor=true&key=AIzaSyCf0sjc3D2ni5EVvcmvrseQijCsBK-o0Mc"
# x <- apiGET(purl)
# x.list <- content2list(x$body, type=x$type)
# st.list <- msubTree(x=x.list, c("address_components", "photos", "reviews"))
# mtree2flat(st.list, c("types", "photo_reference", "author_url"))
#-------------------------------------------


mtree2flat <-
function(x, ids) {
    
    stopifnot(length(x)==length(ids), is.list(x), is.character(ids), length(x)>0, length(ids)>0)
    
    lapply(seq_along(x), FUN=function(i){
      
      x.i <- x[[i]]
      id.i <- ids[i]
      
      tree2flat(x.i, id.i)
      
    })
    
  }
