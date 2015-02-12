###############################
# RwebAPI: Classes and Methods
###############################

# apirequest, a class for API requests
setClass("apirequest",
         slots= c(URL = "character",
                  request.arguments = "data.frame",
                  nodefault.parameters = "data.frame",
                  server = "character"
         ))


# apiresp, a class for API responses
setClass("apiresp",
         slots= c(body = "character",
                  header = "character",
                  type = "character",
                  statusMessage = "character",
                  request.arguments = "data.frame",
                  nodefault.parameters = "data.frame"
                  ))


# sapiresp, a class for API responses summaries
setClass("sapiresp",
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
