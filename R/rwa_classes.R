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
