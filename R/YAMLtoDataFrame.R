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
##' yaml.ex <- system.file("exdata", "YAML_fiction.yml", package = "RWebData")
##' YAMLtoDataFrame(yaml.ex, alignVariables=FALSE)



YAMLtoDataFrame <-
      function(x, alignVariables=FALSE) {
            x.list <- yaml.load_file(x)
            x.df <- listToDataFrame(x.list, alignVariables=FALSE)
            
            return(x.df)
      }
