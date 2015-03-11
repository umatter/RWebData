##' Extract the data from a YAML document as one (or several) data frame(s)
##' 
##' Given YAML document, this function maps the potentially nested YAML data to one or several data frames.
##' The data extraction algorithm applied in this function is based on the assumption that the YAML document describes either one or several entity types containing a set of observations described by different variables.   
##' @usage YAMLtoDataFrame(x)
##' @param x either a string containing YAML or the name of a file containing the YAML
##' @return one data frame or a list of several data frames
##' @export
##' @examples
##' system.file("exdata", "YAML_fiction.yaml", package = "RwebAPI")
##' yaml.ex <- system.file("exdata", "YAML_fiction.yaml", package = "RwebAPI")
##' YAMLtoDataFrame(yaml.ex)



YAMLtoDataFrame <-
      function(x) {
            x.list <- yaml.load(x)
            x.vector <- flattenTree(x.list)
            x.df <- auto.tree2flat(x.vector, primary.key.name="RwebAPI_ID", primary.key="a")
            
            return(x.df)
      }
