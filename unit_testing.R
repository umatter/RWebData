##########################################################
# Unit testing of RWebData v 0.2.1
#
# Run the unit tests in this file via test_file
#
# January 2018
##########################################################


# cases that should work
cases <- c("microcapital.json",
           "JSON_fiction.json", 
           "microcapital.xml",
           "XML_fiction.xml", 
           "microcapital.yaml", 
           "YAML_fiction.yml")
cases_paths <- lapply(cases, function(x) system.file("exdata", x,  package= "RWebData"))

# unit testing (silent if no errors occur)
test_that("Conversion of raw web data to list of data.frames works", {
      # cases that should work
      expect_is(JSONtoDataFrame(cases_paths[[1]]), class(list()))
      expect_is(JSONtoDataFrame(cases_paths[[2]]), class(list()))
      expect_is(XMLtoDataFrame(cases_paths[[3]]), class(list()))
      expect_is(XMLtoDataFrame(cases_paths[[4]]), class(list()))
      expect_is(YAMLtoDataFrame(cases_paths[[5]]), class(list()))
      expect_is(YAMLtoDataFrame(cases_paths[[6]]), class(list()))
      # test if JSON/YAML return identical results
      expect_identical(YAMLtoDataFrame(cases_paths[[5]]), JSONtoDataFrame(cases_paths[[1]]))
      # test if returned data structure is basically identical for all three raw formats
      expect_output(str(JSONtoDataFrame(cases_paths[[1]])), ":'data.frame':	1 obs. of  2 variables:")
      expect_output(str(XMLtoDataFrame(cases_paths[[3]])), ":'data.frame':	1 obs. of  2 variables:")
      expect_output(str(YAMLtoDataFrame(cases_paths[[5]])), ":'data.frame':	1 obs. of  2 variables:")
      
      # cases that should not work
      expect_error(JSONtoDataFrame("foobar"))
      expect_error(XMLtoDataFrame("foobar"))
      expect_error(YAMLtoDataFrame("foobar"))
})



# Example API calls
example_urls <- 
      c("https://projects.propublica.org/nonprofits/api/v1/organizations/142007220.json"
        ,"https://projects.propublica.org/free-the-files/stations/WEWS-TV.json"
        ,"http://projects.propublica.org/forensics/systems.json"
        ,"https://data.police.uk/api/crimes-street/all-crime?lat=52.629729&lng=-1.131592&date=2017-01"
        ,"https://health.data.ny.gov/resource/cnih-y5dw.json?date=2015-02-11T00%3A00%3A00"
        ,"https://data.consumerfinance.gov/resource/jhzv-w97w.json"
        ,"http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/AUS.B1_GE+P31S14_S15+P3S13+P51+P52_P53+B11+P6+P7.GYSA+GPSA+CTQRGPSA.A/all?startTime=2016&endTime=2017"
        ,"http://ergast.com/api/f1/2013/1/results.json"
        ,"http://search.worldbank.org/api/v2/projects?format=json&qterm=Water&source=IBRD&srt=score&order=desc&kw=N"
        ,"http://api.worldbank.org/countries"
        ,"http://api.worldbank.org/countries?incomeLevel=LMC&format=xml"
        ,"http://api.worldbank.org/countries?incomeLevel=LMC&format=json"
        ,"https://api.github.com/users/hadley/orgs"
        ,"https://api.github.com/users/hadley/repos"
        ,"https://api.github.com/repos/hadley/ggplot2/commits"
        ,"http://citibikenyc.com/stations/json"
        ,'http://ergast.com/api/f1/2012/1/results.json'
        ,"https://zlzlap7j50.execute-api.us-east-1.amazonaws.com/prod"
        ,"https://en.wikipedia.org/w/api.php?action=query&titles=Internet|Switzerland&prop=revisions&format=json"
        ,"https://en.wikipedia.org/w/api.php?action=query&titles=Internet|Switzerland&prop=info&format=xml"
        ,"https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22nome%2C%20ak%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
        ,"http://www.wikia.com/api/v1/Activity/LatestActivity?limit=10&namespaces=0&allowDuplicates=true"
        ,"https://graphical.weather.gov/xml/DWMLgen/schema/latest_DWMLByDay24hr.txt"
        ,"http://content.guardianapis.com/search?api-key=test"
        ,"https://api.discogs.com/artists/1/releases?page=2&per_page=75"
        ,"http://openlibrary.org/people/george08/lists.json"
        ,"https://oc-index.library.ubc.ca/collections/berkpost/items"
        ,"http://domesdaymap.co.uk/api/1.0/area/"
        ,"https://api.opensource.org/license/GPL-3.0"
        ,"https://api.opensource.org/licenses/copyleft"
        ,"https://www.openhumans.org/api/public-data/?username=mpball&limit=5"
      )


# Run unit tests of getTabularData
test_that("Query, parse, and convert data from APIs works", {
      for (i in example_urls) {
            expect_output(print(class(getTabularData(i))), "(data.frame|list)", info = TRUE)
      }
})
