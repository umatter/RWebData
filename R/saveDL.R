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


#----------------------------------------------
# saveDL()
# an internal function for request functions
# to manage a large number of requests.
# the queries are split up and the responses
# saved on disk chunk by chunk
# request.function the function handling the api requests

#-----------------------------------------------


saveDL <-
      function(request.function, request.id, chunksize=100,
               pause=0, backupfile="saveDl.list.Rdata", progress="bar", ... ) {
            
            n <- length(request.id)
            if (n <= chunksize) chunksize <- 1
            rest <- n%%chunksize
            chunks.upper <- seq(from = chunksize, to = n, by = chunksize)
            
            if (rest != 0) {
                  chunks.upper[length(chunks.upper) + 1] <- chunks.upper[length(chunks.upper)] + rest
            }
            
            chunks.lower <- c(1,chunks.upper[-length(chunks.upper)] + 1)
            # prepare for loop over all chunks
            chunks <- data.frame(lower=chunks.lower, upper=chunks.upper)
            if (progress=="bar") {
                  pb <- txtProgressBar(min = 0, max = nrow(chunks), style = 3)
            }
            
            response.list <- as.list(1:nrow(chunks))
            save(response.list, file=backupfile) # to be saved and loaded in each loop
            
            # process queries chunkwise
            for (i in 1:nrow(chunks)) {
                  Sys.sleep(pause)
                  
                  first <- chunks$lower[i]
                  last <- chunks$upper[i]
                  rIds <- request.id[first:last]
                  response <- request.function(rIds, ...)
                  
                  load(backupfile)
                  response.list[[i]] <- response
                  save(response.list, file=backupfile)
                  rm(response.list )
                  gc(verbose=FALSE) # clean memory
                  
                  if (progress=="bar") {
                        setTxtProgressBar(pb, i)
                  }
                  
                  if (progress=="text") {
                        cat(paste0("\rData up to request number ", last,
                                   " saved in backup-file: ", backupfile, "."))
                        flush.console()
                  }
            }
            
            load(backupfile)
            response.list <- redlist(response.list)
            
            return(response.list)
      
      }
