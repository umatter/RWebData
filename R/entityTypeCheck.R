## Internal function to detect entity types
## 
## The function checks whether a nested list (potentially a part of a bigger nested list) is
## in line with what we consider in this context as an entity type (according to several conditions that must be met)
## @usage entityTypeCheck(x)
## @param x a nested list
## @return logical, returns TRUE if nested list is considered an entity type 
## @examples
## voters <- list(list("Peter Kunz", adress="", 0061, mobile=123, office=456), list("Hans Meier", "Freienstrasse", 0072, mobile=56, office=98) )
## places <- list(list(id=1, name="Basel"), list(2, name="Zürich"), list(id=3, "Bern"))
## meta <- list(createdat="01-01-2015", updatedat="04-03-2015")
## x <- list(voters=list(list(voters)), places=places, meta)
## entityTypeCheck(x)


entityTypeCheck <- 
      function(x) {
            
            # first remove unnecessary nodes/nesting structure
            x <- reduceNestingStructure(x)
            
            # Check whether the x contains an entitytype 
            # Several conditions must be met.
            # NOTE: these conditions are crucial to the proper functioning of the approach.
            # They basically describe what must be met that something is considered an entity type
            
            # a) the x has more than one element
            severalElements <- length(x) >1
            
            # b) the names of Subelements are all the same (i.e. all of the same entity type)
            subElementNames <- names(x)
            uniqueSubElementName <- unique(subElementNames)
            sameSubElementNames <- length(uniqueSubElementName) == 1
            if (is.null(subElementNames)) {  # if none has a name, this counts as the same name
                  sameSubElementNames <- TRUE
            }
            
            someSameSubElementNames <- FALSE
            if (!sameSubElementNames) { # if not all have the same name, check if at least some do
                  someSameSubElementNames <- anyDuplicated(subElementNames) >0
                  
            }
            
            # c) the sub-elements or the subsub-elements are themselves atomic (not lists anymore, i.e. consist of entities that consist of data values)
            # x_lessNesting <- reduceNestingStructure(x)
            subElementClasses <- lapply(x, class)
            allAtomic <- all("list"!=subElementClasses)
            

            # the following two versions of the last check are both not optimal yet. decide for one after unit test!
            # d1 is rather brute force and delivers usefull results if 
            # the input-list is very poorly labeled (almost no variable names mentioned)
            # it does rather include data in entity-types instead of in the metadata (is default)
            # the second version is more sensitive to well labeled data that might 
            # wrongly be included in an entity type. it rather includes more in the metadata
            # than in entity-types.
            # d1) the sub-sub-elements do have a repeated pattern (i.e. same variable names showing up repeatedly)
      
            if (allAtomic) { # elements are all atomic? check if they have a repeated pattern
                 subElementNames <- subElementNames[subElementNames!=""] # do not count empty names, reconsider this in unit tests
                 oneInSeveral <- anyDuplicated(subElementNames) > 0
                 
                 # overall condition
                 isEntityType <- all(severalElements, allAtomic, oneInSeveral) 
                 
            } else {
                  subSubElementNames <- unlist(lapply(x, names))
                  subSubElementNames <- subSubElementNames[subSubElementNames!=""]
                  oneInSeveral <- anyDuplicated(subSubElementNames) > 0
                  
                  # c) again, this time check lower level
                  subSubElementClasses <- unlist(lapply(x, lapply, class))
                  allAtomicSub <- all("list"!=subSubElementClasses)
                  someAtomic <- any("list"!=subSubElementClasses) # NOTE: this conditions cause troubles in different cases therefore currently two versions!
                  
                  
                  # overall condition
                  isEntityType <- all(severalElements, (allAtomicSub | someAtomic & sameSubElementNames), oneInSeveral) 
                  # alternative : isEntityType <- all(severalElements, (allAtomicSub & someSameSubElementNames | someAtomic & sameSubElementNames), oneInSeveral)
            }
            


#             # d2) the sub-sub-elements do have a repeated pattern (i.e. same variable names showing up repeatedly)
#             # at least one variable of all needs to occur in each of the subsubelements
#             subSubElementNames <- lapply(x, names)
#             allNames <- unique(unlist(subSubElementNames))
#             oneInEachSub <- FALSE
#             #oneInSeveral <- FALSE
#             N_checked <- 1
#             while(!oneInEach & N_checked <= length(allNames)) {
#                   currentName <- allNames[N_checked]
#                   oneInEachList <- lapply(subSubElementNames, FUN=function(x) {
#                         
#                         currentName %in% x     
#                   } 
#                   )
#                   #oneInSeveral <- sum(as.numeric(unlist(oneInEachList)))>1
#                   oneInEachSub <- all(unlist(oneInEachList))
#                   N_checked <- N_checked + 1
#             }


            return(isEntityType) 
      }