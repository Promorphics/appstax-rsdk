.LabelQuery = setRefClass("LabelQuery", 
                                contains="Query",
                                methods  = list(  
                                  get = function(id=character()) {
                                    if(!missing(id)) {
                                      if (!.Validate()$isUUID(id)) {
                                        return(InvalidTypeError("UUID", id))
                                      }
                                      result <- session$doGET(path("label",id))
                                    }
                                    else {
                                      result <- getResult(path("label","all"), list(), c("label"))
                                    }
                                    result
                                  },
                                  getAssets = function(id, all = F) {
                                    if (!.Validate()$isUUID(id)) {
                                      return(InvalidTypeError("UUID", id))
                                    }
                                    if (all == F) {
                                      getResult(path("label", id, "assets"), list(), c("asset"))
                                    }
                                    else {
                                      getResult(path("label", id, "assets", "all"), list(), c("asset"))
                                    }
                                  },
                                  getChildren = function(id) {
                                    if (!.Validate()$isUUID(id)) {
                                      return(InvalidTypeError("UUID", id))
                                    }
                                    getResult(path("label", id, "children"), list(), c("label"))
                                  },
                                  getRoots = function() {
                                    getResult(path("label", "roots"), list(), c("label"))
                                  },
                                  getSubtree = function(id, depth = numeric()) {
                                    if (!.Validate()$isUUID(id)) {
                                      return(InvalidTypeError("UUID", id))
                                    }
                                    if (!missing(depth)) {
                                      params = list(depth = depth)
                                    }
                                    else {
                                      params = list()
                                    }
                                    getResult(path("label", id, "subtree"), params)
                                  }
                                ))

.LabelGroupQuery = setRefClass("LabelGroupQuery", 
                               contains="Query",
                               methods  = list(  
                                 get = function(id=character()) {
                                   if(!missing(id)) {
                                     if (.Validate()$isUUID(id)) {
                                       result <- session$doGET(path("label", "group", id))
                                     }
                                     else {
                                       result <- getResult(path("label", "group", id), list(), c("labelgroup"))
                                     }
                                     
                                   }
                                   else {
                                     result <- getResult(path("label", "group", "all"), list(), c("labelgroup"))
                                   }
                                   result
                                 }
                               ))

.LabelTypeQuery = setRefClass("LabelTypeQuery", 
                               contains="Query",
                               fields = list(
                                 scope = "character"
                               ),
                               methods  = list(  
                                 get = function(id=character()) {
                                   if(!missing(id)) {
                                     if (.Validate()$isUUID(id)) {
                                       result <- getResult(path("label", scope, id), list())
                                     }
                                     else {
                                       result <- getResult(path("label", scope, "find"), list(query = id), c("labeltype"))
                                     } 
                                   }
                                   else {
                                     result <- getResult(path("label", scope, "all"), list(), c("labeltype"))
                                   }
                                   result
                                 },
                                 getLabels = function(id) {
                                   if (!.Validate()$isUUID(id)) {
                                     return(InvalidTypeError("UUID", id))
                                   }
                                   result <- getResult(path("label", scope, id, "labels"), list(), c("label"))
                                 }
                               ))

#' Constructor for Label requests from the SDK session object
#' 
.Session$methods(Label = function() {
  .LabelQuery(session = .self)
})

#' Constructor for LabelGroup requests from the SDK session object
#' 
.LabelQuery$methods(Group = function() {
  .LabelGroupQuery(session = session)
})

#' Constructor for LabelNode requests from the SDK session object
#' 
.LabelQuery$methods(Node = function() {
  .LabelTypeQuery(session = session, scope = "node")
})

#' Constructor for LabelGroup requests from the SDK session object
#' 
.LabelQuery$methods(Tree = function() {
  .LabelTypeQuery(session = session, scope = "tree")
})

#' Constructor for LabelGroup requests from the SDK session object
#' 
.LabelQuery$methods(Scenario = function() {
  .LabelTypeQuery(session = session, scope = "scenario")
})