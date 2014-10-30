.AssetQuery = setRefClass("AssetQuery", 
                          contains="Query",
                          methods  = list(  
                            get = function(id=character()) {
                              if(!missing(id)) {
                                if (!.Validate()$isUUID(id)) {
                                  return(InvalidTypeError("UUID", id))
                                }
                                return(session$doGET(path("asset",id)))
                              }
                              else {
                                return(getResult(path("asset","all"), list(), c("asset")))
                              }
                            }
                          ))

.ChannelQuery = setRefClass("ChannelQuery", 
                          contains="Query",
                          methods  = list(  
                            get = function(asset_id, channel_key=character(), include_derived = T) {
                              if (!.Validate()$isUUID(asset_id)) {
                                return(InvalidTypeError("UUID", asset_id))
                              }
                              if (missing(channel_key)) {
                                return(getResult(path("asset", asset_id, "channel", "all"), list(includeDerived = include_derived), c("channel")))
                              }
                              else {
                                return(getResult(path("asset", asset_id, "channel", channel_key), list(), c("channel")))
                              }
                            }
                          ))

.BlueprintQuery = setRefClass("BlueprintQuery", 
                            contains="Query",
                            methods  = list(  
                              get = function(asset_id) {
                                if (!.Validate()$isUUID(asset_id)) {
                                  return(InvalidTypeError("UUID", asset_id))
                                }
                                return(getResult(path("asset", asset_id, "channel", "blueprints"), list(), c("blueprint")))
                              }
                            ))

#' Constructor for Assets request from the SDK session object
#' 
.Session$methods(Asset = function() {
  .AssetQuery(session = .self)
})

.AssetQuery$methods(Channel = function() {
  .ChannelQuery(session = session)
})

.ChannelQuery$methods(Blueprint = function() {
  .BlueprintQuery(session = session)
})