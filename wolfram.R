#' Basic Wolfram Alpha client and result objects
library(XML)
library(plyr)

WolframClient <- setRefClass(
  "WolframClient",
  fields = list(
    app_id = "character"
  ),
  
  methods = list(
    
    initialize = function(app_id) {
      "initialize Wolfram Alpha Client"
      
      app_id <<- app_id
    },
    
    query = function(query, quiet = TRUE) {
      "run a query against the wolfram alpha database"
      
      message("Querying wolfram alpha...")
      wa_url <- paste0("http://api.wolframalpha.com/v2/query?input=", 
                       URLencode(query), "&appid=", app_id)
      
      destfile <- tempfile()
      download.file(wa_url, destfile, method = "wget", quiet = quiet)
      result <- WolframResult(xmlInternalTreeParse(destfile))
      info <- result$get_info()
      
      if (info$success != "true" || info$error != "false")
        stop("There was a problem with this query. Please check the query infos:\n",
             paste(paste(names(info), info, sep = " = "), collapse = "\n"))
      
      return(result)
    }  
  )
)

WolframResult <- setRefClass(
  "WolframResult",
  fields = list(
    doc = "XMLInternalDocument"
  ),
  
  methods = list(
    initialize = function(doc) {
      doc <<- doc
    },
    
    xml = function() doc,
    
    get_info = function() {
      as.list(xpathApply(doc, "//queryresult", xmlAttrs)[[1]])
    },
    
    get_pods = function() {
      xpathApply(doc, "//pod")
    },
    
    get_plaintext = function() {
      ldply(get_pods(), function(pod) {
        data.frame(
          pod = xmlGetAttr(pod, "title"),
          id = xmlGetAttr(pod, "id"),
          plaintext = unlist(xpathApply(pod, "subpod/plaintext", xmlValue))
        )
      })
    }
  )
)

wa <- WolframClient("L5L9AG-36AY3THUVW")
result <- wa$query("Big Mac vs mc donalds hamburger calories")
a <- result$get_plaintext()


