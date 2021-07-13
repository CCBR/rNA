library(httr)

get_projects<-function(token,user){

  host <- "https://hpcdmeapi.nci.nih.gov:8080/catalog/query"
  config = httr::config(ssl_verifypeer = FALSE)
  content_type <- "json"
  body =
    '{
	        "page" : 1,
	        "pageSize" : 1000,
	        "totalCount" : true
      }'
  
  options(RCurlOptions = list(ssl_verifypeer = FALSE))
  options(rsconnect.check.certificate = FALSE)
  config = httr::config(ssl_verifypeer = FALSE)
  mq <-
    POST(
      add_headers(Authorization = paste0("Bearer ", token)),
      url = host,
      content_type_json(),
      accept_json(),
      body = body,
      config = httr::config(
        ssl_verifyhost = 0,
        ssl_verifypeer = 0
      )
    )
  resp <- fromJSON(content(mq, "text"))
  cats <- resp$catalogs
  catdf <- as.data.frame(cats)
  projects <- catdf[, 1:3]
  project_dat <- catdf[, 4]
  names(project_dat) <- projects[, 3]
  dfm <- data.frame()
  for (ind in 1:length(catdf[, 1])) {
    if (ind == 1) {
      dfm = as.data.frame(catdf[ind, 4])
      names(dfm)[names(dfm) == 'value'] <- catdf[ind, 3]
    } else{
      dfb <- as.data.frame(catdf[ind, 4])
      
      dfm = merge(
        dfm,
        dfb,
        by = "attribute",
        all = TRUE,
        suffixes = c()
      )
      names(dfm)[names(dfm) == 'value'] <- catdf[ind, 3]
      
    }
    
  }
  rownames(dfm) <- dfm$attribute
  dfm$attribute <- NULL
  tdfm <- t(dfm)
  path = rownames(tdfm)
  tdfm = cbind(tdfm, path)
  ndf <- merge(catdf[, 1:3], tdfm, by = "path")
  return(ndf)
}

getfilelocation<-function(pattern,collection,token,user){
  
  host <- paste0("https://hpcdmeapi.nci.nih.gov:8080/dataObject/query",collection)
  config = httr::config(ssl_verifypeer = FALSE)
  content_type <- "json"
  #
  
  body =paste0(
    '{
    "compoundQuery": {
        "operator": "AND",
        "queries":[
            {
                "attribute":"object_name",
                "value": ',pattern,',
                "operator":"LIKE"
            }
        ]
    },
    "detailedResponse": false
}')
  
  options(RCurlOptions = list(ssl_verifypeer = FALSE))
  options(rsconnect.check.certificate = FALSE)
  config = httr::config(ssl_verifypeer = FALSE)
  mq <-
    POST(
      add_headers(Authorization = paste0("Bearer ", token)),
      url = host,
      content_type_json(),
      accept_json(),
      body = body,
      config = httr::config(
        ssl_verifyhost = 0,
        ssl_verifypeer = 0
      )
    )
  resp <- fromJSON(content(mq, "text"))
  colfilename<-resp$dataObjectPaths
  return(colfilename)
}
getfile<-function(path,filename,token,user){
  
  host <- paste0("https://hpcdmeapi.nci.nih.gov:8080/dataObject",path,"/download")
  config = httr::config(ssl_verifypeer = FALSE)
  content_type <- "json"
  #
  
  body ='{}'
  
  options(RCurlOptions = list(ssl_verifypeer = FALSE))
  options(rsconnect.check.certificate = FALSE)
  config = httr::config(ssl_verifypeer = FALSE)
  mq <-
    POST(
      add_headers(Authorization = paste0("Bearer ", token)),
      url = host,
      content_type_json(),
      accept("raw"),
      body = body,
      config = httr::config(
        ssl_verifyhost = 0,
        ssl_verifypeer = 0
      ), write_disk(filename,overwrite = TRUE)
    )
  
  return(filename)
}