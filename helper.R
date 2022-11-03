library(tidyverse)
library(httr)
library(readxl)


##---- estat-api ----
app_id <- "b38c56811d2b40f39a03492d3ec3d66c2259c4e1"
endpoint <- "https://api.e-stat.go.jp/rest/"
ver <- "3.0"
app_type <- "/app/json/"

estat_api <- function(api_method, params = NULL){
  URL <- paste0(endpoint, ver, app_type, api_method)
  
  queries <- list(
    "appId" = app_id
  )
  if(!is.null(params)) {
    queries <- c(queries, params)
  }
  
  res <- httr::GET(url = URL, query = queries)
  content <- httr::content(res)
  
  content
}


##---- save-table ----
save_table <- function(table, category, name) {
  filename <- str_c(name, ".csv")
  write_csv(
    x = table, 
    file = str_c(category, "table", filename, sep = "/")
  )
}