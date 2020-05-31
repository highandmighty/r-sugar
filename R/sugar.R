# R-Sugar
#
# Personal helper functions for R of Arseny Afonin.

#' Airtable Caller
#'
#' Fetches data from a single call to Airtable API into JSON.
#'
#' @inheritParams fetch_airtable
#' @return JSON data if request was successful.
#' @seealso [fetch_airtable()] which is built on top of this function.
#' @examples
#' json = call_airtable(AT_BASE, AT_TABLE, AT_TOKEN, query=list("filterByFormula"="(appsboard=1)"))
call_airtable = function(base, table, token, query) {
  url = paste("https://api.airtable.com/v0", base, table, sep="/")
  r = httr::GET(url, httr::add_headers(Authorization=paste0("Bearer ", token)), query=query)
  if (httr::status_code(r) == 429) {
    print("Airtable API limit reached, 30 seconds break")
    Sys.sleep(30)
    r = httr::rerequest(r)
  }
  return(jsonlite::fromJSON(httr::content(r, as="text", encoding="UTF-8")))
}

#' Airtable Parser
#'
#' Fetches data from Airtable API into dataframe.
#'
#' @param base Airtable base ID e.g. `"app5gby0DG3pF9KbB"`
#' @param table Airtable tablename e.g. `"tablename"`
#' @param token Airtable API token
#' @param query A list of query parameters e.g. `list("filterByFormula"="(appsboard=1)")`
#' @return A dataframe with Airtable data.
#' @seealso [call_airtable()] which this function wraps.
#' @export
#' @examples
#' at = fetch_airtable(AT_BASE, AT_TABLE, AT_TOKEN, query=list("filterByFormula"="(appsboard=1)"))
fetch_airtable = function(..., query=list()) {
  json = call_airtable(..., query=query)
  df = json$records
  df = jsonlite::flatten(df)
  colnames(df) = gsub("fields.", "", colnames(df))

  if ("offset" %in% names(json)) {
    query["offset"] = json$offset
    df_nextpage = fetch_airtable(..., query=query)
    return(dplyr::bind_rows(df, df_nextpage))
  } else {
    return(df)
  }
}

#' Feather from URL Fetcher
#'
#' Fetches Feather file from URL into dataframe.
#'
#' @param url A direct URL of Feather file
#' @return A dataframe with Feather file's data.
#' @export
#' @examples
#' dataframe = download_feather("http://example.com/data/table.feather")
download_feather = function(url) {
  temp = tempfile()
  on.exit(unlink(temp)) # removes temp file after function returns value
  download.file(url, temp, mode="wb")
  df = arrow::read_feather(temp)
  df = df %>% dplyr::mutate_if(bit64::is.integer64, as.integer)
  return(df)
}
