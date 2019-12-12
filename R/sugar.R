# R-Sugar
#
# This is helper functions for R.
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Airtable Caller
#'
#' Parse data from Airtable via API as JSON.
#'
#' @inheritParams fetch_airtable
#' @return JSON data if request was successful.
#' @seealso [fetch_airtable()] which is built on top of this function.
#' @examples
#' json = call_airtable(AT_BASE, AT_TABLE, AT_TOKEN, query=list("filterByFormula"="(appsboard=1)"))
call_airtable = function(base, table, token, query) {
  url = paste0(base, table)
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
#' Parse data from Airtable via API as dataframe object.
#'
#' @param base Airtable database URL e.g. `"https://api.airtable.com/v0/app5gby0DG3pF9KbB/"`
#' @param table Airtable tablename e.g. `"table1"`
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
  df = jsonlite::flatten(df) # 'rlang' package can mask 'flatten' function
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
#' Fetch Feather file from URL to dataframe.
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
  return(feather::read_feather(temp))
}
