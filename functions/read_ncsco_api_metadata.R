# ---- script header ----
# script name: read_ncsco_api_metadata.R
# purpose of script: defines a function that reads metadata (only) from the NC SCO API
# author: sheila saia
# date created: 20200618
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list


# ----  

#' Grab metadata from the NC SCO API.
#'
#' @param query_url A NC SCO API query url (without spaces).
#' @return A dataframe with query metadata.
read_ncsco_api_metadata <- function(query_url) {
  
  # grab metadata from url
  metadata_raw <- read_lines(file = query_url)
  
  # find lines with comments (list of line numbers)
  comment_lines <- grep("## ", metadata_raw)
  
  # select comments portion of query using comment_lines
  metadata_raw_sel <- metadata_raw[min(comment_lines):max(comment_lines)]
  
  # find line of comment header where csv metadata starts
  metadata_lines_start <- grep("## Location Metadata", metadata_raw_sel)
  
  # select metadata table
  metadata_repl <- gsub("## ", "", metadata_raw_sel[(metadata_lines_start + 1):length(metadata_raw_sel)]) # select metadata and replace ##'s
  
  # tidy up header
  metadata_repl_header <- metadata_repl[1] %>%
    gsub(" ", "_", .) %>%
    gsub("[", "", ., fixed = TRUE) %>%
    gsub("]", "", ., fixed = TRUE) %>%
    str_to_lower()
  
  # add tidy header and read in as csv
  metadata_tidy <- read_csv(c(metadata_repl_header, metadata_repl[2:length(metadata_repl)]), col_names = TRUE)
  
  return(metadata_tidy) 
}