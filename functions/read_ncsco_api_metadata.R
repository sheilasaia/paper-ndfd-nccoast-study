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
#' @param query_raw_text The raw text from a NC SCO API query.
#' @return A dataframe with tidied metadata based on the NC SCO API query.
read_ncsco_api_metadata <- function(query_raw_text) {
  
  # grab metadata from url
  metadata_raw <- read_lines(file = query_raw_text)
  
  # find lines with comments (list of line numbers)
  comment_lines <- grep("## ", metadata_raw)
  
  # select comments portion of query using comment_lines
  metadata_raw_sel <- metadata_raw[min(comment_lines):max(comment_lines)]
  
  # find line of comment header where csv metadata starts
  metadata_lines_start <- grep("## Location Metadata", metadata_raw_sel)
  
  # select metadata table
  metadata_repl <- gsub("## ", "", metadata_raw_sel[(metadata_lines_start + 1):length(metadata_raw_sel)]) # select metadata and replace ##'s
  
  # data without headers
  metadata_data_no_header <- metadata_repl[2:length(metadata_repl)]
  
  # dealing with exceptions in data
  # NCAT site in ECONET (have to take out comma in supporting agency field)
  # if (length(grep(pattern = "ECONET", query_url)) == 1) {
  #   metadata_data_no_header_checked <- gsub("NCA&T, NC", "NCA&T and NC", metadata_data_no_header)
  # }
  
  # no issues so save as new name
  # else {
  metadata_data_no_header_checked <- metadata_data_no_header
  # }
  
  # tidy up header
  metadata_repl_header <- metadata_repl[1] %>%
    gsub(" ", "_", .) %>%
    gsub("[", "", ., fixed = TRUE) %>%
    gsub("]", "", ., fixed = TRUE) %>%
    str_to_lower()
  
  # header as list
  # metadata_repl_header_fix <- unlist(str_split(metadata_repl_header, ","))
  
  # no headers
  # metadata_repl_data <- data.frame(data = metadata_data_no_header_checked) %>%
    # mutate(row_id = seq(1, length(metadata_data_no_header), 1),
    #        split_data = str_split(data, pattern = ",")) %>%
    # unnest(split_data) %>%
    # group_by(row_id) %>%
    # count() %>%
    # select(row_id, split_data) %>% 
    # pivot_wider(names_from = metadata_repl_header_fix$header, values_from = split_data)
    # rowwise() %>%
    # purrr:::map_df(str_split(string = .$data, pattern = ","))
    # separate(col = metadata_repl_header_fix, sep = ",", remove = FALSE)
  
  # add tidy header and read in as csv
  metadata_tidy <- read_csv(c(metadata_repl_header, metadata_data_no_header_checked), 
                            col_names = TRUE, 
                            col_types = cols(.default = col_character())) %>%
    distinct_all() # delete replicates
  # NOTE: All colunms are in the character format!
  
  return(metadata_tidy) 
}
