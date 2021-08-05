# ---- script header ----
# script name: read_clouds_metadata.R
# purpose of script: defines a function that reads metadata (only) from the CLOUDS API
# author: sheila saia
# date created: 20200618
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list:
# TODO allow for multiple networks (i.e., a list) as an input to clouds_network
# TODO allow for different step sizes (i.e., month, day, hour) --> right now is just daily
# TODO allow for other parameter functionality (e.g., specific location, etc.)
# TODO add package dependencies


# ---- function ----
#' Grab metadata from the CLOUDS API.
#'
#' @param query_raw_text The raw text from a CLOUDS API query.
#' @return A dataframe with tidied metadata based on the CLOUDS API query.
read_clouds_metadata <- function(query_raw_text) {
  
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
  metadata_repl_header_str <- metadata_repl[1] %>%
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
  
  # final raw metadata list vector (with header)
  metadata_raw_str <- c(metadata_repl_header_str, metadata_data_no_header_checked)
  
  # add carriage return to the end of each line using purrr::map()
  metadata_raw_str_fix <- metadata_raw_str %>%
    map_chr(~ paste(., "\n", sep = ""))
  
  # add tidy header and read in as csv
  metadata_tidy <- read_csv(metadata_raw_str_fix,
                            col_names = TRUE,
                            col_types = cols(.default = col_character()))
  # NOTE: All columns are in the character format!
  
  return(metadata_tidy) 
}