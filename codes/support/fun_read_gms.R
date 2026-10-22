# file_path <- "input/mappings/SimUID_map.gms"
# header.line = 30
# data.line = 32

extract_set_columns <- function(set_string) {
  # Use regex to extract the part inside parentheses
  match <- regmatches(set_string, regexpr("\\((.*?)\\)", set_string))

  # Remove parentheses and split by comma
  columns <- unlist(strsplit(gsub("[()]", "", match), ","))

  # Trim any whitespace
  columns <- trimws(columns)

  return(columns)
}


read_gms_data <- function(file_path, header.line=1, data.lines=c(2,length(lines))) {
  # Read all lines from the file
  lines <- readLines(file_path)

  header <- lines[header.line]
  lines <- lines[c((data.lines[1]:data.lines[2]))]



  # Remove header and trailing '/' (filtering valid data lines)
  lines <- lines[!grepl("SET|/", lines)]

  # Trim leading and trailing whitespaces
  lines <- trimws(lines)

  # Remove empty lines
  lines <- lines[lines != ""]

  data_list <- strsplit(lines, "[\\.]+")


  # Convert to a data frame
  df <- do.call(rbind, data_list)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # Apply gsub to remove all spaces
  df <- data.frame(lapply(df, function(x) gsub(" ", "", x)))


  # Rename columns
  colnames(df) <- extract_set_columns(header)

  # Remove dots from variable values
  df[] <- lapply(df, function(x) gsub("^\\.", "", x))

  return(df)
}
