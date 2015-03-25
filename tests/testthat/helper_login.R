message("Loading Token")

google_token <- readRDS("token_file.rds")
assign("driver_token", google_token, envir=driver:::.state)

list_files()