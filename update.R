
source("utils.R")

the_date <- "2025-01-27"
tf <- "7"
the_cntry <- "US"

releases <- readRDS("data/releases.rds")
the_tag <- paste0(the_cntry, "-", "last_", tf, "_days")


download.file(url = "https://github.com/favstats/wtm_us/raw/refs/heads/main/historic/2025-01-27/7.rds", destfile = "temp.rds", mode = "wb")



the_data <- readRDS("temp.rds") %>% filter(is.na(no_data))

dbdata <- metatargetr::get_targeting_db("US", 7, "2025-01-27")

# the_data %>% count(page_id, sort = T)
# dbdata %>% count(page_id, sort = T)

merged_data <- the_data %>% 
  mutate_all(as.character) %>% 
  filter(!(page_id %in% dbdata$page_id)) %>% 
  bind_rows(dbdata, .) %>% 
  mutate_all(as.character) 

arrow::write_parquet(merged_data, paste0(the_date, ".parquet"))

if(!(identical(merged_data, dbdata))){
  
  print("################ UPLOAD FILE ################")
  
  
  try({
    # print(paste0(the_date, ".rds"))
    # print(the_tag)
    # debugonce(pb_upload_file_fr)
    rsd <- pb_upload_file_fr(
      paste0(the_date, ".parquet"),
      repo = "favstats/meta_ad_targeting",
      tag = the_tag,
      releases = releases
    )
    # pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
    try({
      the_status_code <- httr::status_code(rsd)
    })
  })
  
  print(paste0("################ UPLOADED FILE ################: ", the_cntry))
  
  
} else {
  print("File is identical, will not be uploaded")
}


source("utils.R")

process_targeting_data("2025-01-22", "30", "DE", releases, "favstats/meta_ad_targeting")

# Generate all January 2025 dates
dates <- seq.Date(from = as.Date("2025-01-01"), to = as.Date("2025-01-31"), by = "day")
timeframes <- c("7", "30", "90")
countries <- c("DE", "US", "FR")  # Add more if needed

# Cross all combinations and run function
args_list <- expand.grid(the_date = dates, tf = timeframes, the_cntry = countries, 
                         stringsAsFactors = FALSE)
process_targeting_data2 <- possibly(process_targeting_data, quiet = F, otherwise = NULL)
# Run in parallel using purrr::pwalk
pwalk(args_list, ~ process_targeting_data2(..1, ..2, ..3, releases, "favstats/meta_ad_targeting"))

process_targeting_data <- function(the_date, tf, the_cntry, releases, repo) {
  
  the_tag <- paste0(the_cntry, "-", "last_", tf, "_days")
  url <- paste0("https://github.com/favstats/wtm_", str_to_lower(the_cntry) ,"/raw/refs/heads/main/historic/", the_date, "/", tf, ".rds")
  temp_file <- "temp.rds"
  
  print(paste0("📥 Downloading data for ", the_cntry, " from: ", url))
  download.file(url = url, destfile = temp_file, mode = "wb")
  
  print("📂 Reading downloaded data...")
  the_data <- readRDS(temp_file) %>% filter(is.na(no_data))
  
  print("📂 Fetching database records...")
  dbdata <- metatargetr::get_targeting_db(the_cntry, as.numeric(tf), the_date) %>% 
    filter(cntry != the_cntry)
  
  print("📊 Comparing datasets...")
  
  # Initial row counts
  cat("\n🔹 Initial Row Counts:\n")
  cat("   - Downloaded data: ", nrow(the_data), "rows\n")
  cat("   - Database data: ", nrow(dbdata), "rows\n")
  
  # Unique page_id counts
  cat("\n🔹 Unique page_id counts:\n")
  cat("   - Downloaded data: ", length(unique(the_data$page_id)), "unique page_ids\n")
  cat("   - Database data: ", length(unique(dbdata$page_id)), "unique page_ids\n")
  
  # Finding missing page IDs
  missing_page_ids <- setdiff(the_data$page_id, dbdata$page_id)
  
  cat("\n🔹 New page_ids not in database: ", length(missing_page_ids), "\n")
  
  if (length(missing_page_ids) > 0) {
    cat("   Sample new page_ids:\n")
    print(head(missing_page_ids, 10))
  }
  
  # Merging datasets
  print("🔄 Merging data...")
  merged_data <- the_data %>%
    mutate_all(as.character) %>%
    filter(!(page_id %in% dbdata$page_id)) %>%
    bind_rows(dbdata %>%
                mutate_all(as.character), .)
  
  # Post-merge row counts
  cat("\n📊 Final dataset after merging:\n")
  cat("   - Total rows: ", nrow(merged_data), "\n")
  cat("   - Unique page_ids: ", length(unique(merged_data$page_id)), "\n")
  
  # Save merged data
  output_file <- paste0(the_date, ".parquet")
  print(paste0("💾 Saving merged data to: ", output_file))
  arrow::write_parquet(merged_data, output_file)
  
  # Upload only if data has changed
  if (!identical(merged_data, dbdata)) {
    print("\n🚀 Detected changes! Uploading new dataset...")
    
    try({
      rsd <- pb_upload_file_fr(output_file, repo = repo, tag = the_tag, releases = releases)
      
      try({
        the_status_code <- httr::status_code(rsd)
      })
    })
    
    unlink(paste0(the_date, ".parquet"))
    
    print(paste0("✅ File uploaded successfully for ", the_cntry))
  } else {
    print("\n✅ No changes detected, skipping upload.")
  }
}
