library(tidyverse)

writeLines("INIT", "status.txt")  # Save status
source("https://raw.githubusercontent.com/favstats/appendornot/refs/heads/master/R/save.R")

# Define time frame values
tf_values <- c("7", "30", "90")

# Read and prepare country list
eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DK", "EE", "ES", "FI", 
                  "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                  "PL", "PT", "RO", "SE", "SI", "SK", "NZ",  "MX",
                  "CA", "AU", "US", "NL") %>% sample(31)

outcome <- commandArgs(trailingOnly = TRUE)

print(outcome)

# outcome <- outcome %>% 
#   str_split(" ") %>% 
#   unlist() %>% 
#   str_trim()
# 
# print(outcome)

priority_or_not <- sample(c(0,1,1), 1)

if(priority_or_not== 1){
  full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>%
    rename(iso2c = iso2, country = cntry) %>%
    sample_n(n()) %>% 
    mutate(iso2c = fct_relevel(iso2c, c("DE",eu_countries))) %>% 
    arrange(iso2c) 
} else {
  
  full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>%
    rename(iso2c = iso2, country = cntry) %>%
    arrange(desc(iso2c)) %>% 
    arrange(iso2c)  
}



try({
  the_result <- read_csv("the_result.csv")
  
  remove_that <- the_result %>% 
    filter(!should_continue) %>% 
    count(cntry, the_tf) %>% 
    rename(the_cntry = cntry, tf = the_tf) %>% 
    mutate_all(as.character)
  
  print(the_result)
  print(remove_that)
  

  
  
})

if(!exists("the_result")){
  remove_that <- tibble(the_cntry = "XX", tf = "0")
}


# Create all combinations of TF and countries
params <- crossing(tf = tf_values, the_cntry = full_cntry_list$iso2c) %>% arrange(the_cntry) %>% 
  filter(the_cntry %in% outcome) %>% 
  anti_join(remove_that) 

skip <- F
if(nrow(params)==0) skip <- T

if(skip){
  print(paste0(outcome, " not found. Skipping"))
} else {
  # Loop over each (timeframe, country) pair
  for (i in seq_len(nrow(params))) {
    
    
    try({
      the_result <- read_csv("the_result.csv")
      print(the_result)
    })
    
    
    tf <- params$tf[i]
    the_cntry <- params$the_cntry[i]
    
    print(glue::glue("🔄 Processing: {the_cntry} - Last {tf} Days"))
    
    # if(readLines("status.txt")!="VPN_ROTATION_NEEDED"){
    
    
    consecutive_error_count <- 0
    
    try({
      
      outcome <- commandArgs(trailingOnly = TRUE)
      
      # tf <- outcome[1]
      # the_cntry <- outcome[2]
      # 
      # print(outcome)
      # 
      # if (Sys.info()[["effective_user"]] %in% c("fabio", "favstats")) {
      #   ### CHANGE ME WHEN LOCAL!
      #   tf <- "30"
      #   the_cntry <- "DE"
      #   print(paste0("TF: ", tf))
      #   print(paste0("cntry: ", the_cntry))
      #   
      # }
      
      
      source("utils.R")
      
      
      suppressMessages(suppressPackageStartupMessages({
        library(httr)
        library(httr2)
        library(tidyverse)
        library(lubridate)
        library(rvest)
        library(piggyback)
        library(openssl)
        library(jsonlite)
      }))
      
      
      
      
      
      print("################ CHECK LATEST REPORT ################")
      
      
      try({
        
        timeframes <- c("yesterday", "7", "30", "90")
        # Create all combinations of country codes and timeframes
        combinations <- expand.grid(country_code = the_cntry, timeframe = timeframes, stringsAsFactors = FALSE)
        
        # Apply the function to each combination
        ress <- pmap(combinations, ~ retrieve_reports_data(..1, ..2))
        
        # Combine results into a single data frame
        latest <- bind_rows(ress)      
        
        
        then_this <- latest %>%
          group_by(country, timeframe) %>%
          slice(1) %>%
          ungroup() %>% 
          filter(str_detect(timeframe, "last_90_days"))
        
        
        last7 <- get_report_db(the_cntry, timeframe = 90, then_this$day) %>%
          mutate(sources = "report") %>%
          mutate(party = "unknown")
        
      })
      
      if (!exists("last7")) {
        last7 <- tibble()
      }
      
      
      togetstuff <- last7 %>% 
        select(contains("page_id") , contains("amount")) %>% 
        set_names("page_id", "spend") %>% 
        mutate(spend = parse_number(spend)) %>% 
        arrange(desc(spend))
      
      
      
      for (i in seq_along(togetstuff$page_id)) {
        # Get insights for the current page ID
        jb <- get_page_insights(
          togetstuff$page_id[i], 
          timeframe = glue::glue("LAST_90_DAYS"), 
          include_info = "targeting_info"
        )
        
        # Ensure `jb` is not NULL before proceeding
        if (!is.null(jb)) {
          
          # If `jb` is empty, move to next iteration
          if (nrow(jb) == 0) {
            next
          } 
          
          # Check if API block occurred due to login issues
          if (if_not_null(jb$error, str_detect(str_to_lower(jb$error), "log in"))) {
            message("❌ API Block or No Data! Exiting early...")
            writeLines("VPN_ROTATION_NEEDED", "status.txt")  # Save status
            break  # Stop execution
          } 
          
          # Extract `new_ds` value safely
          new_ds <- jb %>% 
            arrange(ds) %>% 
            slice(1) %>% 
            pull(ds)
          
          # If `new_ds` is valid, break loop
          if (!is.null(new_ds)) {
            break
          }
        } 
      }
      
      if(readLines("status.txt")=="VPN_ROTATION_NEEDED"){
        break
      }
      
      to_get <- latest %>%
        filter(day == new_ds) %>%
        filter(str_detect(timeframe, tf))
      
      if (nrow(to_get) != 0) {
        # debugonce(retrieve_reports_data)
        last7 <- get_report_db(the_cntry, readr::parse_number(to_get$timeframe), to_get$day)
        
        togetstuff <-
          last7 %>% select(page_id , contains("amount")) %>%
          set_names("page_id", "spend") %>%
          mutate(spend = parse_number(spend)) %>%
          arrange(desc(spend))
        
        report_matched = T
      } else {
        report_matched = F
        
      }
      
      print("################ LATEST TARGETING DATA ################")
      
      try({
        
        # Combine results into a single data frame
        thosearethere <- retrieve_targeting_metadata(the_cntry, tf)
        
        try({
          
          latest_elex <- get_targeting_db2(the_cntry, tf, thosearethere$ds[1], verbose = T)
          
        })
        
        if (!exists("latest_elex")) {
          latest_elex <- tibble()
        }
        
        if (!("ds" %in% names(latest_elex))) {
          latest_elex <- latest_elex %>% mutate(ds = "")
        }
        
        latest_ds <- thosearethere$ds[1]
        
      })
      
      if(exists("the_result")){
        status <- the_result %>% 
          filter(the_ds == latest_ds) %>% 
          filter(cntry == the_cntry) %>% 
          filter(the_tf == tf)        
      } else {
        status <- tibble(should_continue = TRUE)
      }
      
      if(nrow(status)!=0){
        if(!status$should_continue){
          print("ABBRUCH: This DS already passed a couple of times.")
          break
        }
      }
      
      if (!exists("latest_ds")) {
        latest_ds <- "2023-01-01"
      } else if (is.na(latest_ds)) {
        latest_ds <- "2023-01-01"
      }
      
      
      tstamp <- Sys.time()
      
      write_lines(lubridate::as_date(tstamp), "tstamp.txt")
      
      country_codes <- c(
        "AD", "AL", "AM", "AR", "AT", "AU", "BA",
        "BE", "BG", "BR", "CA", "CH", "CL", "CO", 
        "CY", "CZ", "DE", "DK", "EC", "EE", "ES", 
        "FI", "FR", "GB", "GR", "GT", "HR", "HU",
        "IE", "IN", "IS", "IT", "LI", "LT", "LU",
        "LV", "MD", "ME", "MK", "MT", "MX", "NL", 
        "NO", "NZ", "PL", "PT", "RO", "RS", "SE",
        "SI", "SK", "SM", "TR", "UA", "US", "VE",
        "ZA"
      )
      
      print("################ WTM DATA ################")
      
      
      library(httr)
      # if (runif(1) < 1e-4 | Sys.info()["effective_user"] == "favstats") {
      if (runif(1) < 1e-4) {
        
        # if (runif(1) < 1e-4) {
        
        
        try({
          
          # thecntry <- "CA"
          url <- "https://data-api.whotargets.me/advertisers-export-csv"
          
          token <- Sys.getenv("WHO_TARGETS_TOKEN")
          
          headers <- add_headers(
            accept = "application/json",
            `accept-language` = "en-US,en;q=0.9,de-DE;q=0.8,de;q=0.7,nl;q=0.6,it;q=0.5,sv;q=0.4,is;q=0.3",
            # authorization = paste("Bearer", token),
            `x-access-token` = token ,
            `content-type` = "application/json",
            priority = "u=1, i",
            `sec-ch-ua` = '"Chromium";v="134", "Not:A-Brand";v="24", "Google Chrome";v="134"',
            `sec-ch-ua-mobile` = "?0",
            `sec-ch-ua-platform` = '"macOS"',
            `sec-fetch-dest` = "empty",
            `sec-fetch-mode` = "cors",
            `sec-fetch-site` = "same-site"
          )
          
          wtm_data <- country_codes %>% 
            map_dfr(~{
              the_cntry <- .x
              body <- list(
                alpha2 = stringr::str_to_lower(the_cntry),
                should_be_emailed = FALSE
              )
              
              response <- POST(url, headers, body = body, encode = "json")
              
              # library(tidyverse)
              
              # vroom::vroom(url(content(response, "parsed")$url))
              
              download.file(content(response, "parsed")$url, destfile = "wtmdata.csv")
              
              print(the_cntry)
              
              if(nrow(readr::read_csv("wtmdata.csv")!=0 )){
                wtm_data <- readr::read_csv("wtmdata.csv") %>% #names
                  select(page_id = advertisers_platforms.advertiser_platform_ref,
                         page_name = name,
                         party = entities.short_name)  %>%
                  mutate(page_id = as.character(page_id)) %>%
                  mutate(sources = "wtm") %>% 
                  mutate(cntry = the_cntry)
                
                return(wtm_data)       
              }
              
              
              
            })
          
          write_csv(wtm_data, "data/wtm_advertisers.csv")
          
          
          
        })
        
        
      }
      
      
      
      # } else {
      wtm_data <-  readr::read_csv("data/wtm_advertisers.csv")
      wtm_data <- wtm_data %>% 
        # mutate(cntry = stringr::str_to_upper(countries.alpha2)) %>% 
        filter(cntry == the_cntry)
      
      polsample <- readRDS("data/polsample.rds")
      
      tep_dat <- polsample %>%
        filter(cntry %in% the_cntry) %>%
        mutate(sources = "tep") %>%
        rename(party = name_short)
      
      
      all_dat <- bind_rows(wtm_data) %>%
        bind_rows(tep_dat) %>%
        bind_rows(last7) %>%
        distinct(page_id, .keep_all = T) %>%
        add_count(page_name, sort  = T) %>%
        mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
        filter(!remove_em) %>%
        select(-n,-contains("no_data"))  %>% 
        mutate(total_n = n()) %>% 
        filter(page_id != 0) 
      
      
      the_amount <- all_dat %>% names() %>% keep(~str_detect(.x, "amount_spent")) %>% .[1]
      
      
      
      all_dat <- all_dat %>% 
        mutate(amount_spent = parse_number(as.character(all_dat[[the_amount]]))) %>% 
        arrange(desc(amount_spent)) %>%
        distinct(page_id, .keep_all = T)
      
      
      saveRDS(all_dat, "data/all_dat.rds")
      
      scrape_dat <- all_dat
      
      fin <<- tibble(no_data = T)
      
      scraper <- function(internal, time = tf) {
        # try({
        
        # debugonce(get_page_insights)  
        # get_page_insights("106359662726593")
        
        
        # if(is.null(fin$error)){
        
        fin <<-
          # get_targeting(internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
          get_page_insights(internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS"), include_info = "targeting_info", iso2c = the_cntry)
        
        # print(fin)
        if (nrow(fin) != 0) {
          if(is.null(fin$error)){
            # print("mmm")
            fin <- fin %>% 
              mutate(tstamp = tstamp)
            if (!dir.exists(glue::glue("targeting/{time}"))) {
              dir.create(glue::glue("targeting/{time}"), recursive = T)
            }
            
            path <-  paste0(glue::glue("targeting/{time}/"), internal$page_id, ".rds")
            
            saveRDS(fin, file = path)           
          } else {
            # print("ccc")
            
            fin <- tibble(internal_id = internal$page_id, no_data = T, error = fin$error) %>%
              mutate(tstamp = tstamp)
          }
          
          # }
        } else {
          # print("kkk")
          
          fin <- tibble(internal_id = internal$page_id, no_data = T) %>%
            mutate(tstamp = tstamp)
        }
        
        
        
        # if (Sys.info()[["effective_user"]] %in% c("fabio", "favstats")) {
        ### CHANGE ME WHEN LOCAL!
        # print(paste0(internal$page_name, ": ", nrow(fin)))
        
        # }# 
        # })
        return(fin)
        
        # })
        # }
        
      }
      
      str_detect_safe <- function(string, pattern) {
        if (is.null(string) || is.null(pattern)) {
          return(FALSE)
        }
        out <- str_detect(string, pattern)
        return(out)
      }
      
      
      
      scraper_for_loop <- function(data, time = tf, ds = new_ds, cntry = the_cntry) {
        tried_pages_file <- "tried_pages.csv"
        
        # Read previously tried combinations
        if (file.exists(tried_pages_file)) {
          tried_pages <- read_csv(tried_pages_file, col_types = cols(.default = "c"))
        } else {
          tried_pages <- tibble(ds = character(), cntry = character(), tf = character(), page_id = character())
        }
        
        # Identify already tried pages for this specific ds, cntry, and timeframe
        tried_subset <- tried_pages %>% filter(ds == !!ds, cntry == !!cntry, tf == !!time)
        
        results <- list()  # Store results
        new_tried_rows <- tibble()  # To store newly tried combinations
        
        for (i in seq_len(nrow(data))) {
          internal <- data[i, , drop = FALSE]  # Extract row as a tibble
          
          # Skip if this (ds, cntry, tf, page_id) has already been processed
          if (internal$page_id %in% tried_subset$page_id) {
            print(glue::glue("⏩ Skipping already processed: {ds}, {cntry}, {time}, {internal$page_id}"))
            next
          }
          
          the_error <- "no error"
          result <- tryCatch({
            # Call scraper function
            scraper(internal, time = time)
          }, error = function(e) {
            message("❌ Error occurred: ", e$message)
            the_error <<- e$message
            return(NULL)  # Return NULL on failure
          })
          
          if (
            if_not_null(the_error, str_detect(str_to_lower(the_error), "log in")) | 
            if_not_null(result$error, str_detect(str_to_lower(result$error), "log in"))
          ) {
            message("❌ API Block! Exiting early...")
            writeLines("VPN_ROTATION_NEEDED", "status.txt")  # Save status
            break  # Stop execution
          }
          
          # Store successful page_id
          new_tried_rows <- bind_rows(new_tried_rows, tibble(ds = ds, cntry = cntry, tf = time, page_id = internal$page_id))
          
          results[[length(results) + 1]] <- result  # Store successful result
        }
        
        # Save new records to the tracking file
        if (nrow(new_tried_rows) > 0) {
          if (file.exists(tried_pages_file)) {
            write_csv(new_tried_rows, tried_pages_file, append = TRUE)
          } else {
            write_csv(new_tried_rows, tried_pages_file)
          }
        }
        
        # Return combined results
        if (length(results) > 0) {
          final_result <- bind_rows(results) %>% mutate_all(as.character)
          return(final_result)
        } else {
          return(tibble())  # Return empty tibble if no data
        }
      }
      
      # enddat5 <- scraper_for_loop( tibble(page_id = c("47217143218", "47217143218", "XXX")), time = "7")
      
      # scraper <- possibly(scraper, otherwise = NULL, quiet = F)
      
      
      print("################ RETRIEVE AUDIENCES ################")
      
      try({
        
        current_date <-
          paste0("historic/",
                 as.character(new_ds),
                 "/",
                 "last_",
                 tf,
                 "_days")
        
        if (new_ds == latest_ds) {
          print(glue::glue("New DS: {new_ds}: Old DS: {latest_ds}"))
          
          scrape_dat <- all_dat %>%
            # arrange(page_id) %>%
            # slice(1:150) %>%
            filter(!(page_id %in% latest_elex$page_id))  %>%
            filter(page_id %in% last7$page_id) %>% 
            mutate(total_n = n())
          
          the_rows_to_be_checked <- nrow(scrape_dat)
          
          print(paste0("Number of remaining pages to check: ", nrow(scrape_dat)))
          
          ### save seperately
          # enddat <-  scrape_dat %>%
          #   split(1:nrow(.)) %>%
          #   map_dfr_progress(scraper)
          
          enddat <- scraper_for_loop(scrape_dat, time = tf)
          
          if (nrow(enddat) == 0) {
            
            print("same length! will just save the same parquet!")
            
            election_dat <- latest_elex
            
            dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
            
            
            arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
            
          } else {
            
            print("new data to be uploaded")
            
            if(is.null(enddat$page_id)){
              enddat$page_id <- enddat$internal_id
            }
            
            new_elex <- enddat
            
            print(glue::glue("Old Number of Page IDs: {length(unique(new_elex$page_id))}"))
            
            try({
              
              latest_elex <- get_targeting_db2(the_cntry, tf, thosearethere$ds[1], verbose = T)
              
            })
            
            library(dplyr)
            
            distinct_if <- function(data, ..., .keep_all = TRUE) {
              # Capture column names as symbols
              vars <- rlang::ensyms(...)
              
              # Filter for variables that exist in the dataset
              present_vars <- vars[sapply(vars, function(x) rlang::as_string(x) %in% names(data))]
              
              # If no variables are present, return the data unchanged
              if (length(present_vars) == 0) {
                warning("None of the specified variables are present in the data. Returning the original data.")
                return(data)
              }
              
              # Apply distinct on the present variables
              data %>%
                distinct(across(all_of(sapply(present_vars, rlang::as_string))), .keep_all = .keep_all)
            }
            
            # ones <- jb %>% 
            #   mutate(id = 1:n()) %>% 
            #   as_tibble() 
            # 
            # twos <- jb %>% 
            #   mutate(id = 1:n()) %>% 
            #   as_tibble() %>% 
            #   distinct_if(page_id, total_num_ads, total_spend_formatted, is_exclusion,
            #               value, type, detailed_type, custom_audience_type, location_type, .keep_all = T) 
            # 
            # ones %>% anti_join(twos %>% select(id)) %>% View()
            # ones %>% View()
            election_dat  <- enddat %>%
              mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) %>%
              # rename(page_id = internal_id) %>%
              left_join(all_dat) %>% mutate_all(as.character) %>% 
              bind_rows(latest_elex %>% mutate_all(as.character) %>% filter(!(page_id %in% enddat$page_id))) %>% 
              distinct_if(page_id, total_num_ads, total_spend_formatted, is_exclusion,
                          value, type, detailed_type, custom_audience_type, location_type, .keep_all = T) 
            
            dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
            
            print(glue::glue("New Number of Rows: {length(unique(election_dat$page_id))}"))
            
            
            arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
            
            
          }
          
          
        } else {
          
          print(glue::glue("Complete new Data. New DS: {new_ds}: Old DS: {latest_ds} 2"))
          
          print(paste0("Number of pages to check: ", nrow(scrape_dat)))
          
          the_rows_to_be_checked <- nrow(scrape_dat)
          
          # debugonce(scraper)
          ### save seperately
          # election_dat <- all_dat %>%
          #   # arrange(page_id) %>%
          #   # slice(1:2) %>%
          #   split(1:nrow(.)) %>%
          #   map_dfr_progress(scraper)  %>%
          #   mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) 
          # 
          election_dat <- scraper_for_loop(all_dat, time = tf)
          
          if(nrow(election_dat)!=0){
            election_dat <- election_dat %>%
              mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) 
          }
          
          if(is.null(election_dat$page_id)){
            election_dat$page_id <- election_dat$internal_id
          }
          
          
          election_dat <- election_dat %>% 
            left_join(all_dat)
          
          
          print(glue::glue("Number of Rows: {nrow(election_dat)}"))
          
          dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
          
          
          arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
          
          
        }
      })
      
      
      the_tag <- paste0(the_cntry, "-", "last_", tf, "_days")
      the_date <- new_ds
      
      
      releases <- readRDS("data/releases.rds")
      
      cntry_name <- full_cntry_list %>%
        filter(iso2c == the_cntry) %>%
        pull(country)
      
      if(!(the_tag %in% releases$tag_name)){
        try({
          pb_release_create_fr(
            repo = "favstats/meta_ad_targeting",
            tag = the_tag,
            body = paste0(
              "This release includes ",
              cntry_name , " '", "last_", tf, "_days" , "' Meta ad target audiences."
            ),
            releases = releases
          )    # Sys.sleep(5)
        })
      }
      
      
      file.copy(paste0(current_date, ".parquet"),
                paste0(the_date, ".parquet"),
                overwrite = T)
      
      print(file.exists(paste0(the_date, ".parquet")))
      
      if("no_data" %in% names(election_dat)){
        election_dat <- election_dat %>% filter(is.na(no_data))
      }
      
      if("no_data" %in% names(latest_elex)){
        latest_elex <- latest_elex %>% filter(is.na(no_data))
      }
      
      if("error" %in% names(latest_elex)){
        latest_elex <- latest_elex %>% select(-error)
      }      
      
      if("error" %in% names(election_dat)){
        election_dat <- election_dat %>% select(-error)
      }      
      
      
      if(!(identical(latest_elex, election_dat))){
        
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
      
      # file.remove(paste0(the_date, ".parquet"))
      
      
      gc()
      
      # }
      # # .[1:7] %>%
      # walk_progress( ~ {
      #
      #
      # })
      
      # unzip("report/TN/2023-11-28.zip", exdir = "extracted", overwrite = T)
      
      # unzip(dir(paste0("report/",cntry_str), full.names = T, recursive = T), exdir = "extracted")
      
      
      
      unlink("targeting", recursive = T, force = T)
      unlink("historic", recursive = T, force = T)
      
      print("################ FIN ################")
      
      # }
      
      # unlink("node_modules", recursive = T, force = T)
      # unlink("out", recursive = T, force = T)
      
      
    })
    
    if(!exists("new_elex")){
      new_elex <- tibble()
    } else {
      if("no_data" %in% names(new_elex)){
        new_elex <- new_elex %>% filter(is.na(no_data))
      }
    }
    
    if(!exists("the_rows_to_be_checked")){
      the_rows_to_be_checked <- tibble()
    } 
    
    if(!exists("the_status_code")){
      the_status_code <- "no status code"
    } 
    
    
    
    
    # Telegram bot setup
    TELEGRAM_BOT_ID <- Sys.getenv("TELEGRAM_BOT_ID")
    TELEGRAM_GROUP_ID <- Sys.getenv("TELEGRAM_GROUP_ID")
    
    # Function to log final statistics with Telegram integration
    log_final_statistics <- function(stage, tf, cntry, new_ds, latest_ds,
                                     the_rows_to_be_checked, election_dat, new_elex,
                                     pushed_successfully, togetstuff, report_matched) {
      # Check if ds was already present
      ds_present <- ifelse(new_ds == latest_ds, "Yes", "No")
      
      # Calculate statistics
      total_rows <- length(unique(election_dat$page_id))
      new_rows <- length(unique(new_elex$page_id))
      lag_days <- as.numeric(Sys.Date() - lubridate::ymd(new_ds))
      
      election_dat$amount_spent <- as.numeric(election_dat$amount_spent)
      
      # Spending coverage statistics
      page_ids_in_togetstuff <- sum(togetstuff$page_id %in% election_dat$page_id)
      total_spend_in_togetstuff <- sum(togetstuff$spend, na.rm = TRUE)
      election_dat <- distinct(election_dat, page_id, .keep_all = T)
      covered_spend <- sum(election_dat$amount_spent[election_dat$page_id %in% togetstuff$page_id], na.rm = TRUE)
      
      spend_coverage_pct <- round((covered_spend / total_spend_in_togetstuff) * 100)
      coverage_status <- ifelse(spend_coverage_pct == 100, "✅", "❌")
      
      # Check GitHub push status
      push_status <- ifelse(pushed_successfully, "✅ Yes", "❌ No")
      report_status <- ifelse(report_matched, "✅ Yes", "❌ No")
      
      
      # 🛑 Decision: Should the workflow continue?
      if (page_ids_in_togetstuff == nrow(togetstuff) | new_rows == 0) {
        should_continue <- F
        reason <- if (page_ids_in_togetstuff == nrow(togetstuff)) {
          "🔴 All required Page IDs were already retrieved, nothing new to check."
        } else {
          "🔴 No new Page IDs were added in this run, no updates needed."
        }
      } else {
        should_continue <- T
        reason <- if (new_rows > 0) {
          "🟢 New Page IDs were added, requiring additional runs."
        } else {
          "🟢 Some Page IDs are still missing, continuing the process."
        }
      }
      
      # ✅ Log the decision explicitly
      print(glue::glue(
        "📌 *Decision to Continue: {should_continue}*\n",
        "   └ 🎯 *Reason:* {reason}\n",
        "   └ 🔍 Page IDs Checked in Report: {page_ids_in_togetstuff} / {nrow(togetstuff)}\n",
        "   └ ➕ New Page IDs Found: {new_rows}\n"
      ))
      
      
      if (should_continue) {
        writeLines("changes_detected", glue::glue("status_{tf}.txt"))
        print(glue::glue("Status for timeframe {tf}: changes_detected"))
      } else {
        writeLines("no_changes", glue::glue("status_{tf}.txt"))
        print(glue::glue("Status for timeframe {tf}: no_changes"))
      }
      
      the_result <- tibble(cntry = the_cntry, the_ds = latest_ds, the_tf = tf, should_continue)
      
      save_csv(the_result, path = "the_result.csv")      
      
      should_continue <- ifelse(should_continue, "✅ Yes", "❌ No")
      
      # Construct details message
      details <- glue::glue(
        "   \t\t📌 *Newest DS:* {new_ds}\n",
        "   \t\t📌 *Latest DS:* {latest_ds}\n",
        "   \t\t📌 *DS Already Present:* {ds_present}\n",
        "   \t\t🔋 *Page IDs Checked:* {the_rows_to_be_checked}\n",
        "   \t\t📊 *Total Page IDs:* {total_rows}\n",
        "   \t\t➕ *New Page IDs Added:* {new_rows}\n",
        "   \t\t🕒 *Days Lagging:* {lag_days} days\n",
        "   \t\t🚀 *GitHub Push Successful:* {push_status}\n",
        "   \t\t😎 *Report Matched:* {report_status}\n",
        "   \t\t🔍 *Page IDs Present (of Report):* {page_ids_in_togetstuff}/{nrow(togetstuff)}\n",
        "   \t\t💰 *Spending Coverage:* {covered_spend}/{total_spend_in_togetstuff} ({spend_coverage_pct}% {coverage_status})\n",
        "   \t\t📌 *Continue Today:* {should_continue}\n",
        "   \t\t💰 *Source:* local"
      )
      

      
      # Construct the full message
      the_message <- glue::glue(
        "🔹 *{stage}* 🔹\n",
        "🌍 *Country:* {cntry}\n",
        "⏳ *Timeframe:* {tf}\n",
        "🕒 *Time:* {Sys.time()}\n",
        "{details}"
      )
      
      print(the_message)
      
      # Send the message to Telegram
      url <- paste0("https://api.telegram.org/bot", Sys.getenv("TELEGRAM_BOT_ID"), "/sendMessage")
      out <<- httr::POST(url, body = list(chat_id = Sys.getenv("TELEGRAM_GROUP_ID"), text = the_message, parse_mode = "Markdown"), encode = "form")
      if (httr::http_error(out)) {
        print(httr::content(out))
        print(httr::headers(out))
      }
      
      # # Data for the tibble
      # the_data <- tibble::tibble(
      #   Field = c(
      #     "stage",
      #     "country",
      #     "timeframe",
      #     "time",
      #     "newest_ds",
      #     "latest_ds",
      #     "ds_already_present",
      #     "page_ids_checked",
      #     "total_page_ids",
      #     "new_page_ids_added",
      #     "days_lagging",
      #     "github_push_successful",
      #     "report_matched",
      #     "page_ids_present_audience",
      #     "page_ids_present_report",
      #     "spending_coverage"
      #   ),
      #   Value = c(
      #     stage,
      #     cntry,
      #     tf,
      #     as.character(Sys.time()),
      #     new_ds,
      #     latest_ds,
      #     ds_present,
      #     the_rows_to_be_checked,
      #     total_rows,
      #     new_rows,
      #     lag_days,
      #     push_status,
      #     report_status,
      #     page_ids_in_togetstuff,
      #     nrow(togetstuff),
      #     paste(covered_spend, "/", total_spend_in_togetstuff, "(", spend_coverage_pct, "%", coverage_status, ")")
      #   )
      # )
      
      
    }
    
    update_workflow_schedule <- function(should_continue = TRUE, thetf = tf, verbose = TRUE) {
      all_timeframes <- c(7, 30, 90)  # Define all timeframes
      other_timeframes <- setdiff(all_timeframes, as.numeric(thetf))  # Timeframes to modify
      
      if (should_continue) {
        # Remove `on: push` from other timeframes
        for (tf in other_timeframes) {
          workflow_file <- glue::glue(".github/workflows/r{tf}.yml")
          if (verbose) print(glue::glue("Processing workflow file: {workflow_file}"))
          
          if (!file.exists(workflow_file)) {
            if (verbose) print(glue::glue("Workflow file does not exist: {workflow_file}. Skipping."))
            next
          }
          
          workflow_content <- readLines(workflow_file)
          
          # Remove `push` block if it exists
          push_start_idx <- which(str_detect(workflow_content, "^  push:"))
          if (length(push_start_idx) > 0) {
            branches_end_idx <- push_start_idx + 3  # Assuming the block has 3 lines
            workflow_content <- workflow_content[-(push_start_idx:branches_end_idx)]
            if (verbose) print(glue::glue("'push' block removed from {workflow_file}."))
          } else {
            if (verbose) print(glue::glue("No 'push' block found in {workflow_file}. Skipping."))
          }
          
          writeLines(workflow_content, workflow_file)
        }
        
        # Ensure `on: push` exists for the current timeframe
        workflow_file <- glue::glue(".github/workflows/r{thetf}.yml")
        if (verbose) print(glue::glue("Processing current workflow file: {workflow_file}"))
        
        if (!file.exists(workflow_file)) {
          if (verbose) print(glue::glue("Workflow file does not exist: {workflow_file}. Exiting."))
          return(FALSE)
        }
        
        workflow_content <- readLines(workflow_file)
        
        # Check if `push` block exists
        push_start_idx <- which(str_detect(workflow_content, "^  push:"))
        if (length(push_start_idx) == 0) {
          # Add `push` block after `on:`
          on_idx <- which(str_detect(workflow_content, "^on:"))
          if (length(on_idx) == 1) {
            if (verbose) print(glue::glue("Adding 'push' block to {workflow_file}."))
            push_block <- c(
              "  push:",
              "    branches:",
              "      - master",
              "      - main"
            )
            workflow_content <- append(workflow_content, push_block, after = on_idx)
            writeLines(workflow_content, workflow_file)
            if (verbose) print(glue::glue("'push' block added successfully to {workflow_file}."))
          } else {
            if (verbose) print(glue::glue("Could not find 'on:' block in {workflow_file}. Skipping."))
          }
        } else {
          if (verbose) print(glue::glue("'push' block already exists in {workflow_file}. No changes made."))
        }
      } else {
        # For `should_continue = FALSE`, remove `on: push` and update cron schedule for the current timeframe
        workflow_file <- glue::glue(".github/workflows/r{thetf}.yml")
        if (verbose) print(glue::glue("Processing current workflow file: {workflow_file}"))
        
        if (!file.exists(workflow_file)) {
          if (verbose) print(glue::glue("Workflow file does not exist: {workflow_file}. Exiting."))
          return(FALSE)
        }
        
        workflow_content <- readLines(workflow_file)
        
        # Remove `push` block if it exists
        push_start_idx <- which(str_detect(workflow_content, "^  push:"))
        if (length(push_start_idx) > 0) {
          branches_end_idx <- push_start_idx + 3  # Assuming the block has 3 lines
          workflow_content <- workflow_content[-(push_start_idx:branches_end_idx)]
          if (verbose) print(glue::glue("'push' block removed from {workflow_file}."))
        }
        
        # Update cron schedule
        cron_line_idx <- which(str_detect(workflow_content, "cron:"))
        settimer <- sample(1:12, 1)
        new_cron <- glue::glue("    - cron: '0 {settimer} * * *'")
        
        if (length(cron_line_idx) > 0) {
          workflow_content[cron_line_idx] <- new_cron
          if (verbose) print(glue::glue("Updated cron schedule in {workflow_file} to: {new_cron}"))
        } else {
          if (verbose) print("No cron line found. This may cause issues.")
        }
        
        writeLines(workflow_content, workflow_file)
        if (verbose) print(glue::glue("Workflow file updated: {workflow_file}"))
      }
      
      if (verbose) print("Workflow update process complete.")
      return(should_continue)
    }
    
    # tf <- 30
    # update_workflow_schedule(T)
    
    
    try({
      # Example integration (call this after processing):
      log_final_statistics(
        stage = "Process Complete",
        tf = tf,
        cntry = the_cntry,
        new_ds = new_ds,
        latest_ds = latest_ds,
        the_rows_to_be_checked = the_rows_to_be_checked,
        election_dat = election_dat,
        new_elex = new_elex,
        pushed_successfully = the_status_code,
        togetstuff = togetstuff,
        report_matched = report_matched
      )
    })
    
    
    print(glue::glue("🔄 Processed: {the_cntry} - Last {tf} Days"))
    
    
    print("################ VERY END ################")
    
    
    # } else {
    #   break
    # }
    
  }
  
}
