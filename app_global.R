
x0 <- read_civis('staging_pmihq.smc_eligibility_data') %>%
  mutate(date = as.Date(as.character(date))) %>%
  mutate(area = paste0(country, "/",admin_level_1, "/\n", admin_level_2)) 

x0 <- x0 %>%
  mutate(across(starts_with("smc"), ~ gsub("Eligible\\+", "Eligible + 10%", .)))

# list of countries that are smc-eligible at different cutoffs
# csmc <- x0 %>% select(geo_id, starts_with("smc")) %>% distinct() 
  
xtxt <- read_civis('staging_pmihq.smc_eligibility_data_txt')

# Shapefiles -------------------------------------------------------------------

get_shapefile <- function(admin_level, file_id) {
  # stop if no input aruments are provided
  if (missing(admin_level) & missing(file_id) )
    stop("Please provide either an admin aggregation level (admin_level) or a file ID (file_id).")
  if (!missing(admin_level) & !missing(file_id) )
    stop("Please provide only one input arguement (admin_level) or (file_id).")
  
  # get file id or file name based on user input
  if (!missing(admin_level) & missing(file_id)) {
    file_name <- paste0("combo_admin",admin_level,"_simple_aligned")
    # obtain a list of the files in civis PMI shapefile project using the civis function
    objects <- projects_get(110044)
    # flatten nested list into data frame for searching
    obj_files_list <- objects$files
    obj_files_df <- as.data.frame(matrix(unlist(obj_files_list), ncol=5,byrow = TRUE),stringsAsFactors = FALSE)[,c(1,4)]
    colnames(obj_files_df) <- c("file_id","file_name")
    # get file id
    idx <- which(obj_files_df$file_name ==file_name)
    file_id <- as.numeric(obj_files_df$file_id[idx])
  } else if (missing(admin_level) & !missing(file_id)) {
    file_name<-files_get(file_id)$name 
  } 
  # create temp files and directories
  temp_dir <- tempdir()
  temp_file <- tempfile()
  temp_dir_sf_name <- paste0(temp_dir,"/",file_name,"/")  
  
  # extract country name from report title
  download_civis(file_id, file = temp_file, overwrite = TRUE) # download file
  unzip(temp_file, exdir = temp_dir)
  return(sf::st_read(dsn = temp_dir_sf_name, layer = file_name)) # load into R
  
}



# Extract shapefiles and change the admin level column name to match QR data
s0 <- get_shapefile(admin_level = 0)
s1 <- get_shapefile(admin_level = 1)
s2 <- get_shapefile(admin_level = 2)


s0 <- s0 %>%
  filter(admin0_raw != 'Laos',
         admin0_raw != 'Viet Nam')
s1 <- s1 %>%
  filter(admin0_raw != 'Laos',
         admin0_raw != 'Viet Nam')

s2 <- s2 %>%
  filter(admin0_raw != 'Laos',
         admin0_raw != 'Viet Nam')

s0_ctr <- st_point_on_surface(s0$geometry) %>% st_geometry() %>% st_coordinates()



# append country-level SF centroids
s0$lat_ctr <- s0_ctr[,"Y"]
s0$lon_ctr <- s0_ctr[,"X"]

# s2_map <- merge(s2, csmc , by = "geo_id", all.x = TRUE, all.y = TRUE) %>%
#   mutate(across((starts_with("smc")), ~ case_when(is.na(.) ~ "Insufficient Data",
#                                                TRUE ~ .))) %>%
#   mutate(across(c(starts_with("smc")), 
#                 ~ factor(., levels = c("Eligible", "Eligible + 10%", "Not Eligible",
#                                        "Annual cases <= 250", "Insufficient Data")))) %>%
#   filter(!is.na(geo_id)) %>%
#   mutate(popup_txt = paste0("Country: <b>", country, "</b><br>Admin 1: <b>", admin1,
#                             "</b><br>Admin2: <b>", admin2, "</b>"))


# s2_gid <- st_drop_geometry(s2_map) %>% pull(geo_id)

cc_list <- sort(unique(s0$country)) 
cc_list <- cc_list[!cc_list %in% c("Cambodia", "Ethiopia", "Myanmar", "Tanzania (Mainland)", "Tanzania (Zanzibar)", "Thailand")]

cc_indi <- c("Confirmed cases" = "confirmed_cases", 
             "Suspected cases" = "suspected_cases",
             "Rainfall" = "rf")

# gglist <- NULL
# for(i in 1:length(s2_gid)) {
#   dfi <- x0 %>% filter(geo_id == s2_gid[i])
#   
#   if( (nrow(dfi) == 0) | all(is.na(dfi$confirmed_cases)) ) {
#     gi <- ggplot() + theme_void() + 
#       geom_text(aes(0, 0, label = "Data unavailable"))
#   } else {
#     title_txt <- paste(unique(dfi$country), unique(dfi$admin_level_1), unique(dfi$admin_level_2), sep = "/")
#     gi <- ggplot(dfi, aes(x = date, y = confirmed_cases)) + geom_line() + geom_point() +
#       scale_x_date(date_labels = "%b %Y") +
#       ggtitle(title_txt) + ylab("Confirmed Cases") + xlab("")
#     theme_minimal()
#   }
#   
#   gglist[[i]] <- gi
#   
# }


clr_smc <- colorFactor(c("#228833", "#66CCEE", "#EE6677", "#AA3377", "#BBBBBB"), 
                       levels = c("Eligible", "Eligible + 10%", "Not Eligible", "Annual cases <= 250", "Insufficient Data"))



# leaflet(s2_map) %>% addTiles() %>% 
#   addPolygons(color = "black", weight = 0.4, fillColor = ~ clr_smc(smc_60),
#               popup = popupGraph(gglist, height = 300, width = 400),
#               popupOptions = popupOptions(maxWidth = 700)) %>%
#   # addPolygons(data = s0, color = "black", weight = 0.6, fillColor = NA) %>%
#   addLegend("bottomright", pal = clr_smc, values = ~ smc_60)
 


get_data_main <- function(ctry, var, cutoff) {
  
  var_cutoff <- gsub("%", "", paste0("smc_", cutoff))
  
  xin <- x0 %>% filter( (country %in% ctry) & (variable %in% var) ) %>%
    rename(smc_in = var_cutoff)
  
  xtmp <- xin %>% select(geo_id, smc_in) %>% distinct()
  
  s2in <- s2 %>% filter(country %in% ctry) %>%
    merge(., xtmp, by = "geo_id") %>%
    mutate(across((starts_with("smc")), ~ case_when(is.na(.) ~ "Insufficient Data",
                                                    TRUE ~ .))) %>%
    mutate(across(c(starts_with("smc")), 
                  ~ factor(., levels = c("Eligible", "Eligible + 10%", "Not Eligible",
                                         "Annual cases <= 250", "Insufficient Data")))) %>%
    filter(!is.na(geo_id)) %>%
    mutate(popup_txt = paste0("Country: <b>", country, "</b><br>Admin 1: <b>", admin1,
                              "</b><br>Admin2: <b>", admin2, "</b>")) %>%
    mutate(smc_in = replace(smc_in, is.na(smc_in), "Insufficient Data"))
  
  
  # s2in <- s2_map %>% filter( (country %in% ctry) & (variable %in% var)) %>%
  #   rename(smc_in = var_cutoff) %>%
  #   mutate(smc_in = replace(smc_in, is.na(smc_in), "Insufficient Data"))
  s1in <- s1 %>% filter(country %in% ctry)
  
  y <- list(xin = xin, s2in = s2in, s1in = s1in, cutoff = cutoff)
}



get_data_table <- function(ctry, var, cutoff) {
  
  ct <- as.integer(gsub("%", "", cutoff))
  
  out <- xtxt %>% filter( (country %in% ctry) & (variable == var) & (cutoff == ct) ) %>%
    filter(cutoff == ct) %>%  select(-c(country, variable, cutoff)) %>%
    rename("Admin Level 2" = "admin_level_2", "Admin Level 1" = "admin_level_1",
           "Total Annual" = "total", "# Of Eligible Period" = "n", "Eligible Period" = "txt")
    (out)
}


dummy_data <- data.frame(x = c(0,0), y = c(0,0), group = c("a", "b"))
