#  Data available at: https://www.realtor.com/research/data/
#  https://econdata.s3-us-west-2.amazonaws.com/Reports/Hotness/RDC_Inventory_Hotness_Metrics_Zip_History.csv
#  https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_Zip_History.csv

source(str_glue("/Users/langford/OneDrive - Adobe Systems Incorporated/",
                "Adobe/Outside Group Presentations/Tech Moms/",
                "Real Estate Example/realEstate_Functions.R"))

readRealEstateFile = function(file_name, data_directory = NULL, download_url = FALSE, file_type, header = TRUE) {

  seperator = switch(file_type,
                     "csv" = ",",
                     "tab" = "\t")
  
  if(download_url == FALSE) {
    
    data_directory = paste("/Users/langford/OneDrive - Adobe Systems Incorporated/Adobe/",
                           "Outside Group Presentations/Tech Moms/Real Estate Example/",
                           sep = "")
    
  } else {
    
    options(timeout = 200)
    
    if(str_detect(tolower(file_name), "hotness")){
      metric_type = "Hotness"
    } else {
      metric_type = "Core"
    }

    data_directory = str_glue("https://econdata.s3-us-west-2.amazonaws.com/Reports/{metric_type}/")
  }
  
  full_file_directory = str_glue("{data_directory}{file_name}.{file_type}")
  data = read.table(file = full_file_directory, header = header, sep = seperator)

  return(data)
}

core_data = readRealEstateFile(file_name = "RDC_Inventory_Core_Metrics_Zip_History", 
                               download_url = FALSE,
                               file_type = "csv", 
                               header = TRUE) %>%
  mutate(month_date_yyyymm = as.character(month_date_yyyymm)) %>%
  rename(core_quality_flag = quality_flag)

hotness_data = readRealEstateFile(file_name = "RDC_Inventory_Hotness_Metrics_Zip_History", 
                                  download_url = FALSE,
                                  file_type = "csv", 
                                  header = TRUE) %>%
  filter(!is.na(hotness_rank)) %>%
  mutate(postal_code = as.numeric(postal_code)) %>%
  rename(hotness_quality_flag = quality_flag)


######################################
### UTAH MARKET TREND EXPLORATION  ###
######################################
market_data = core_data %>%
  left_join(hotness_data, by = c("month_date_yyyymm" = "month_date_yyyymm", "postal_code" = "postal_code")) %>%
  select(-contains(".y")) %>%
  rename_at(vars(ends_with(".x")), ~(str_replace(.,".x",""))) %>%
  mutate(month_date_yyyymm = paste0(month_date_yyyymm, "01")) %>%
  mutate(Date = as_date(month_date_yyyymm, format = "%Y%m%d"))

marketTrendPlot(market_data = market_data,
                plot_cols = c("median_days_on_market", 
                              "active_listing_count", 
                              "median_listing_price"),
                my_zips = c(84005, 84045, 84017))



##########################################
### VACATION RENTAL TREND EXPLORATION  ###
##########################################
vacation_rental_cities <- c("Moab, UT", "Park City, UT", "Whitefish, MT",  
                            "Hilton Head, SC", "Myrtle Beach, SC", "Ocean City, MD",
                            "Rehoboth Beach, DE", "Outer Banks, NC")
df_median_vrc <-
  myMedianPrices(locations = vacation_rental_cities, return_message = F)

df_median_vrc %>%
  head()

df_median_vrc %>%
  arrange(desc(priceListingMedian)) %>%
  hchart(
    "scatter",
    backgroundColor = "#FFFFFF",
    style = list(fontSize = "2em"),
    fontFamily = "Helvetica",
    hcaes(
      x = priceListingMedian,
      y = pricePerSFMedian,
      group = stateSearch,
      size = areaPropertySFMedian
    )
  )  %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_plotOptions(series = list(
    dataLabels = list(
      enabled = TRUE,
      inside = TRUE,
      format = '{point.locationSearch}',
      color = 'black',
      padding = 0,
      font = '8px'
    )
  )) %>%
  hc_title(text = "Vacation Rental Market Median Price Exploration") %>%
  hc_subtitle(text = "Price PSF by Listing Price Sized by Home Area<br>Data As Of: 2022-01", align = "left") %>%
  hc_legend(
    layout = "vertical",
    verticalAlign = "top",
    align = "right",
    valueDecimals = 0
  )




