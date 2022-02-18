library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(highcharter)
library(formattable)
library(skimr)
library(glue)
library(leaflet)
library(viridis)
library(forcats)
library(ggplot2)
library(hrbrthemes)
library(ggbeeswarm)
library(curl)
library(readr)
library(rvest)
library(plotly)
library(lubridate)
library(RCurl)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plotly)
library(purrr)
library(stringr)
library(realtR)
library(highcharter)
library(formattable)
library(skimr)
library(glue)
library(leaflet)
library(viridis)
library(forcats)

# utah_sgid = read.csv("/Users/langford/OneDrive - Adobe Systems Incorporated/Adobe/Outside Group Presentations/UVU/R Example Code/Utah_ZIP_Code_Areas.csv")
# utah_sgid %>% 
#   head()
# utah_sgid %>% 
#   summary()
# 
# utah_county_zipcodes = utah_sgid %>%
#   filter(COUNTYNBR == 25) %>%
#   distinct(ZIP5) %>%
#   pull(ZIP5)


myMedianPrices = function (locations = NULL, return_message = TRUE) 
{
  if (locations %>% is_null()) {
    stop("Please enter location names!!")
  }

  .generate_market_urls_safe <- possibly(realtR:::.generate_market_urls, 
                                         tibble())
  df_urls <- .generate_market_urls_safe(locations = locations)
  if (df_urls %>% nrow() == 0) {
    "No results" %>% cat(fill = T)
    return(invisible())
  }
  .parse_market_data_urls_safe <- possibly(myParseMarketDataUrls, 
                                           tibble())
  all_data <- .parse_market_data_urls_safe(urls = df_urls$urlAPI, 
                                           return_message = return_message)
  if (all_data %>% nrow() == 0) {
    "No results" %>% cat(fill = T)
    return(invisible())
  }
  if (all_data %>% rlang:::has_name("priceRentMedian")) {
    all_data <- all_data %>% mutate(pctRentYield = (priceRentMedian * 
                                                      12)/priceListingMedian)
  }
  if (all_data %>% rlang:::has_name("priceListingMedian")) {
    all_data <- all_data %>% mutate(areaPropertySFMedian = (priceListingMedian/pricePerSFMedian) %>% 
                                      round(digits = 2))
  }
  all_data <- all_data %>% left_join(df_urls, by = "urlAPI") %>% 
    select(one_of(c("locationSearch", "streetOrNeighborhoodSearch", 
                    "citySearch", "stateSearch", "pricePerSFMedian", 
                    "priceRentMedian", "pctRentYield", "areaPropertySFMedian")), 
           everything()) %>% suppressWarnings() %>% realtR:::remove_na() %>% 
    realtR:::remove_columns()
  all_data
}

myParseMarketDataUrls = function (urls = "https://www.realtor.com/median_prices?city=Bethesda&state_code=MD", 
          return_message = TRUE) 
{
  .parse_market_data_url_safe <- possibly(myParseMarketDataUrl, 
                                          tibble())
  urls %>% map_dfr(function(url) {
    if (return_message) {
      glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>% 
        cat(fill = T)
    }
    .parse_market_data_url_safe(url = url)
  })
}

myParseMarketDataUrl = function (url = "https://www.realtor.com/median_prices?city=Bethesda&state_code=MD") 
{
  data <- url %>% jsonlite:::fromJSON(simplifyVector = T, 
                                      simplifyDataFrame = T, flatten = T)
  data <- data$data %>% flatten_df() %>% dplyr::select(-dplyr::matches("_display"))
  df_names <- realtR:::dictionary_realtor_names()
  actual_names <- names(data) %>% map_chr(function(name) {
    df_row <- df_names %>% filter(nameRealtor == name)
    if (df_row %>% nrow() == 0) {
      glue("Missing {name}") %>% cat(fill = T)
      return(name)
    }
    df_row %>% pull(nameActual)
  })
  data <- data %>% set_names(actual_names) %>% mutate(urlAPI = url)
  if (data %>% rlang:::has_name("pricePerSF")) {
    data <- data %>% dplyr::rename(pricePerSFMedian = pricePerSF)
  }
  data
}

myCurlJSON = function (url) 
{
  df_call <- realtR:::generate_url_reference()
  curl_obj = curl(url, handle = new_handle())
  json_data = jsonlite::fromJSON(url)
  json_data
  
  test = jsonlite:::fromJSON(txt = url,
                      simplifyVector = T, 
                      simplifyDataFrame = T, flatten = T)
}


myTrendsZipcodes = function(zipcodes, return_message = FALSE) {
  if (zipcodes %>% is_null()) {
    stop("Enter a vector of zipcodes")
  }
  df_urls <- realtR:::.generate_zipcode_trend_urls(zipcodes = zipcodes)
  .parse_zip_trend_url_safe <- possibly(realtR:::.parse_zip_trend_url, 
                                        tibble())
  all_data <- 1:nrow(df_urls) %>% map_df(function(x) {
    df_row <- df_urls %>% dplyr::slice(x)
    url <- df_row$urlTrendAPI
    zip <- df_row$zipcodeLocation
    if (return_message) {
      glue("Acquiring market trends for zipcode: {zip}") %>% 
        cat(fill = T)
    }
    data <- .parse_zip_trend_url_safe(url = url)
    data
  })
  
  available_trends = all_data %>% distinct(urlTrendAPI)
  df_urls = df_urls %>% filter(urlTrendAPI %in% available_trends$urlTrendAPI)
  
  all_data <- all_data %>% left_join(df_urls, by = "urlTrendAPI") %>% 
    dplyr::select(zipcodeLocation, everything())
  
  return(all_data)
}

marketTrendPlot = function(market_data, plot_cols, my_zips) {

  df_plot_data <-
    market_data %>%
    filter(postal_code %in% my_zips) %>%
    mutate(postal_code = factor(postal_code)) %>%
    select(Date, postal_code, one_of(plot_cols)) %>%
    pivot_longer(-c(Date, postal_code), names_to = "Metric", values_to = "Value") %>%
    mutate(Value = case_when(is.na(Value) ~ 0, TRUE ~ Value))
  
  
  plot = df_plot_data %>%
    ggplot(aes(x = Date, y = Value, colour = postal_code)) +
    geom_line(alpha = 0.8) +
    facet_wrap( ~ Metric, nrow = 3,    scales = "free") +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    hrbrthemes::scale_colour_ipsum() +
    scale_x_date(date_breaks = '4 months',
                 date_labels = "%Y-%m") +
    ggtitle(label = "Market Trend Exploration")
  
  return(plot)
}



myListings = function (locations = NULL, listing_type = "sale", search_type = "city", 
                       city_isolated = NULL, county_isolated = NULL, zipcode_isolated = NULL, 
                       state_isolated = NULL, street_isolated = NULL, features = NULL, 
                       only_open_houses = NULL, neighborhood_isolated = NULL, beds_min = NULL, 
                       beds_max = NULL, baths_min = NULL, baths_max = NULL, price_min = NULL, 
                       price_max = NULL, property_type = NULL, sqft_min = NULL, 
                       sqft_max = NULL, acre_min = NULL, acre_max = NULL, age_min = NULL, 
                       age_max = NULL, days_on_market = NULL, pending = NULL, is_new_construction = NULL, 
                       generate_new_cookies = F, include_pending_contingency = TRUE, 
                       sleep_time = 5) 
{
  if (locations %>% is_null()) {
    stop("Enter locations")
  }
  
  .get_location_listings_safe <- possibly(myGetLocationListings, 
                                          tibble())
  all_data <- locations %>% map_dfr(function(location) {
    data <- .get_location_listings_safe(location_name = as.character(location), 
                                        listing_type = listing_type, search_type = search_type, 
                                        city_isolated = city_isolated, county_isolated = county_isolated, 
                                        zipcode_isolated = zipcode_isolated, state_isolated = state_isolated, 
                                        street_isolated = street_isolated, generate_new_cookies = generate_new_cookies, 
                                        neighborhood_isolated = neighborhood_isolated, beds_min = beds_min, 
                                        beds_max = beds_max, baths_min = baths_min, baths_max = baths_max, 
                                        price_min = price_min, price_max = price_max, property_type = property_type, 
                                        sqft_min = sqft_min, sqft_max = sqft_max, acre_min = acre_min, 
                                        acre_max = acre_max, age_min = age_min, age_max = age_max, 
                                        days_on_market = days_on_market, pending = pending, 
                                        is_new_construction = is_new_construction, include_pending_contingency = include_pending_contingency, 
                                        features = features, only_open_houses = only_open_houses)
    if (!sleep_time %>% is_null()) {
      Sys.sleep(time = sleep_time)
    }
    data
  }) %>% suppressWarnings()
  all_data <- all_data %>% realtR:::remove_columns() %>% group_by(urlListing) %>% 
    mutate(idListing = 1:n()) %>% filter(idListing == min(idListing)) %>% 
    ungroup() %>% select(-idListing) %>% realtR:::.add_date()
  all_data
}

myGetLocationListings = function (location_name = 10016, listing_type = "sale", search_type = "city", 
          city_isolated = NULL, county_isolated = NULL, zipcode_isolated = NULL, 
          state_isolated = NULL, street_isolated = NULL, features = NULL, 
          only_open_houses = NULL, neighborhood_isolated = NULL, beds_min = NULL, 
          beds_max = NULL, baths_min = NULL, baths_max = NULL, price_min = NULL, 
          price_max = NULL, property_type = NULL, sqft_min = NULL, 
          sqft_max = NULL, acre_min = NULL, acre_max = NULL, age_min = NULL, 
          age_max = NULL, days_on_market = NULL, pending = NULL, is_new_construction = NULL, 
          generate_new_cookies = T, include_pending_contingency = TRUE) 
{
  if (location_name %>% str_to_lower() %>% str_detect("county")) {
    search_type <- "county"
  }
  listing_counts.safe <- possibly(listing_counts, tibble())
  df_count <- listing_counts.safe(locations = location_name, 
                                  listing_type = listing_type, search_type = search_type, 
                                  features = features, city_isolated = city_isolated, county_isolated = county_isolated, 
                                  zipcode_isolated = zipcode_isolated, state_isolated = state_isolated, 
                                  street_isolated = street_isolated, neighborhood_isolated = neighborhood_isolated, 
                                  beds_min = beds_min, beds_max = beds_max, baths_min = baths_min, 
                                  baths_max = baths_max, price_min = price_min, price_max = price_max, 
                                  only_open_houses = only_open_houses, property_type = property_type, 
                                  sqft_min = sqft_min, sqft_max = sqft_max, acre_min = acre_min, 
                                  acre_max = acre_max, age_min = age_min, age_max = age_max, 
                                  days_on_market = days_on_market, generate_new_cookies = generate_new_cookies, 
                                  pending = pending, is_new_construction = is_new_construction, 
                                  include_pending_contingency = include_pending_contingency)
  if (df_count %>% nrow() == 0) {
    return(invisible())
  }
  pages <- df_count$countListings%/%50
  pages <- max(1, pages)
  headers <- realtR:::.headers_search_json(generate_new_cookies = generate_new_cookies)
  all_properties <- 1:pages %>% map_dfr(possibly(function(page_no) {
    glue("Parsing page {page_no} of {pages} for location {location_name}") %>% 
      cat(fill = T)
    if (page_no == 1) {
      url <- "https://www.realtor.com/search_result"
    }
    else {
      url <- "https://www.realtor.com/pagination_result"
    }
    data <- realtR:::.generate_data(location_name = location_name, 
                           search_type = search_type, listing_type = listing_type, 
                           page = page_no, city_isolated = city_isolated, county_isolated = county_isolated, 
                           zipcode_isolated = zipcode_isolated, state_isolated = state_isolated, 
                           street_isolated = street_isolated, neighborhood_isolated = neighborhood_isolated, 
                           beds_min = beds_min, beds_max = beds_max, baths_min = baths_min, 
                           baths_max = baths_max, price_min = price_min, price_max = price_max, 
                           property_type = property_type, sqft_min = sqft_min, 
                           sqft_max = sqft_max, acre_min = acre_min, acre_max = acre_max, 
                           age_min = age_min, age_max = age_max, days_on_market = days_on_market, 
                           pending = pending, is_new_construction = is_new_construction, 
                           include_pending_contingency = include_pending_contingency, 
                           features = features, only_open_houses = only_open_houses)

    df_params <- realtR:::.parse_data_parameters(data_param = data)
    df_call <- realtR:::generate_url_reference()
    h <- new_handle(verbose = F, useragent = df_call$urlReferer) %>% 
      handle_setopt(copypostfields = data %>% jsonlite:::toJSON(auto_unbox = T), 
                    customrequest = "POST") %>% handle_setheaders(.list = headers %>% 
                                                                    as.list())
    resp <- curl_fetch_memory(url = url, handle = new_handle())
    content <- resp$content %>% rawToChar() %>% str_split("\n") %>% 
      flatten_chr() %>% str_c(collapse = "")
    page <- realtR:::.parse_content_to_page(content = content)
    page_nodes <- page %>% rvest:::html_nodes(".component_property-card")
    
    if (search_type %>% str_to_lower() != "rent") {
      browser()
      data_prop <- seq_along(page_nodes) %>% map_dfr(function(x) {
        fact_node <- page_nodes[[x]]
        if (fact_node %>% rvest:::html_attr("class") %>% str_detect("ads-wrapper")) {
          return(invisible())
        }
        page_node <- fact_node %>% rvest:::html_nodes(".data-wrap") %>% 
          rvest:::html_attrs()
        if (length(page_node) == 0) {
          return(invisible())
        }
        wrap_nodes <- page_node %>% .[[1]]
        wrap_names <- names(wrap_nodes)
        wrap_values <- as.character(wrap_nodes)
        df_wrap <- tibble(name = wrap_names, value = wrap_values) %>% 
          filter(!name == "class")
        data_atrs <- fact_node %>% html_attrs()
        df_attrs <- tibble(name = names(data_atrs), value = data_atrs %>% 
                             as.character()) %>% bind_rows(df_wrap) %>% 
          filter(!name %in% c("class", "data-lead_attributes", 
                              "data-search_flags")) %>% distinct()
        df_json_rows <- df_attrs %>% filter(name %in% 
                                              c("data-lead_attributes", "data-search_flags"))
        df_base <- df_attrs %>% filter(!name %in% c("class", 
                                                    "data-lead_attributes", "data-search_flags"))
        meta_nodes <- fact_node %>% html_nodes("meta")
        meta_values <- meta_nodes %>% html_attr("content")
        meta_names <- meta_nodes %>% html_attr("itemprop")
        df_meta <- tibble(name = meta_names, value = meta_values)
        df_base <- df_base %>% bind_rows(df_meta) %>% 
          distinct()
        property_nodes <- fact_node %>% html_nodes(".seo-wrap span")
        property_names <- property_nodes %>% html_attr("itemprop")
        property_values <- property_nodes %>% html_text() %>% 
          str_trim() %>% gsub("\\s+", " ", .)
        df_property <- tibble(name = property_names, 
                              value = property_values) %>% filter(!value == 
                                                                    "")
        df_base <- df_base %>% bind_rows(df_property) %>% 
          distinct()
        broker_node <- fact_node %>% html_nodes(".broker-info span")
        broker_name <- broker_node %>% html_attr("data-label") %>% 
          discard(is.na)
        broker_value <- broker_node %>% html_text() %>% 
          str_trim() %>% str_c(collapse = " ") %>% str_remove_all("Brokered by") %>% 
          str_trim()
        df_broker <- tibble(name = broker_name, value = broker_value)
        bf_base <- df_base %>% bind_rows(df_broker) %>% 
          distinct()
        if (df_json_rows %>% nrow() > 0) {
          df_json_data <- 1:nrow(df_json_rows) %>% map_dfr(function(x) {
            df_json_rows %>% dplyr::slice(x) %>% pull(value) %>% 
              fromJSON() %>% flatten_df() %>% mutate_all(as.character) %>% 
              gather(name, value)
          })
          df_base <- df_base %>% bind_rows(df_json_data)
        }
        has_image <- fact_node %>% html_nodes(".photo-wrap img") %>% 
          length() > 0
        if (has_image) {
          image_node <- fact_node %>% html_nodes(".photo-wrap img")
          image_url <- image_node %>% html_attr("src") %>% 
            .[[1]]
          address <- image_node %>% html_attr("title") %>% 
            .[[1]]
          df_base <- df_base %>% bind_rows(tibble(name = c("addressPropertyFull", 
                                                           "urlImage"), value = c(address, image_url)))
        }
        df_base <- df_base %>% mutate(numberListing = x) %>% 
          filter(!name %in% c("data-rank", "id", "brand", 
                              "productID", "image", "manufacturer", "URL", 
                              "category")) %>% select(numberListing, everything())
        df_base
      })
    }
    else {
      data_prop <- seq_along(page_nodes) %>% map_dfr(function(x) {
        df_base <- tibble()
        meta_nodes <- fact_node %>% html_nodes("meta")
        meta_values <- meta_nodes %>% html_attr("content")
        meta_names <- meta_nodes %>% html_attr("itemprop")
        df_meta <- tibble(name = meta_names, value = meta_values)
        df_base <- tibble()
        df_base <- df_base %>% bind_rows(df_meta) %>% 
          distinct()
        property_nodes <- fact_node %>% html_nodes(".seo-wrap span")
        property_names <- property_nodes %>% html_attr("itemprop")
        property_values <- property_nodes %>% html_text() %>% 
          str_trim() %>% gsub("\\s+", " ", .)
        df_property <- tibble(name = property_names, 
                              value = property_values) %>% filter(!value == 
                                                                    "")
        df_base <- df_base %>% bind_rows(df_property) %>% 
          distinct()
        broker_node <- fact_node %>% html_nodes(".broker-info span")
        broker_name <- broker_node %>% html_attr("data-label") %>% 
          discard(is.na)
        broker_value <- broker_node %>% html_text() %>% 
          str_trim() %>% str_c(collapse = " ") %>% str_remove_all("Brokered by") %>% 
          str_trim()
        df_broker <- tibble(name = broker_name, value = broker_value)
        bf_base <- df_base %>% bind_rows(df_broker) %>% 
          distinct()
        has_image <- fact_node %>% html_nodes(".photo-wrap img") %>% 
          length() > 0
        if (has_image) {
          image_node <- fact_node %>% html_nodes(".photo-wrap img")
          image_url <- image_node %>% html_attr("src") %>% 
            .[[1]]
          address <- image_node %>% html_attr("title") %>% 
            .[[1]]
          df_base <- df_base %>% bind_rows(tibble(name = c("addressPropertyFull", 
                                                           "urlImage"), value = c(address, image_url)))
        }
        df_base <- df_base %>% mutate(numberListing = x) %>% 
          filter(!name %in% c("data-rank", "id", "brand", 
                              "productID", "image", "manufacturer", "URL", 
                              "category")) %>% select(numberListing, everything())
        df_base
      })
    }
    df_prop <- data_prop %>% left_join(realtR:::dictionary_css_page() %>% 
                                         rename(name = id)) %>% suppressMessages()
    if (df_prop %>% filter(nameActual %>% is.na()) %>% nrow() > 
        0) {
      missing_names <- df_prop %>% filter(nameActual %>% 
                                            is.na()) %>% pull(name) %>% unique() %>% str_c(collapse = "\n")
      glue("Missing {missing_names}") %>% cat(fill = T)
    }
    df_prop <- df_prop %>% select(numberListing, nameActual, 
                                  value) %>% mutate_all(funs(ifelse(. == "", NA_character_, 
                                                                    .))) %>% filter(!is.na(value)) %>% filter(!nameActual %>% 
                                                                                                                str_detect("remove_")) %>% distinct() %>% group_by(numberListing, 
                                                                                                                                                                   nameActual) %>% mutate(id = 1:n()) %>% ungroup() %>% 
      filter(id == min(id)) %>% select(-id) %>% spread(nameActual, 
                                                       value)
    df_prop <- df_prop %>% realtR:::.munge_realtor() %>% suppressMessages() %>% 
      mutate(numberPage = page_no) %>% select(numberPage, 
                                              everything())
    df_prop
  }, tibble()))
  df_count_merge <- df_count %>% select(-one_of(c("cityProperty", 
                                                  "urlListing", "countListings", "numberPage", "typeProperty", 
                                                  "zipcodeProperty")))
  all_data <- all_properties %>% 
    mutate(id = 1) %>% 
    left_join(df_count_merge %>% 
              mutate(id = 1)) %>% 
    select(-id) %>% 
    select(one_of(names(df_count_merge)), everything()) %>% 
    select(-numberPage) %>% 
    distinct() %>% 
    suppressMessages()
  all_data <- all_data %>% mutate(urlListing = urlListing %>% 
                                    gsub("https://www.realtor.com//", "https://www.realtor.com/", 
                                         .))
  all_data <- all_data %>% mutate(urlPropertyAPI = glue("https://www.realtor.com/property-overview/M{idProperty}") %>% 
                                    as.character())
  all_data
}
