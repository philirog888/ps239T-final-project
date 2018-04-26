edgar_api_url_base <- "http://datafied.api.edgar-online.com/v2/"

#it appears there are three main packagages: core financials, insider trades, and institutional ownership.
#I'm looking at core financials

core_financials_package <- "corefinancials/"

#the api offers both quarterly and annual data, as well as company level meta-data
annual <- "ann.json?"
company_meta <- "companies.json?"
primary_symbol <- "primarysymbols="
num_periods <- "&numperiods=1"


#my api key
key <- "&appkey=5vxhbnrunkq8m3hxade48tsw"

#the url request for annual data
ann_dat_request <- paste0(edgar_api_url_base, core_financials_package, annual, primary_symbol, num_periods, key)

#GET request for annual data
ann_dat_get <- httr::GET(url = ann_dat_request)
ann_dat_json <- httr::content(x = ann_dat_get, as = "text")
ann_dat <- jsonlite::fromJSON(txt = ann_dat_json, simplifyDataFrame = TRUE, flatten = TRUE, simplifyVector = FALSE)

#it appears the ultimate sub-element we are looking for is nested as company_meta_dat >results > rows)
ann_dat_list <- ann_dat$result$rows$values
ann_dat_list
