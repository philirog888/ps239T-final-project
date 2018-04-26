#Load the company ticker listing and other data obtained from previous research
library(foreign)
setwd("~/Documents/PS239T/Final Project")
chinese_companies_info <- as.data.frame(read.csv("chinese_company_list_for_api.csv"))

# Construct a list of primary symbols.
primary_symbols_list <- chinese_companies_info$Symbol

#Set up the components of the get requests url
edgar_api_url_base <- "http://datafied.api.edgar-online.com/v2/"
core_financials_package <- "corefinancials/"
annual <- "ann.json?"
primary_symbol <- "primarysymbols="
num_periods <- "&numperiods=1"
fields <- "&fields=companyname,primaryexchange,marketoperator,markettier,sicdescription,formtype,fiscalyear,incomebeforetaxes,netincome,totalassets,totalliabilities"
key <- "&appkey=5vxhbnrunkq8m3hxade48tsw"

#Create a function to get the annual data for each specific company
ann_dat_fun <- function(x)
{
  ann_dat_request <- paste0(edgar_api_url_base, core_financials_package, annual, primary_symbol, x, num_periods, fields, key)
  ann_dat_get <- httr::GET(url = ann_dat_request)
  
  #convert raw data from JSON
  ann_dat_json <- httr::content(x = ann_dat_get, as = "text")
  ann_dat <- jsonlite::fromJSON(txt = ann_dat_json, simplifyDataFrame = TRUE, flatten = TRUE, simplifyVector = FALSE)
  
  #dig into where the data actually are and extract them
  ann_dat_list <- ann_dat[["result"]][["rows"]][["values"]]
  ann_dat_unlist <- unlist(ann_dat_list)
  ann_dat_values <- t(as.data.frame(unname(ann_dat_unlist[grepl("value",names(ann_dat_unlist))])))
  
  #return the values
  return(ann_dat_values)

}

#Set up the construction of a dataframe 
api_variables <- as.data.frame(matrix(nrow = length(primary_symbols_list), ncol = 11))

#fill the dataframe by using a for loop
for (i in 1:nrow(api_variables))
{
  api_variables[i,] <- ann_dat_fun(primary_symbols_list[i])
}

#name the columns and view the dataframe
colnames(api_variables) <- c("companyname", "primaryexchange", "marketoperator", "markettier", "sicdescription", "formtype", "fiscalyear", "incomebeforetaxes", "netincome", "totalassets", "totalliabilities")
View(api_variables)

#bind together with information on ticker symbol, listing year and VIE status
Chinese_Company_df <- cbind(chinese_companies_info$Symbol, api_variables, chinese_companies_info$List_Year, chinese_companies_info$Is_VIE.)
colnames(Chinese_Company_df)[1] = "symbol"
colnames(Chinese_Company_df)[13] = "list_year"
colnames(Chinese_Company_df)[14] = "is_vie"

#Save the dataframe
save(Chinese_Company_df, file = "Chinese_Company_df.RData")