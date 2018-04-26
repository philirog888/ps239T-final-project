edgar_api_url_base <- "http://datafied.api.edgar-online.com/v2/"

#it appears there are three main packagages: core financials, insider trades, and institutional ownership.
#I'm looking at core financials

core_financials_package <- "corefinancials/"

#the api offers both quarterly and annual data, as well as company level meta-data
annual <- "ann.json?"
company_meta <- "companies.json?"

primary_symbol <- "primarysymbols=BIDU+WBAI"

#my api key
key <- "&appkey=5vxhbnrunkq8m3hxade48tsw"

#the url request for company-level meta data
company_meta_dat_request <- paste0(edgar_api_url_base, company_meta, primary_symbol, key)

#GET request for company meta data
company_meta_dat_get <- httr::GET(url = company_meta_dat_request)
company_meta_dat_json <- httr::content(x = company_meta_dat_get, as = "text")
company_meta_dat <- jsonlite::fromJSON(txt = company_meta_dat_json, simplifyDataFrame = TRUE, flatten = TRUE, simplifyVector = FALSE)

#Digging out the company meta data
#Inspect the nature of the company meta data
class(company_meta_dat) #it's a list of one with sub-elements inside
company_meta_dat 

#it appears the ultimate sub-element we are looking for is nested as company_meta_dat >results > rows)
company_meta_dat_list <- company_meta_dat$result$rows$values
class(company_meta_dat_list)
View(company_meta_dat_list)
#this yields a lovely "table", but it needs to be reshaped (the elements in thefirst column would make
#great column names, and the values in the second column are pieces of data we are interested in).

#unlist the elements in company_meta_dat_list to get them as individual elements
company_meta_dat_unlist <- unlist(company_meta_dat_list)
company_meta_dat_fields <- unname(company_meta_dat_unlist[grepl("field",names(company_meta_dat_unlist))])
company_meta_dat_values <- unname(company_meta_dat_unlist[grepl("value",names(company_meta_dat_unlist))])

#and construct the data frame for the company meta data
company_meta_dat_df <- t(as.data.frame(company_meta_dat_values))
colnames(company_meta_dat_df) <- company_meta_dat_fields
rownames(company_meta_dat_df) <- as.character(nrow(df))
View(company_meta_dat_df)

#the url request for annual data
ann_dat_request <- paste0(edgar_api_url_base, core_financials_package, annual, primary_symbol, key)

#GET request for annual data
ann_dat_get <- httr::GET(url = ann_dat_request)
ann_dat_json <- httr::content(x = ann_dat_get, as = "text")
ann_dat <- jsonlite::fromJSON(txt = ann_dat_json, simplifyDataFrame = TRUE, flatten = TRUE, simplifyVector = FALSE)

#Digging out the company annual data
#Inspect the nature of the company meta data
class(ann_dat) #it's a list of one with sub-elements inside
View(ann_dat) 

#it appears the ultimate sub-element we are looking for is nested as company_meta_dat >results > rows)
ann_dat_list <- ann_dat$result$rows$values
ann_dat_list
#this yields a lovely "table", but it needs to be reshaped (the elements in thefirst column would make
#great column names, and the values in the second column are pieces of data we are interested in).
#Moreover, all we really want for purposes of the project is fiscal year 2017 data. That corresponds to
#"row 1".

#unlist the elements in company_meta_dat_list to get them as individual elements
ann_dat_unlist <- unlist(ann_dat[["result"]][["rows"]][["values"]])
ann_dat_fields <- unname(ann_dat_unlist[grepl("field",names(ann_dat_unlist))])
ann_dat_values <- unname(ann_dat_unlist[grepl("value",names(ann_dat_unlist))])

#and construct the data frame for the company meta data
ann_dat_df <- t(as.data.frame(ann_dat_values))
colnames(ann_dat_df) <- ann_dat_fields
rownames(ann_dat_df) <- as.character(nrow(df))
View(ann_dat_df)
