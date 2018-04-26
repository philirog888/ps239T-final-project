#load the the plotting packages
library(ggplot2)
library(waffle)

#load the data frame
setwd("~/Documents/PS239T/Final Project")
load("Chinese_Company_df.RData")

#prepare the squares for a waffle plot breaking down the composition of all companies in terms of VIE usage
use_vie <- sum(Chinese_Company_df$is_vie)
no_use_vie <- sum(as.numeric(Chinese_Company_df$is_vie==0))

#waffle plot for the composition of all companies in terms of VIE usage
vie_or_not_parts <- c("Uses VIE"=use_vie, "Does Not Use VIE"=no_use_vie)
vie_or_not_waffle <- waffle(vie_or_not_parts, rows = 10, xlab = "One Square Per Company (n=168", size = .5, colors = c("Black", "Gray"), title = "Breakdown by VIE Usage")
vie_or_not_waffle

#prepare the counts of the market tier categories for VIE and non-VIE companies
NYSE_vie <- sum(as.numeric(Chinese_Company_df$markettier=="NYSE" & Chinese_Company_df$is_vie==1))
NYSE_nonvie <- sum(as.numeric(Chinese_Company_df$markettier=="NYSE" & Chinese_Company_df$is_vie==0))
NYSE_MKT_vie <- sum(as.numeric(Chinese_Company_df$markettier=="NYSE MKT" & Chinese_Company_df$is_vie==1))
NYSE_MKT_nonvie <- sum(as.numeric(Chinese_Company_df$markettier=="NYSE MKT" & Chinese_Company_df$is_vie==0))
OTCQB_vie <- sum(as.numeric(Chinese_Company_df$markettier=="OTCQB" & Chinese_Company_df$is_vie==1))
OTCQB_nonvie <- sum(as.numeric(Chinese_Company_df$markettier=="OTCQB" & Chinese_Company_df$is_vie==0))
NASDAQ_global_select_market_vie <- sum(as.numeric(Chinese_Company_df$markettier=="NASDAQ Global Select Market" & Chinese_Company_df$is_vie==1))
NASDAQ_global_select_market_nonvie <- sum(as.numeric(Chinese_Company_df$markettier=="NASDAQ Global Select Market" & Chinese_Company_df$is_vie==0))
NASDAQ_global_market_vie <- sum(as.numeric(Chinese_Company_df$markettier=="NASDAQ Global Market" & Chinese_Company_df$is_vie==1))
NASDAQ_global_market_nonvie <- sum(as.numeric(Chinese_Company_df$markettier=="NASDAQ Global Market" & Chinese_Company_df$is_vie==0))
NASDAQ_capital_market_vie <- sum(as.numeric(Chinese_Company_df$markettier=="NASDAQ Capital Market" & Chinese_Company_df$is_vie==1))
NASDAQ_capital_market_nonvie <- sum(as.numeric(Chinese_Company_df$markettier=="NASDAQ Capital Market" & Chinese_Company_df$is_vie==0))

#prepare VIE User barplot
vie_market_tier_df <- as.data.frame(rbind(NYSE_vie, NYSE_MKT_vie, OTCQB_vie, NASDAQ_global_select_market_vie, NASDAQ_global_market_vie, NASDAQ_capital_market_vie))
vie_market_tier_df$Market_Tier <- c("NYSE", "NYSE MKT", "OTCQB", "NASDAQ Global Select", "NASDAQ Global", "NASDAQ Capital")
colnames(vie_market_tier_df)[1] <- c("Count")
rownames(vie_market_tier_df) <- c(1:6)
vie_user_barplot <- ggplot(vie_market_tier_df, aes(x=Market_Tier, y=Count)) + geom_bar(stat = "identity")
vie_user_barplot <- vie_user_barplot + theme(axis.text.x = element_text(angle=70, vjust=0.6)) + labs(x="Market Tier", y="Count") + ggtitle("Market Tier Frequencies Among VIE Users (N=86")
vie_user_barplot

#prepare Non-VIE User barplot
non_vie_market_tier_df <- as.data.frame(rbind(NYSE_nonvie, NYSE_MKT_nonvie, OTCQB_nonvie, NASDAQ_global_select_market_nonvie, NASDAQ_global_market_nonvie, NASDAQ_capital_market_nonvie))
non_vie_market_tier_df$Market_Tier <- c("NYSE", "NYSE MKT", "OTCQB", "NASDAQ Global Select", "NASDAQ Global", "NASDAQ Capital")
colnames(non_vie_market_tier_df)[1] <- c("Count")
rownames(non_vie_market_tier_df) <- c(1:6)
non_vie_user_barplot <- ggplot(vie_market_tier_df, aes(x=Market_Tier, y=Count)) + geom_bar(stat = "identity")
non_vie_user_barplot <- non_vie_user_barplot + theme(axis.text.x = element_text(angle=70, vjust=0.6)) + labs(x="Market Tier", y="Count") + ggtitle("Market Tier Frequencies Among Non-VIE Users (N=82")
non_vie_user_barplot

#create a "qualitative" variable indicating yes or no for VIE usage
yes_no_fun <- function(x)
{
 ifelse(x==1, "Yes", "No")
}

Chinese_Company_df$is_vie_qual <- t(as.data.frame(lapply(Chinese_Company_df$is_vie, yes_no_fun)))

#box and whisker plot comparing the listing year for VIE companies and non-VIE companies (has the
#use of the VIE been relatively spread out over time, or has it clumped around specific years?)
list_year_boxplot <- ggplot(Chinese_Company_df, aes(x =is_vie_qual, y =list_year)) + geom_boxplot()
list_year_boxplot <- list_year_boxplot + ggtitle("Boxplot of Chinese Companies' Initial Listing Years")  + scale_x_discrete(name = "VIE Usage") + scale_y_continuous(name = "Year of Initial Listing")
list_year_boxplot <- list_year_boxplot + coord_flip() + theme_bw()
list_year_boxplot

#create correlation coefficients (are there associational patterns between
#company-level characteristics and VIE usage?)
income_before_taxes_vie_cor <- cor(as.numeric(Chinese_Company_df$incomebeforetaxes), Chinese_Company_df$is_vie)
net_income_vie_cor <- cor(as.numeric(Chinese_Company_df$netincome), Chinese_Company_df$is_vie)
total_assets_vie_cor <- cor(as.numeric(Chinese_Company_df$totalassets), Chinese_Company_df$is_vie)
total_liabilities_vie_cor <- cor(as.numeric(Chinese_Company_df$totalliabilities), Chinese_Company_df$is_vie)

#create a correlation coefficient dataframe
characteristics <- c("Income Before Taxes", "Net Income", "Total Assets", "Total Liabilities")
cor_coef_df <- as.data.frame(rbind(income_before_taxes_vie_cor, net_income_vie_cor, total_assets_vie_cor, total_liabilities_vie_cor))
cor_coef_df <- cbind(characteristics, cor_coef_df)
colnames(cor_coef_df) <- c("Characteristic", "Correlation_Coefficient")
rownames(cor_coef_df) <- c("1", "2", "3", "4")

#create a barplot showing the (lack of!) correlation
characteristics_barplot <- ggplot(cor_coef_df, aes(x=Characteristic, y=Correlation_Coefficient )) + geom_bar(stat = "identity")
characteristics_barplot <- characteristics_barplot + scale_y_continuous(limits = c(-1,1), name = "Correlation Coefficient") + ggtitle("Correlation Between VIE Usage and Company Characteristics") + theme_bw()
characteristics_barplot







