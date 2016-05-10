library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
library(gridExtra, warn.conflicts=FALSE, quietly=TRUE) 
library(GGally, warn.conflicts=FALSE, quietly=TRUE)
library(RColorBrewer, warn.conflicts=FALSE, quietly=TRUE)
library(memisc, warn.conflicts=FALSE, quietly=TRUE)
library(MASS, warn.conflicts=FALSE, quietly=TRUE)
library(lattice, warn.conflicts=FALSE, quietly=TRUE)
library(e1071, warn.conflicts=FALSE, quietly=TRUE) # stats package
library(zoo, warn.conflicts=FALSE, quietly=TRUE)


# --------------------------------------------
# Functions
# --------------------------------------------
# purpose: calculate moving average of given list or dataframe
# inputs: dataframe (x), moving average number (n)
# output: moving average of given series
movingAvg <- function(x, n = 5) {  
  return (stats::filter(x, rep(1/n, n), sides = 2))
}


# --------------------------------------------
# Loading
# --------------------------------------------
# load raw data from Kaggle csv files
setwd("D:/Github/Data_Meteorology_Finance")
# datasets on ENSO from Australia's BOM and US NOAA
bom_soi <- tbl_df(read.csv("Raw/bom_soi_historical.csv", header = FALSE, stringsAsFactors=FALSE))
noaa_enso <- tbl_df(read.csv("Raw/noaa_enso_historical.csv", stringsAsFactors=FALSE))
# datasets on weather in Melbourne, Melbourne from USA NOAA
noaa_usa <- tbl_df(read.csv("Raw/usa_noaa_temp_rain_monthly.csv", stringsAsFactors=FALSE))
# datasets on weather in Melbourne, Victoria from Australian BOM
aus_bom_rain_mean <- tbl_df(read.csv("Raw/aus_bom_rain_mean.csv", stringsAsFactors=FALSE))
aus_bom_temp_max_mean <- tbl_df(read.csv("Raw/aus_bom_temp_max_mean.csv", stringsAsFactors=FALSE))
aus_bom_temp_min_mean <- tbl_df(read.csv("Raw/aus_bom_temp_min_mean.csv", stringsAsFactors=FALSE))
# datasets on commodity markets from Quandl sources (IMF, CME, World Bank etc.)
quandl_agro_index_wb <- tbl_df(read.csv("Raw/quandl_agro_index_wb.csv", stringsAsFactors=FALSE))
quandl_copper_imf <- tbl_df(read.csv("Raw/quandl_copper_imf.csv", stringsAsFactors=FALSE))
quandl_corn_cme <- tbl_df(read.csv("Raw/quandl_corn_cme.csv", stringsAsFactors=FALSE))
quandl_gold_lbma <- tbl_df(read.csv("Raw/quandl_gold_lbma.csv", stringsAsFactors=FALSE))
quandl_nickel_imf <- tbl_df(read.csv("Raw/quandl_nickel_imf.csv", stringsAsFactors=FALSE))
quandl_palm_oil_imf <- tbl_df(read.csv("Raw/quandl_palm_oil_imf.csv", stringsAsFactors=FALSE))
quandl_rice_cme <- tbl_df(read.csv("Raw/quandl_rice_cme.csv", stringsAsFactors=FALSE))
quandl_soybean_cme <- tbl_df(read.csv("Raw/quandl_soybean_cme.csv", stringsAsFactors=FALSE))
quandl_soybean_oil_cme <- tbl_df(read.csv("Raw/quandl_soybean_oil_cme.csv", stringsAsFactors=FALSE))
quandl_wheat_cme <- tbl_df(read.csv("Raw/quandl_wheat_cme.csv", stringsAsFactors=FALSE))
quandl_zinc_imf <- tbl_df(read.csv("Raw/quandl_zinc_imf.csv", stringsAsFactors=FALSE))

# --------------------------------------------
# Wrangling
# --------------------------------------------
# Tidy data principles:
#   1. each variable forms a column
#   2. each observation forms a row
#   3. each type of observational unit forms a table
# Monthly Mean minimum temperature (MMNT)
# Monthly Mean maximum temperature (MMXT)
# Total precipitation (TPCP)
  
# check for NA
sum(is.na(bom_soi))
sum(is.na(noaa_enso))
sum(is.na(aus_bom_rain_mean))
sum(is.na(aus_bom_temp_max_mean))
sum(is.na(aus_bom_temp_min_mean))
sum(is.na(quandl_agro_index_wb))
sum(is.na(quandl_copper_imf))
sum(is.na(quandl_corn_cme$Open))
sum(is.na(quandl_gold_lbma[2]))
sum(is.na(quandl_nickel_imf))
sum(is.na(quandl_palm_oil_imf))
sum(is.na(quandl_rice_cme$Open))
sum(is.na(quandl_soybean_cme$Open))
sum(is.na(quandl_soybean_oil_cme$Open))
sum(is.na(quandl_wheat_cme$Open))
sum(is.na(quandl_zinc_imf))
quandl_gold_lbma$USD[is.na(quandl_gold_lbma[2])] <- 0
quandl_rice_cme$Open[is.na(quandl_rice_cme$Open)] <- 0
quandl_soybean_cme$Open[is.na(quandl_soybean_cme$Open)] <- 0
noaa_usa <- noaa_usa %>% filter(EMXT != -9999)

# column re-names
names(bom_soi) <- c("year","month","soi")
names(aus_bom_rain_mean)[5] <- "TPCP"
names(aus_bom_temp_max_mean)[5] <- "MMXT"
names(aus_bom_temp_min_mean)[5] <- "MMNT"
names(quandl_agro_index_wb)[2] <- "AgroIndex"
names(quandl_copper_imf)[2] <- "Copper"
names(quandl_corn_cme)[2] <- "Corn"
names(quandl_gold_lbma)[2] <- "Gold.USD"
names(quandl_nickel_imf)[2] <- "Nickel"
names(quandl_palm_oil_imf)[2] <- "Palm.Oil"
names(quandl_rice_cme)[2] <- "Rice"
names(quandl_soybean_cme)[2] <- "Soybean"
names(quandl_soybean_oil_cme)[2] <- "Soybean.Oil"
names(quandl_wheat_cme)[2] <- "Wheat"
names(quandl_zinc_imf)[2] <- "Zinc"

# remove unnecessary columns
noaa_usa <- subset(noaa_usa, select = c(DATE,MMXT,MMNT,TPCP))
aus_bom_rain_mean <- subset(aus_bom_rain_mean, select = c(Year, Month, TPCP))
aus_bom_temp_max_mean <- subset(aus_bom_temp_max_mean, select = c(Year, Month, MMXT))
aus_bom_temp_min_mean <- subset(aus_bom_temp_min_mean, select = c(Year, Month, MMNT))
quandl_corn_cme <- subset(quandl_corn_cme, select = c(Date, Corn))
quandl_gold_lbma <- subset(quandl_gold_lbma, select = c(Date, Gold.USD))
quandl_rice_cme <- subset(quandl_rice_cme, select = c(Date, Rice))
quandl_soybean_cme <- subset(quandl_soybean_cme, select = c(Date, Soybean))
quandl_soybean_oil_cme <- subset(quandl_soybean_oil_cme, select = c(Date, Soybean.Oil))
quandl_wheat_cme <- subset(quandl_wheat_cme, select = c(Date, Wheat))

# subset data between the years 1950 - 2015
noaa_enso <- subset(noaa_enso, Year < 2016)
bom_soi <- subset(bom_soi, year < 2016)
bom_soi <- subset(bom_soi, year > 1949)
aus_bom_rain_mean <- subset(aus_bom_rain_mean, Year > 1949)
aus_bom_temp_max_mean <- subset(aus_bom_temp_max_mean, Year > 1949)
aus_bom_temp_min_mean <- subset(aus_bom_temp_min_mean, Year > 1949)

# clean incorrect data
noaa_usa$TPCP[noaa_usa$TPCP == '-9999'] <- 0
noaa_usa$MMNT[noaa_usa$MMNT == '-9999'] <- 0
noaa_usa$MMXT[noaa_usa$MMXT == '-9999'] <- 0

# tidy data by gathering row headings into column variable
noaa_enso <- noaa_enso %>% gather("months", "enso", -Year)

# format date columns
noaa_usa$DATE <- as.Date(as.character(noaa_usa$DATE), "%Y%m%d")
noaa_usa <- separate(noaa_usa,DATE,c("YEAR","MONTH","DAY"), sep = "-")
noaa_usa <- subset(noaa_usa, select = -DAY)
quandl_agro_index_wb <- separate(quandl_agro_index_wb,Date,c("Year","Month","Day"), sep = "-")
quandl_copper_imf <- separate(quandl_copper_imf,Date,c("Year","Month","Day"), sep = "-")
quandl_corn_cme <- separate(quandl_corn_cme,Date,c("Year","Month","Day"), sep = "-")
quandl_gold_lbma <- separate(quandl_gold_lbma,Date,c("Year","Month","Day"), sep = "-")
quandl_nickel_imf <- separate(quandl_nickel_imf,Date,c("Year","Month","Day"), sep = "-")
quandl_palm_oil_imf <- separate(quandl_palm_oil_imf,Date,c("Year","Month","Day"), sep = "-")
quandl_rice_cme <- separate(quandl_rice_cme,Date,c("Year","Month","Day"), sep = "-")
quandl_soybean_cme <- separate(quandl_soybean_cme,Date,c("Year","Month","Day"), sep = "-")
quandl_soybean_oil_cme <- separate(quandl_soybean_oil_cme,Date,c("Year","Month","Day"), sep = "-")
quandl_wheat_cme <- separate(quandl_wheat_cme,Date,c("Year","Month","Day"), sep = "-")
quandl_zinc_imf <- separate(quandl_zinc_imf,Date,c("Year","Month","Day"), sep = "-")

# create factors for the 3 month running average
# for months
noaa_enso$months <- factor(noaa_enso$months, levels = c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ"))
bom_soi$month <- factor(bom_soi$month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
noaa_usa$MONTH <- factor(noaa_usa$MONTH, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
aus_bom_rain_mean$Month <- factor(aus_bom_rain_mean$Month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
aus_bom_temp_max_mean$Month <- factor(aus_bom_temp_max_mean$Month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
aus_bom_temp_min_mean$Month <- factor(aus_bom_temp_min_mean$Month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
quandl_agro_index_wb$Month <- factor(quandl_agro_index_wb$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_copper_imf$Month <-factor(quandl_copper_imf$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_corn_cme$Month <-factor(quandl_corn_cme$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_gold_lbma$Month <-factor(quandl_gold_lbma$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_nickel_imf$Month <-factor(quandl_nickel_imf$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_palm_oil_imf$Month <-factor(quandl_palm_oil_imf$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_rice_cme$Month <-factor(quandl_rice_cme$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_soybean_cme$Month <-factor(quandl_soybean_cme$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_soybean_oil_cme$Month <-factor(quandl_soybean_oil_cme$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_wheat_cme$Month <-factor(quandl_wheat_cme$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
quandl_zinc_imf$Month <-factor(quandl_zinc_imf$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))

# for years
noaa_enso$Year <- factor(noaa_enso$Year)
bom_soi$year <- factor(bom_soi$year)
noaa_usa$YEAR <- factor(noaa_usa$YEAR)
aus_bom_rain_mean$Year <- factor(aus_bom_rain_mean$Year)
aus_bom_temp_max_mean$Year <- factor(aus_bom_temp_max_mean$Year)
aus_bom_temp_min_mean$Year <- factor(aus_bom_temp_min_mean$Year)
quandl_agro_index_wb$Year <- factor(quandl_agro_index_wb$Year)
quandl_copper_imf$Year <- factor(quandl_copper_imf$Year)
quandl_corn_cme$Year <- factor(quandl_corn_cme$Year)
quandl_gold_lbma$Year <- factor(quandl_gold_lbma$Year)
quandl_nickel_imf$Year <- factor(quandl_nickel_imf$Year)
quandl_palm_oil_imf$Year <- factor(quandl_palm_oil_imf$Year)
quandl_rice_cme$Year <- factor(quandl_rice_cme$Year)
quandl_soybean_cme$Year <- factor(quandl_soybean_cme$Year)
quandl_soybean_oil_cme$Year <- factor(quandl_soybean_oil_cme$Year)
quandl_wheat_cme$Year <- factor(quandl_wheat_cme$Year)
quandl_zinc_imf$Year <- factor(quandl_zinc_imf$Year)

# column renames for noaa_usa (for joins)
names(noaa_usa)[1] <- "Year"
names(noaa_usa)[2] <- "Month"

# create a combined dataframe for the Australian BOM dataset
bom_aus <- inner_join(aus_bom_temp_max_mean, aus_bom_temp_min_mean)
bom_aus <- inner_join(bom_aus,aus_bom_rain_mean)

# create a combined dataframe for all commodity values in time series
quandl_commodities <- full_join(quandl_copper_imf, quandl_corn_cme)
quandl_commodities <- full_join(quandl_commodities, quandl_wheat_cme)
quandl_commodities <- full_join(quandl_commodities, quandl_rice_cme)
quandl_commodities <- full_join(quandl_commodities, quandl_soybean_cme)
quandl_commodities <- full_join(quandl_commodities, quandl_soybean_oil_cme)
quandl_commodities <- full_join(quandl_commodities, quandl_palm_oil_imf)
quandl_commodities <- full_join(quandl_commodities, quandl_nickel_imf)
quandl_commodities <- full_join(quandl_commodities, quandl_zinc_imf)
quandl_commodities <- full_join(quandl_commodities, quandl_gold_lbma)
quandl_commodities <- full_join(quandl_commodities, quandl_agro_index_wb)

# for data subset 1986 - 2015
# quandl_commodities <- inner_join(quandl_corn_cme, quandl_rice_cme)
# quandl_commodities <- inner_join(quandl_commodities, quandl_wheat_cme)
# quandl_commodities <- inner_join(quandl_commodities, quandl_soybean_cme)
# quandl_commodities <- inner_join(quandl_commodities, quandl_soybean_oil_cme)
# quandl_commodities <- inner_join(quandl_commodities, quandl_palm_oil_imf)
# quandl_commodities <- inner_join(quandl_commodities, quandl_copper_imf)
# quandl_commodities <- inner_join(quandl_commodities, quandl_nickel_imf)
# quandl_commodities <- inner_join(quandl_commodities, quandl_zinc_imf)
# quandl_commodities <- inner_join(quandl_commodities, quandl_gold_lbma)
# quandl_commodities <- inner_join(quandl_commodities, quandl_agro_index_wb)

# create mean summaries for the commodities by year and month
sum(is.na(quandl_commodities))
by_month <- group_by(quandl_commodities, Year, Month)
quandl_commodities <- summarise(by_month, Corn = mean(Corn, na.rm = TRUE), 
          Rice = mean(Rice, na.rm = TRUE),
          Wheat = mean(Wheat, na.rm = TRUE),
          Soybean = mean(Soybean, na.rm = TRUE),
          Soybean.Oil = mean(Soybean.Oil, na.rm = TRUE),
          Palm.Oil = mean(Palm.Oil, na.rm = TRUE),
          Copper = mean(Copper, na.rm = TRUE),
          Nickel = mean(Nickel, na.rm = TRUE),
          Zinc = mean(Zinc, na.rm = TRUE),
          Gold.USD = mean(Gold.USD, na.rm = TRUE),
          AgroIndex = mean(AgroIndex, na.rm = TRUE))
quandl_commodities$Year <- factor(quandl_commodities$Year)

# create mean summaries for the temp and rain data from NOAA by year and month
# by_month <- group_by(noaa_usa, Year, Month)
# noaa_usa <- summarise(by_month, MMXT = mean(MMXT, na.rm = TRUE), 
#                MMNT = mean(MMNT, na.rm = TRUE),
#                TPCP = mean(TPCP, na.rm = TRUE))
# 
# # create mean summaries for the temp and rain data from BOM by year and month
# by_month <- group_by(bom_aus, Year, Month)
# bom_aus <- summarise(by_month, MMXT = mean(MMXT, na.rm = TRUE), 
#                       MMNT = mean(MMNT, na.rm = TRUE),
#                       TPCP = mean(TPCP, na.rm = TRUE))


# standardize column names across dataframes for joins and plots
names(noaa_enso)[1] <- "year"
names(noaa_enso)[2] <- "monthavg"
names(noaa_usa)[1] <- "year"
names(noaa_usa)[2] <- "month"
names(bom_aus)[1] <- "year"
names(bom_aus)[2] <- "month"
names(quandl_commodities)[1] <- "year"
names(quandl_commodities)[2] <- "month"

# print summaries of key dataframes to be used for analysis
sum(is.na(noaa_enso))
sum(is.na(bom_soi))
sum(is.na(noaa_usa))
sum(is.na(bom_aus))
sum(is.na(quandl_commodities))

str(noaa_enso)          # tidy enso dataset from enso
str(bom_soi)            # tidy soi dataset from bom
str(noaa_usa)           # tidy weather (rain and temp) data for melbourne, Melbourne from noaa
str(bom_aus)            # tidy weather (rain and temp) data for melbourne, victoria from bom
str(quandl_commodities) # tidy summary of all commodities from quandl (imf, cme, world bank, lbma)


# --------------------------------------------
# Analysis
# --------------------------------------------
# Plots:
# 1. NOAA Oceanic Nino Index Historical trend
# 2. BOM Southern Oscillation Index Historical trend
# 3. ENSO Impact on Weather in Melbourne, USA vs Melbourne, Australia
# 4. ENSO Impact on Commodity Pricing


# 1. NOAA Oceanic Nino Index Historical trend
# prep plot data for NOAA Oceanic Nino Index Historical trend
noaa_enso

# create date column for time series plots
noaa_enso$date[noaa_enso$monthavg == "DJF"] <- paste(noaa_enso$year,"01","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "JFM"] <- paste(noaa_enso$year,"02","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "FMA"] <- paste(noaa_enso$year,"03","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "MAM"] <- paste(noaa_enso$year,"04","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "AMJ"] <- paste(noaa_enso$year,"05","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "MJJ"] <- paste(noaa_enso$year,"06","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "JJA"] <- paste(noaa_enso$year,"07","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "JAS"] <- paste(noaa_enso$year,"08","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "ASO"] <- paste(noaa_enso$year,"09","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "SON"] <- paste(noaa_enso$year,"10","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "OND"] <- paste(noaa_enso$year,"11","01", sep = "/")
noaa_enso$date[noaa_enso$monthavg == "NDJ"] <- paste(noaa_enso$year,"12","01", sep = "/")
noaa_enso$date <- as.Date(noaa_enso$date)
noaa_enso <- noaa_enso %>%
  arrange(date)

# El Nino / El Nina events are defined as 5 consecutive overlapping 3-month periods at or above the +0.5 anomaly 
# for warm (El Nino) events and at or below the -0.5 anomaly for cold (La Nina) events.  
# The threshold is further broken down into Strong (1.5 to 1.9) and Very Strong (>= 2.0) events. 
# For an event to be categorized as strong or very strong it must have equaled or exceeded the 
# threshold for at least 3 consecutive overlapping 3-month periods. 
# The ENSO events are (via NOAA analysis):
# Strong El Nino: 1957-58, 1965-66, 1972-73
# Very Strong El Nino: 1982-83, 1997-98, 2015-16
# Strong El Nina: 1973-74, 1975-76, 1988-89

# plot for NOAA Oceanic Nino Index Historical trend
ggplot(data = noaa_enso, aes(x = date, y = enso)) + 
  # annotations for strong El Nino events
  annotate("text", x = as.Date("1957-09-01"), y = 1.8, label = "Strong El Nino") + 
  annotate("rect", xmin = as.Date("1957-09-01"), xmax = as.Date("1958-06-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1965-06-01"), xmax = as.Date("1966-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1972-06-01"), xmax = as.Date("1973-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  # annotations for very strong El Nino events
  annotate("text", x = as.Date("1982-08-01"), y = 2.3, label = "Very Strong El Nino") + 
  annotate("rect", xmin = as.Date("1982-08-01"), xmax = as.Date("1983-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1997-08-01"), xmax = as.Date("1998-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("2015-07-01"), xmax = as.Date("2016-03-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  # annotations for strong El Nina events
  annotate("text", x = as.Date("1973-07-01"), y = -2, label = "Strong El Nina") + 
  annotate("rect", xmin = as.Date("1973-07-01"), xmax = as.Date("1974-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1975-07-01"), xmax = as.Date("1976-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1988-07-01"), xmax = as.Date("1989-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) +   
  geom_line() +
  scale_y_continuous (limits = c(-2.5, 2.5), breaks = seq(-2.5,2.5,0.5)) + 
  ggtitle("Historical NOAA Oceanic Nino Index (ONI)") + 
  xlab("Year") +
  ylab ("3-Month ONI Average")


# 2. BOM Southern Oscillation Index Historical trend
# plots for BOM Southern Oscillation Index Historical trend
bom_soi

# create date column for time series plots
bom_soi$date <- paste(bom_soi$year,bom_soi$month,"01", sep = "/")
bom_soi$date <- as.Date(bom_soi$date)
bom_soi <- bom_soi %>%
  arrange(date)

# calculate moving average
bom_soi$mean <- movingAvg(bom_soi$soi,5)

# plot for BOM Southern Oscillation Index Historical trend
ggplot(data = bom_soi, aes(x = date, y = soi)) + 
  geom_line(color = 'grey') +
  geom_line(aes(x = date, y = mean)) +
  # annotations for strong El Nino events
  annotate("text", x = as.Date("1957-09-01"), y = -10, label = "Strong El Nino") + 
  annotate("rect", xmin = as.Date("1957-09-01"), xmax = as.Date("1958-06-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1965-06-01"), xmax = as.Date("1966-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1972-06-01"), xmax = as.Date("1973-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  # annotations for very strong El Nino events
  annotate("text", x = as.Date("1982-08-01"), y = -30, label = "Very Strong El Nino") + 
  annotate("rect", xmin = as.Date("1982-08-01"), xmax = as.Date("1983-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1997-08-01"), xmax = as.Date("1998-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("2015-07-01"), xmax = as.Date("2016-03-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  # annotations for strong El Nina events
  annotate("text", x = as.Date("1973-07-01"), y = 23, label = "Strong El Nina") + 
  annotate("rect", xmin = as.Date("1973-07-01"), xmax = as.Date("1974-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1975-07-01"), xmax = as.Date("1976-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1988-07-01"), xmax = as.Date("1989-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) +
  scale_y_continuous (limits = c(-35, 35), breaks = seq(-35,35,10)) + 
  ggtitle("Historical BOM Southern Oscillation Index (SOI)") + 
  xlab("Year") +
  ylab ("Monthly Average SOI")

# exploration relationship between OCI and SOI data
combined_enso <- inner_join(noaa_enso, bom_soi)
combined_enso <- subset(combined_enso, select = c(date, soi, enso, mean))

# plot to view OCI to SOI relationship
ggplot(data = combined_enso, aes(x = mean, y = enso)) + 
  geom_line(color = 'grey') +
  geom_smooth(span = 0.15) +
  ggtitle("OCI vs Mean SOI Relationship for ENSO Events") + 
  xlab("Mean SOI") +
  ylab ("OCI")
  
# create a linear regression model
lin_reg <- lm(enso ~ mean, data = combined_enso)
summary(lin_reg)

# find correlation between SOI and ENSO
combined_enso_num <- combined_enso[c("enso", "soi", "mean")]
# convert column vectors to numeric for correlation function
combined_enso_num$soi <- as.numeric(combined_enso_num$soi)
combined_enso_num$enso <- as.numeric(combined_enso_num$enso)
combined_enso_num$mean <- as.numeric(combined_enso_num$mean)
# filter out NAs
combined_enso_num <- combined_enso_num %>%
  filter(!is.na(mean))
# create correlation table 
round(cor(combined_enso_num), 2) # corr = -0.74 / -0.86 (soi / mean soi)

# 3a. ENSO Impact on Weather in Melbourne, USA
# plots for ENSO Impact on Weather Across Pacific
noaa_usa

# create date column for time series plots
noaa_usa$date <- paste(noaa_usa$year,noaa_usa$month,"01", sep = "/")
noaa_usa$date <- as.Date(noaa_usa$date)
noaa_usa <- noaa_usa %>%
  arrange(date)

# create columns for ENSO events
noaa_usa$event <- "No Event"
# for all strong El Nino events
noaa_usa$event[which(noaa_usa$date >= "1957-04-01" & noaa_usa$date <= "1958-06-01")] <- "Strong El Nino"
noaa_usa$event[which(noaa_usa$date >= "1965-06-01" & noaa_usa$date <= "1966-04-01")] <- "Strong El Nino"
noaa_usa$event[which(noaa_usa$date >= "1972-05-01" & noaa_usa$date <= "1973-03-01")] <- "Strong El Nino"
# for all very strong El Nino events
noaa_usa$event[which(noaa_usa$date >= "1982-04-01" & noaa_usa$date <= "1983-06-01")] <- "Very Strong El Nino"
noaa_usa$event[which(noaa_usa$date >= "1997-05-01" & noaa_usa$date <= "1998-05-01")] <- "Very Strong El Nino"
noaa_usa$event[which(noaa_usa$date >= "2015-03-01" & noaa_usa$date <= "2016-03-01")] <- "Very Strong El Nino"
# for all strong El Nina events
noaa_usa$event[which(noaa_usa$date >= "1973-06-01" & noaa_usa$date <= "1974-07-01")] <- "Strong El Nina"
noaa_usa$event[which(noaa_usa$date >= "1975-03-01" & noaa_usa$date <= "1976-03-01")] <- "Strong El Nina"
noaa_usa$event[which(noaa_usa$date >= "1988-05-01" & noaa_usa$date <= "1989-05-01")] <- "Strong El Nina"

# create column for MMXT standard deviations divisions
# view distribution of MMXT
ggplot(noaa_usa,aes(MMXT)) + 
  geom_histogram(binwidth = 0.1) 
skewness(noaa_usa$MMXT) #-0.29
# label standard deviation distribution for MMXT
mean <- mean(noaa_usa$MMXT)
stdev <- sd(noaa_usa$MMXT)
noaa_usa$MMXT.stdev <- 3
noaa_usa$MMXT.stdev[which(noaa_usa$MMXT <= (mean + stdev) & noaa_usa$MMXT >= (mean - stdev))] <- 1
noaa_usa$MMXT.stdev[which(noaa_usa$MMXT.stdev != 1 & noaa_usa$MMXT <= (mean + 2*stdev) & noaa_usa$MMXT >= (mean - 2*stdev))] <- 2
table(noaa_usa$MMXT.stdev)
noaa_usa$MMXT.stdev[noaa_usa$MMXT.stdev == 1] <- "Within 1 Std. Dev"
noaa_usa$MMXT.stdev[noaa_usa$MMXT.stdev == 2] <- "Within 2 Std. Dev"
noaa_usa$MMXT.stdev[noaa_usa$MMXT.stdev == 3] <- "Over 2 Std. Dev"

# create column for MMXT quantile divisions
noaa_usa$MMXT.quantile <- cut(noaa_usa$MMXT, breaks = quantile(noaa_usa$MMXT),
                            labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)
table(noaa_usa$MMXT.quantile)

# create column for MMNT standard deviations divisions
# view distribution of MMNT
ggplot(noaa_usa,aes(MMNT)) + 
  geom_histogram(binwidth = 0.3) 
skewness(noaa_usa$MMNT) #-12.3
# label standard deviation distribution for MMNT
mean <- mean(noaa_usa$MMNT)
stdev <- sd(noaa_usa$MMNT)
noaa_usa$MMNT.stdev <- 3
noaa_usa$MMNT.stdev[which(noaa_usa$MMNT <= (mean + stdev) & noaa_usa$MMNT >= (mean - stdev))] <- 1
noaa_usa$MMNT.stdev[which(noaa_usa$MMNT.stdev != 1 & noaa_usa$MMNT <= (mean + 2*stdev) & noaa_usa$MMNT >= (mean - 2*stdev))] <- 2
table(noaa_usa$MMNT.stdev)
noaa_usa$MMNT.stdev[noaa_usa$MMNT.stdev == 1] <- "Within 1 Std. Dev"
noaa_usa$MMNT.stdev[noaa_usa$MMNT.stdev == 2] <- "Within 2 Std. Dev"
noaa_usa$MMNT.stdev[noaa_usa$MMNT.stdev == 3] <- "Over 2 Std. Dev"

# create column for TPCP standard deviations divisions
# view distribution of TPCP
ggplot(noaa_usa,aes(TPCP)) + 
  geom_histogram(binwidth = 0.5) 
# exploring skewness
skewness(noaa_usa$TPCP) #1.43
skewness(subset(noaa_usa, TPCP >0)$TPCP)
# label standard deviation distribution for TPCP
mean <- mean(noaa_usa$TPCP)
stdev <- sd(noaa_usa$TPCP)
noaa_usa$TPCP.stdev <- 3
noaa_usa$TPCP.stdev[which(noaa_usa$TPCP <= (mean + stdev) & noaa_usa$TPCP >= (mean - stdev))] <- 1
noaa_usa$TPCP.stdev[which(noaa_usa$TPCP.stdev != 1 & noaa_usa$TPCP <= (mean + 2*stdev) & noaa_usa$TPCP >= (mean - 2*stdev))] <- 2
table(noaa_usa$TPCP.stdev)
noaa_usa$TPCP.stdev[noaa_usa$TPCP.stdev == 1] <- "Within 1 Std. Dev"
noaa_usa$TPCP.stdev[noaa_usa$TPCP.stdev == 2] <- "Within 2 Std. Dev"
noaa_usa$TPCP.stdev[noaa_usa$TPCP.stdev == 3] <- "Over 2 Std. Dev"

# create column for TPCP quantile divisions
noaa_usa$TPCP.quantile <- cut(noaa_usa$TPCP, breaks = quantile(noaa_usa$TPCP),
                              labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)
table(noaa_usa$TPCP.quantile)

# label whether it rained or not
quantile(noaa_usa$TPCP)
nrow(subset(noaa_usa, TPCP >= 0 & TPCP <= 2)) # consder rain less than 2mm for the day to be a dry day
noaa_usa$TPCP.rained <- ifelse(noaa_usa$TPCP > 2, "Yes", "No")
table(noaa_usa$TPCP.rained)

# plot for ENSO Impact on rain across temperature deviations in Melbourne, USA
ggplot(data = noaa_usa, aes(event, TPCP)) + 
  geom_boxplot() + 
  facet_grid( ~ MMXT.stdev) + 
  ggtitle("Rain Distribution Across Temperature Deviations in Melbourne, USA") + 
  xlab("ENSO Events") +
  ylab ("Monthly Precipitation (inches)")

# verify graph numerically
tapply(noaa_usa$TPCP, noaa_usa$event, summary)

# 3b. ENSO Impact on Weather in Melbourne, Australia
# plots for ENSO Impact on Weather Across Pacific
bom_aus

# create date column for time series plots
bom_aus$date <- paste(bom_aus$year,bom_aus$month,"01", sep = "/")
bom_aus$date <- as.Date(bom_aus$date)
bom_aus <- bom_aus %>%
  arrange(date)

# create columns for ENSO events
bom_aus$event <- "No Event"
# for all strong El Nino events
bom_aus$event[which(bom_aus$date >= "1957-04-01" & bom_aus$date <= "1958-06-01")] <- "Strong El Nino"
bom_aus$event[which(bom_aus$date >= "1965-06-01" & bom_aus$date <= "1966-04-01")] <- "Strong El Nino"
bom_aus$event[which(bom_aus$date >= "1972-05-01" & bom_aus$date <= "1973-03-01")] <- "Strong El Nino"
# for all very strong El Nino events
bom_aus$event[which(bom_aus$date >= "1982-04-01" & bom_aus$date <= "1983-06-01")] <- "Very Strong El Nino"
bom_aus$event[which(bom_aus$date >= "1997-05-01" & bom_aus$date <= "1998-05-01")] <- "Very Strong El Nino"
bom_aus$event[which(bom_aus$date >= "2015-03-01" & bom_aus$date <= "2016-03-01")] <- "Very Strong El Nino"
# for all strong El Nina events
bom_aus$event[which(bom_aus$date >= "1973-06-01" & bom_aus$date <= "1974-07-01")] <- "Strong El Nina"
bom_aus$event[which(bom_aus$date >= "1975-03-01" & bom_aus$date <= "1976-03-01")] <- "Strong El Nina"
bom_aus$event[which(bom_aus$date >= "1988-05-01" & bom_aus$date <= "1989-05-01")] <- "Strong El Nina"

# create column for MMXT standard deviations divisions
# view distribution of MMXT
ggplot(bom_aus,aes(MMXT)) + 
  geom_histogram(binwidth = 0.3) 
skewness(bom_aus$MMXT) #0.12
# label standard deviation distribution for MMXT
mean <- mean(bom_aus$MMXT)
stdev <- sd(bom_aus$MMXT)
bom_aus$MMXT.stdev <- 3
bom_aus$MMXT.stdev[which(bom_aus$MMXT <= (mean + stdev) & bom_aus$MMXT >= (mean - stdev))] <- 1
bom_aus$MMXT.stdev[which(bom_aus$MMXT.stdev != 1 & bom_aus$MMXT <= (mean + 2*stdev) & bom_aus$MMXT >= (mean - 2*stdev))] <- 2
table(bom_aus$MMXT.stdev)
bom_aus$MMXT.stdev[bom_aus$MMXT.stdev == 1] <- "Within 1 Std. Dev"
bom_aus$MMXT.stdev[bom_aus$MMXT.stdev == 2] <- "Within 2 Std. Dev"
bom_aus$MMXT.stdev[bom_aus$MMXT.stdev == 3] <- "Over 2 Std. Dev"

# create column for MMXT quantile divisions
bom_aus$MMXT.quantile <- cut(bom_aus$MMXT, breaks = quantile(bom_aus$MMXT),
                              labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)
table(bom_aus$MMXT.quantile)

# create column for MMNT standard deviations divisions
# view distribution of MMNT
ggplot(bom_aus,aes(MMNT)) + 
  geom_histogram(binwidth = 0.3) 
skewness(bom_aus$MMNT) #0.18
# label standard deviation distribution for MMNT
mean <- mean(bom_aus$MMNT)
stdev <- sd(bom_aus$MMNT)
bom_aus$MMNT.stdev <- 3
bom_aus$MMNT.stdev[which(bom_aus$MMNT <= (mean + stdev) & bom_aus$MMNT >= (mean - stdev))] <- 1
bom_aus$MMNT.stdev[which(bom_aus$MMNT.stdev != 1 & bom_aus$MMNT <= (mean + 2*stdev) & bom_aus$MMNT >= (mean - 2*stdev))] <- 2
table(bom_aus$MMNT.stdev)
bom_aus$MMNT.stdev[bom_aus$MMNT.stdev == 1] <- "Within 1 Std. Dev"
bom_aus$MMNT.stdev[bom_aus$MMNT.stdev == 2] <- "Within 2 Std. Dev"
bom_aus$MMNT.stdev[bom_aus$MMNT.stdev == 3] <- "Over 2 Std. Dev"

# create column for MMNT quantile divisions
bom_aus$MMNT.quantile <- cut(bom_aus$MMNT, breaks = quantile(bom_aus$MMNT),
                             labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)
table(bom_aus$MMNT.quantile)

# create column for TPCP standard deviations divisions
# view distribution of TPCP
ggplot(bom_aus,aes(TPCP)) + 
  geom_histogram(binwidth = 3) 
skewness(bom_aus$TPCP) #1.29
# label standard deviation distribution for TPCP
mean <- mean(bom_aus$TPCP)
stdev <- sd(bom_aus$TPCP)
bom_aus$TPCP.stdev <- 3
bom_aus$TPCP.stdev[which(bom_aus$TPCP <= (mean + stdev) & bom_aus$TPCP >= (mean - stdev))] <- 1
bom_aus$TPCP.stdev[which(bom_aus$TPCP.stdev != 1 & bom_aus$TPCP <= (mean + 2*stdev) & bom_aus$TPCP >= (mean - 2*stdev))] <- 2
table(bom_aus$TPCP.stdev)
bom_aus$TPCP.stdev[bom_aus$TPCP.stdev == 1] <- "Within 1 Std. Dev"
bom_aus$TPCP.stdev[bom_aus$TPCP.stdev == 2] <- "Within 2 Std. Dev"
bom_aus$TPCP.stdev[bom_aus$TPCP.stdev == 3] <- "Over 2 Std. Dev"

# create column for TPCP quantile divisions
bom_aus$TPCP.quantile <- cut(bom_aus$TPCP, breaks = quantile(bom_aus$TPCP),
                              labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)
table(bom_aus$TPCP.quantile)

# label whether it rained or not
quantile(bom_aus$TPCP)
nrow(subset(bom_aus, TPCP >= 0 & TPCP <= 2)) # consder rain less than 2mm for the day to be a dry day
bom_aus$TPCP.rained <- ifelse(bom_aus$TPCP > 2, "Yes", "No")
table(bom_aus$TPCP.rained)

# plot for ENSO Impact on rain across temperature deviations in Melbourne, Australia
ggplot(data = bom_aus, aes(event, TPCP)) + 
  geom_boxplot() + 
  facet_grid( ~ MMXT.stdev) + 
  ggtitle("Rain Distribution Across Temperature Deviations in Melbourne, Australia") + 
  xlab("ENSO Events") +
  ylab ("Monthly Precipitation (mm)")


# 4. ENSO Impact on Commodity Pricing
# plots for ENSO Impact on Commodity Pricing
quandl_commodities

# create date column for time series plots
quandl_commodities$date <- paste(quandl_commodities$year,quandl_commodities$month,"01", sep = "/")
quandl_commodities$date <- as.Date(quandl_commodities$date)
quandl_commodities <- quandl_commodities %>%
  arrange(date)

# combine all commodity columns for plot
commodities <- subset(quandl_commodities, select = -c(year, month))
commodities <- commodities %>% gather("Commodity", "Price", -date)

# plot for ENSO Impact on historical Commodity Pricing
ggplot(data = commodities, aes(x = date, y = log(Price))) + 
  geom_line(aes(color = Commodity)) + 
  # annotations for strong El Nino events
  annotate("text", x = as.Date("1965-06-01"), y = 10, label = "Strong El Nino") + 
  annotate("rect", xmin = as.Date("1965-06-01"), xmax = as.Date("1966-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1972-06-01"), xmax = as.Date("1973-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  # annotations for very strong El Nino events
  annotate("text", x = as.Date("1982-08-01"), y = 10, label = "Very Strong El Nino") + 
  annotate("rect", xmin = as.Date("1982-08-01"), xmax = as.Date("1983-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1997-08-01"), xmax = as.Date("1998-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("2015-07-01"), xmax = as.Date("2016-03-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  # annotations for strong El Nina events
  annotate("text", x = as.Date("1973-07-01"), y = 10, label = "Strong El Nina") + 
  annotate("rect", xmin = as.Date("1973-07-01"), xmax = as.Date("1974-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1975-07-01"), xmax = as.Date("1976-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1988-07-01"), xmax = as.Date("1989-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) +
  ggtitle("Historical Commodities Pricing During ENSO Events") + 
  xlab("Year") +
  ylab ("Log(Price)")

# filter for just a commodities relevant in USA and Australia
commodities <- commodities %>%
  filter(Commodity %in% c("Soybean", "Soybean.Oil", "Corn", "Wheat", "Palm.Oil"))

# plot for ENSO Impact on historical Commodity Pricing
ggplot(data = commodities, aes(x = date, y = log(Price))) + 
  geom_line(aes(color = Commodity)) + 
  # annotations for strong El Nino events
  annotate("text", x = as.Date("1965-06-01"), y = 10, label = "Strong El Nino") + 
  annotate("rect", xmin = as.Date("1965-06-01"), xmax = as.Date("1966-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1972-06-01"), xmax = as.Date("1973-03-01"), ymin = -Inf, ymax = +Inf, fill='#4cb9ee', alpha = 0.2) + 
  # annotations for very strong El Nino events
  annotate("text", x = as.Date("1982-08-01"), y = 10, label = "Very Strong El Nino") + 
  annotate("rect", xmin = as.Date("1982-08-01"), xmax = as.Date("1983-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1997-08-01"), xmax = as.Date("1998-05-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("2015-07-01"), xmax = as.Date("2016-03-01"), ymin = -Inf, ymax = +Inf, fill='#0055b4', alpha = 0.2) + 
  # annotations for strong El Nina events
  annotate("text", x = as.Date("1973-07-01"), y = 10, label = "Strong El Nina") + 
  annotate("rect", xmin = as.Date("1973-07-01"), xmax = as.Date("1974-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1975-07-01"), xmax = as.Date("1976-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) + 
  annotate("rect", xmin = as.Date("1988-07-01"), xmax = as.Date("1989-04-01"), ymin = -Inf, ymax = +Inf, fill='#f65b5b', alpha = 0.2) +
  ggtitle("Historical Pricing During ENSO Events for USA, Australia Commodities") + 
  xlab("Year") +
  ylab ("Log(Price)")


# --------------------------------------------
# Hypothesis Testing (ANOVA)
# --------------------------------------------
# Question: Is there a strong relationship between commodity pricing and ENSO events?
# To do:
# - control for commodity normal during events
# - see if you local data in australia / usa available for commodities
#     if not, then pick commodities whether either country is majority exporter (supply price)
# - figure out appropriate method (research econ papers on interest rate influence on security pricing)

