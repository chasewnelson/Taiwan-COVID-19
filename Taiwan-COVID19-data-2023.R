# ------------------------------------------------------------------------------
# Approximation of Taiwan's cumulative COVID-19 deaths and merging of JHU, WHO/OWID, TWCDC data
# ------------------------------------------------------------------------------

# Author: Chase W. Nelson, cnelson AT amnh DOT org
# Date created: 2023-10-21
# Last date modified: 2023-11-01

# Import libraries
library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggrepel)
library(ggtext)
library(extrafont)
font_import()
# sort(fonts())


# ------------------------------------------------------------------------------
# CHANGE THIS TO THE REPOSITORY DIRECTORY ON YOUR MACHINE
setwd("/github_Taiwan-COVID-19/")  # <== PATH TO THIS REPOSITORY


# ------------------------------------------------------------------------------
# INPUT DEATH DATA from Johns Hopkins University (JHU)

# JHU historical data
# source/deaths: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv 
JHU_deaths <- read_csv("time_series_covid19_deaths_global.csv")
# JHU_deaths

# source/cases: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv
JHU_cases <- read_csv("time_series_covid19_confirmed_global.csv")

# examine levels
# sort(unique(JHU_deaths$`Country/Region`))
# sort(unique(JHU_deaths$`Province/State`))

# ----------
# Taiwan

# rename (ridiculous *asterisk)
JHU_deaths[JHU_deaths$`Country/Region` == 'Taiwan*', ]$`Country/Region` <- 'Taiwan'
JHU_cases[JHU_cases$`Country/Region` == 'Taiwan*', ]$`Country/Region` <- 'Taiwan'
# filter(JHU_deaths, `Country/Region` == 'Taiwan')


# ----------
# PIVOT LONG - deaths
JHU_deaths_LONG <- pivot_longer(
   data = JHU_deaths,
   cols = all_of(setdiff(names(JHU_deaths), c('Province/State', 'Country/Region', 'Lat', 'Long'))),
   names_to = 'date',
   values_to = 'total_deaths'
)

# names
names(JHU_deaths_LONG) <- c('region', 'location', 'lat', 'long', 'date', 'total_deaths')

# format date
JHU_deaths_LONG <- JHU_deaths_LONG |>
   mutate(date = mdy(date))
JHU_deaths_LONG

# filter Taiwan
JHU_deaths_LONG_Taiwan <- filter(JHU_deaths_LONG, location == 'Taiwan')
JHU_deaths_LONG_Taiwan  # 1,143 × 6
# view(JHU_deaths_LONG_Taiwan)


# ----------
# PIVOT LONG - cases
JHU_cases_LONG <- pivot_longer(
   data = JHU_cases,
   cols = all_of(setdiff(names(JHU_cases), c('Province/State', 'Country/Region', 'Lat', 'Long'))),
   names_to = 'date',
   values_to = 'total_cases'
   )

# names
names(JHU_cases_LONG) <- c('region', 'location', 'lat', 'long', 'date', 'total_cases')

# format date
JHU_cases_LONG <- JHU_cases_LONG |>
   mutate(date = mdy(date))
JHU_cases_LONG

# filter Taiwan
JHU_cases_LONG_Taiwan <- filter(JHU_cases_LONG, location == 'Taiwan')
JHU_cases_LONG_Taiwan
# 1,143 × 6
# view(JHU_cases_LONG_Taiwan)


# ----------
# JOIN deaths & cases
JHU_data_LONG_Taiwan <- left_join(JHU_deaths_LONG_Taiwan,  # 1,143 × 6
                                  dplyr::select(JHU_cases_LONG_Taiwan, 'date', 'total_cases'),  # 1,143 × 2
                                  by = 'date')
JHU_data_LONG_Taiwan  # 1,143 × 7 QED


# ------------------------------------------------------------------------------
# INPUT DATA from Our World in Data (OWID)

# source: https://covid.ourworldindata.org/data/owid-covid-data.csv
# source2: https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv
# wget https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv

OWID_data <- read_csv("owid-covid-data.csv")
OWID_data
# 350,019 × 67 on 20231021

# NON-TAIWAN ROWS
(non_TW_row_count <- nrow(filter(OWID_data, location != 'Taiwan')))  # 348671 on 20231021

# ----------
# Filter to Taiwan
# view(filter(OWID_data, location == 'Taiwan'))
(OWID_data_Taiwan <- filter(OWID_data, location == 'Taiwan'))
# 1,348 × 67 on 20231021

# this is the table we want to FILL IN with Taiwan's data

# ----------
# INSERT Taiwan's data OWID from JHU
# OWID_data
# OWID_data_Taiwan
# JHU_deaths_LONG_Taiwan

# named deaths vector
JHU_data_LONG_Taiwan_deaths <- JHU_data_LONG_Taiwan$total_deaths
names(JHU_data_LONG_Taiwan_deaths) <- JHU_data_LONG_Taiwan$date

# named cases vector
JHU_data_LONG_Taiwan_cases <- JHU_data_LONG_Taiwan$total_cases
names(JHU_data_LONG_Taiwan_cases) <- JHU_data_LONG_Taiwan$date
# JHU_data_LONG_Taiwan_deaths['2020-12-01']  # 7

# Add deaths and cases to OWID ('fill in') from JHU
OWID_data_Taiwan$total_deaths <- JHU_data_LONG_Taiwan_deaths[as.character(OWID_data_Taiwan$date)]
OWID_data_Taiwan$total_cases <- JHU_data_LONG_Taiwan_cases[as.character(OWID_data_Taiwan$date)]
OWID_data_Taiwan
# 1,348 × 67 QED

# inspect
# view(OWID_data_Taiwan)
# updates through 2023/02/22
# numbers through 2023/03/09


# ------------------------------------------------------------------------------
# INPUT DATA from Taiwan CDC (TWCDC)

# source: manual data entry by CWN from Chinese-language press releases at https://www.cdc.gov.tw/

TWCDC_data <- read_csv("Taiwan-COVID19-data-2023.csv")
TWCDC_data
# 36 × 24 on 20231021

# retain only columns withn exact names/correspondences in the OWID data
col_to_keep <- c(
   "location",
   "date",
   "total_cases",
   "new_cases",
   "total_deaths",
   "new_deaths",
   "total_cases_per_million",
   "new_cases_per_million",
   "total_deaths_per_million",
   "new_deaths_per_million"
)

TWCDC_data_cut <- dplyr::select(TWCDC_data, any_of(col_to_keep))
TWCDC_data_cut  # 36 × 5
# date       total_cases new_cases total_deaths new_deaths
# 1 2023-02-23     9985320     14387        17709         37
# 2 2023-03-02    10069539     84224        18010        301
# 3 2023-03-09    10143788     74263        18371        364
# 4 2023-03-16    10206482     62713        18656        287
# 5 2023-03-23          NA        NA        18894        238
# ...


# ----------
# INSERT Taiwan's data OWID from TWCDC

# get dates used FOR TAIWAN in either OWID or TWCDC
# used_dates_TW <- sort(unique(c(
#    OWID_data_Taiwan$date,
#    TWCDC_data_cut$date)))

# dates used for TW just in OWID
used_dates_TW_OWID <- sort(unique(OWID_data_Taiwan$date))
# tail(used_dates_TW_OWID)  # "2023-09-19" "2023-09-20" "2023-09-21" "2023-09-22" "2023-09-23" "2023-09-24"

# dates used for ANYTHING in OWID
used_dates_OWID_all <- sort(unique(OWID_data$date))
# tail(used_dates_OWID_all)  # "2023-10-15" "2023-10-16" "2023-10-17" "2023-10-18" "2023-10-19" "2023-10-20"

# get dates missing from either OWID or TWCDC for Taiwan, in OWID
(missing_dates <- as.Date(setdiff(
   seq(min(used_dates_OWID_all), max(used_dates_OWID_all), by = "day"),
   used_dates_TW_OWID
)))

# add empty (NA) rows for missing dates to OWID_data_Taiwan
missing_rows <- tibble(date = missing_dates)
for (colname in setdiff(names(OWID_data_Taiwan), 'date')) {
   missing_rows[[colname]] <- NA
}

# order correctly
missing_rows <- dplyr::select(missing_rows, names(OWID_data_Taiwan))

# add rows
OWID_data_Taiwan <- bind_rows(OWID_data_Taiwan, missing_rows)

# sort by date
OWID_data_Taiwan <- arrange(OWID_data_Taiwan, date)

# any dups?
length(OWID_data_Taiwan$date) == length(unique(OWID_data_Taiwan$date))  # TRUE QED
nrow(OWID_data_Taiwan) == nrow(distinct(OWID_data_Taiwan))  # TRUE

# add metadata
OWID_data_Taiwan$iso_code <- "TWN"
OWID_data_Taiwan$continent <- "Asia"
OWID_data_Taiwan$location <- "Taiwan"
tail(OWID_data_Taiwan)


# ----------
# DEATHS 

# REMOVE data after '2023/02/22' (JHU stopped)
OWID_data_Taiwan[OWID_data_Taiwan$date > as.Date('2023-02-22'), ]$total_deaths <- NA
OWID_data_Taiwan[OWID_data_Taiwan$date > as.Date('2023-02-22'), ]$total_cases <- NA
# view(OWID_data_Taiwan)

# ADD deaths and cases for specific dates (2023-02-23, 2023-03-02, 2023-03-09, 2023-03-16) last reported by TWCDC
OWID_data_Taiwan[OWID_data_Taiwan$date >= as.Date('2023-02-23') & OWID_data_Taiwan$date < as.Date('2023-03-02'), ]$total_deaths <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-02-23'), ]$total_deaths
OWID_data_Taiwan[OWID_data_Taiwan$date >= as.Date('2023-02-23') & OWID_data_Taiwan$date < as.Date('2023-03-02'), ]$total_cases <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-02-23'), ]$total_cases

OWID_data_Taiwan[OWID_data_Taiwan$date >= as.Date('2023-03-02') & OWID_data_Taiwan$date < as.Date('2023-03-09'), ]$total_deaths <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-03-02'), ]$total_deaths
OWID_data_Taiwan[OWID_data_Taiwan$date >= as.Date('2023-03-02') & OWID_data_Taiwan$date < as.Date('2023-03-09'), ]$total_cases <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-03-02'), ]$total_cases

OWID_data_Taiwan[OWID_data_Taiwan$date >= as.Date('2023-03-09') & OWID_data_Taiwan$date < as.Date('2023-03-16'), ]$total_deaths <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-03-09'), ]$total_deaths
OWID_data_Taiwan[OWID_data_Taiwan$date >= as.Date('2023-03-09') & OWID_data_Taiwan$date < as.Date('2023-03-16'), ]$total_cases <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-03-09'), ]$total_cases

OWID_data_Taiwan[OWID_data_Taiwan$date == as.Date('2023-03-16'), ]$total_deaths <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-03-16'), ]$total_deaths
OWID_data_Taiwan[OWID_data_Taiwan$date == as.Date('2023-03-16'), ]$total_cases <- 
   TWCDC_data[TWCDC_data$date == as.Date('2023-03-16'), ]$total_cases

# new named DEATH vectors from TWCDC (no more CASE data after 2023/03/16)
TWCDC_data_cut_new_deaths <- TWCDC_data_cut$new_deaths
names(TWCDC_data_cut_new_deaths) <- TWCDC_data_cut$date

# filter to only date range after cumulative totals reported by CDC
TWCDC_data_cut_new_deaths <- TWCDC_data_cut_new_deaths[names(TWCDC_data_cut_new_deaths) > as.Date('2023-03-16')]
# TWCDC_data_cut_new_deaths['2023-03-22']  # NA QED
TWCDC_data_cut_new_deaths['2023-03-23']  # 238 QED
# TWCDC_data_cut_new_deaths['2023-03-24']  # NA QED
# TWCDC_data_cut_new_deaths['2023-09-25']  # 35 QED
# TWCDC_data_cut_new_deaths['2023-09-26']  # NA QED

# ADD new_deaths from the TW CDC data after '2023-03-16'***
OWID_data_Taiwan$new_deaths <- TWCDC_data_cut_new_deaths[as.character(OWID_data_Taiwan$date)]
# tail(OWID_data_Taiwan)
# view(OWID_data_Taiwan)

# fill in *new* DEATHS, works for dates until 2023/03/16
OWID_data_Taiwan <- OWID_data_Taiwan |>
   mutate(new_deaths = ifelse(is.na(new_deaths), total_deaths - lag(total_deaths, default = total_deaths[1]), new_deaths))
(min_date_deaths <- min(OWID_data_Taiwan[! is.na(OWID_data_Taiwan$total_deaths), ]$date))  # '2020-01-22'
OWID_data_Taiwan[OWID_data_Taiwan$date == min_date_deaths, ]$new_deaths <- OWID_data_Taiwan[OWID_data_Taiwan$date == min_date_deaths, ]$total_deaths

# fill in *new* CASES, works for dates until 2023/03/16
OWID_data_Taiwan <- OWID_data_Taiwan |>
   mutate(new_cases = ifelse(is.na(new_cases), total_cases - lag(total_cases, default = total_cases[1]), new_cases))
(min_date_cases <- min(OWID_data_Taiwan[! is.na(OWID_data_Taiwan$total_cases), ]$date))  # '2020-01-22'
OWID_data_Taiwan[OWID_data_Taiwan$date == min_date_cases, ]$new_cases <- OWID_data_Taiwan[OWID_data_Taiwan$date == min_date_cases, ]$total_cases
# view(OWID_data_Taiwan)

# fill in remaining NAs with 0s (nothing reported that day)
OWID_data_Taiwan[is.na(OWID_data_Taiwan$new_deaths) & OWID_data_Taiwan$date >= min_date_deaths, ]$new_deaths <- 0
OWID_data_Taiwan[is.na(OWID_data_Taiwan$new_cases) & OWID_data_Taiwan$date >= min_date_deaths, ]$new_cases <- 0

# fill CASES after 2023-03-16 with NA, because they are no longer meaningfully reported
OWID_data_Taiwan[OWID_data_Taiwan$date > as.Date('2023-03-16'), ]$total_cases <- NA
OWID_data_Taiwan[OWID_data_Taiwan$date > as.Date('2023-03-16'), ]$new_cases <- NA
# view(OWID_data_Taiwan)

# fill total_deaths & population after 2023-03-16
for (this_date in seq(as.Date('2023-03-16'), max(OWID_data_Taiwan$date), by = "day")) {
   # this_date <- as.Date('2023-03-16')
   prev_date <- this_date - 1
   prev_date_total_deaths <- OWID_data_Taiwan[OWID_data_Taiwan$date == prev_date, ]$total_deaths
   
   # total deaths
   OWID_data_Taiwan[OWID_data_Taiwan$date == this_date, ]$total_deaths <- 
      prev_date_total_deaths + 
      OWID_data_Taiwan[OWID_data_Taiwan$date == this_date, ]$new_deaths
   
   # population
   OWID_data_Taiwan[OWID_data_Taiwan$date == this_date, ]$population <- max(OWID_data_Taiwan$population, na.rm = TRUE)
}

(OWID_data_Taiwan <- arrange(OWID_data_Taiwan, date))
view(OWID_data_Taiwan)

# -----
# CASES & DEATHS PER CAPITA
# TOTAL_POPULATION <- 23893396
OWID_data_Taiwan$total_deaths_per_million <- OWID_data_Taiwan$total_deaths / OWID_data_Taiwan$population * 1e6
OWID_data_Taiwan$total_cases_per_million <- OWID_data_Taiwan$total_cases / OWID_data_Taiwan$population * 1e6

# SAVE Taiwan-only OWID filled in
# write_csv(OWID_data_Taiwan, "owid-covid-data-Taiwan-only.csv")


# ------------------------------------------------------------------------------
# RE-INSERT TAIWAN'S DATA INTO OWID
(OWID_data_Taiwan_added <- bind_rows(
   filter(OWID_data, location != 'Taiwan'),
   OWID_data_Taiwan))
# sort(unique(OWID_data$iso_code))
# filter(OWID_data, iso_code == 'ABW')
# 350,060 × 67 QED num cols

# NON-TAIWAN ROWS SHOULD BE non_TW_row_count
nrow(filter(OWID_data_Taiwan_added, location != 'Taiwan')) == non_TW_row_count  # TRUE QED

# Taiwan's max date THE max date?
max(filter(OWID_data_Taiwan_added, location == 'Taiwan')$date)  # "2023-10-20" on 20231021
max(OWID_data_Taiwan_added$date)  # "2023-10-20" QED
max(filter(OWID_data_Taiwan_added, location == 'Taiwan')$date) == max(OWID_data_Taiwan_added$date)  # TRUE QED
tail(filter(OWID_data_Taiwan_added, location == 'Taiwan'))

# sort rows by location, date
OWID_data_Taiwan_added <- arrange(OWID_data_Taiwan_added, location, date)
OWID_data_Taiwan_added
# 350,060 × 67 OK

# SAVE
# write_csv(OWID_data_Taiwan_added, "owid-covid-data-Taiwan-added.csv")


# ------------------------------------------------------------------------------
# DOUBLE CHECK VALUES

# equality
identical(
   dplyr::select(filter(OWID_data_Taiwan_added, location == 'Taiwan', date %in% filter(OWID_data, location == 'Taiwan')$date), -total_deaths, -new_deaths, -total_cases, -new_cases, -total_deaths_per_million, -total_cases_per_million),
   dplyr::select(filter(OWID_data, location == 'Taiwan', date %in% OWID_data_Taiwan_added$date), -total_deaths, -new_deaths, -total_cases, -new_cases, -total_deaths_per_million, -total_cases_per_million))
# TRUE


# ------------------------------------------------------------------------------
# PLOT COUNTRY COMPARISON

# RELOAD
OWID_data_Taiwan_added <- read_csv("owid-covid-data-Taiwan-added.csv")
# sort(unique(OWID_data_Taiwan_added$location))

MIN_DATE_SHOWN <- as.Date('2022-04-01')  # as.Date('2022-01-01')  # as.Date('2023-01-01')
# MAX_DATE <- as.Date(max(filter(OWID_data_Taiwan_added, location %in% LOCATIONS_SHOWN)$date))

# plot parameters
LOCATIONS_SHOWN <- c(
   'Australia',
   # 'Azerbaijan',  # UPDATING*
   # 'Bahrain',  # NOT updating
   # 'Botswana',  # not much
   'Canada',
   'Denmark',
   # 'Eswatini',  # not updating
   # 'Guatemala',  # not updating
   # 'Hong Kong',
   # 'Honduras',  # not updating
   # 'India',  # not updating
   # 'Ireland',
   # 'Israel',  # UPDATING*
   'Jamaica',
   # 'Japan',  # not updating
   # 'Jordan',  # not updating
   # 'Kazakhstan',  # not updating
   # 'Malaysia',  # no recent updates
   # 'Netherlands',  # no recent updates
   'New Zealand',
   # 'Palestine',  # not updating
   'Norway',
   # 'Oman',  # not updating
   # 'Philippines',
   # 'Singapore',
   'South Korea',
   'Taiwan'
   # 'Turkey',  # not updating
   # 'World'  # UPDATING*
)

length(LOCATIONS_SHOWN)  # 8
OWID_colors_n8 <- c('#18470F', '#286BBB', '#BE5915', '#2C8465', '#C15065', '#6D3E91', '#883039', '#CF0A66')
   # c('#CF0A66', '#18470F', '#2C8465', '#BE5915', 
   #                  '#C15065', '#6D3E91', '#286BBB', '#883039')

# view(filter(OWID_data_Taiwan_added, location %in% LOCATIONS_SHOWN, ! is.na(total_cases_per_million)) %>%
#    group_by(date) %>%
#    summarise(
#       num_non_na = sum(! is.na(total_deaths_per_million))
#    ))

(MAX_DATE <- OWID_data_Taiwan_added %>%
   filter(location %in% LOCATIONS_SHOWN) %>% 
   group_by(date) %>%
   summarize(
      total_non_na = sum(! is.na(total_deaths_per_million))
   ) %>%
   filter(total_non_na == length(LOCATIONS_SHOWN)) %>%
   pull(date) %>%
   max(na.rm = TRUE))
# "2023-10-18" on 20231021

# # find locations whose current values are near Taiwan
# unique(filter(OWID_data_Taiwan_added, 
#               date == MAX_DATE,
#               # total_deaths_per_million > max(filter(OWID_data_Taiwan_added, location == 'Taiwan')$total_deaths_per_million, na.rm = TRUE) - 200,
#               total_deaths_per_million > max(filter(OWID_data_Taiwan_added, location == 'Taiwan')$total_deaths_per_million, na.rm = TRUE),
#               total_deaths_per_million < max(filter(OWID_data_Taiwan_added, location == 'Taiwan')$total_deaths_per_million, na.rm = TRUE) + 500)$location)

# view(OWID_data_Taiwan_added %>%
#         filter(location %in% LOCATIONS_SHOWN) %>% 
#         group_by(date))

# What FONTS are available?
# quartzFonts()
# $serif
# [1] "Times-Roman"      "Times-Bold"       "Times-Italic"     "Times-BoldItalic"
# $sans
# [1] "Helvetica"             "Helvetica-Bold"        "Helvetica-Oblique"     "Helvetica-BoldOblique"
# $mono
# [1] "Courier"             "Courier-Bold"        "Courier-Oblique"     "Courier-BoldOblique"

generate_breaks <- function(n) {
   # Initial interval
   interval <- n / 5
   
   # Get rounding magnitude
   magnitude <- 10^floor(log10(interval))
   
   # Round the interval to the nearest "clean" number
   clean_interval <- ceiling(interval/magnitude) * magnitude
   
   # Generate breaks
   breaks <- seq(0, 5 * clean_interval, by = clean_interval)
   
   return(breaks)
}

FIVE_BREAKS <- breaks_pretty(n = 5)
# SIX_BREAKS <- breaks_pretty(n = 6)

# Create a custom date formatter function
pretty_date_format <- function() {
   function(x) {
      formatted <- format(x, "%b %e, %Y")
      gsub(" ([0-9],)", "\\1", formatted)
   }
}

# ------------------------------------------------------------------------------
# LOOP AND PRINT country comparison frames
image_counter <- 0
for (this_MAX_DATE in seq(as.Date(MIN_DATE_SHOWN) + 30, as.Date(MAX_DATE), by = "1 day")) {  # 10 days
   image_counter <- image_counter + 1
   image_name <- paste0('image_', sprintf("%03d", image_counter))
   
   # (this_MAX_DATE <- as.Date(MIN_DATE_SHOWN) + 30)
   # this_MAX_DATE <- MAX_DATE
   this_MAX_DATE <- as.Date(this_MAX_DATE)
   DAYS_TO_NUDGE <- round(0.1 * as.numeric(this_MAX_DATE - MIN_DATE_SHOWN))  # round(0.025 * as.numeric(this_MAX_DATE - MIN_DATE_SHOWN))
   # SEGMENT_TO_NUDGE <- DAYS_TO_NUDGE / 100
   
   # LABEL positions
   label_positions <- filter(OWID_data_Taiwan_added, location %in% LOCATIONS_SHOWN, date >= MIN_DATE_SHOWN, date <= this_MAX_DATE) %>%
      group_by(location) %>%
      summarize(date = max(date), total_deaths_per_million = max(total_deaths_per_million))
   
   # CREATE Y axis
   (Y_AXIS_MAX <- max(filter(OWID_data_Taiwan_added, location %in% LOCATIONS_SHOWN, date >= MIN_DATE_SHOWN, date <= this_MAX_DATE)$total_deaths_per_million, na.rm = TRUE))
   (Y_AXIS_BREAKS <- generate_breaks(Y_AXIS_MAX))
   
   X_AXIS_BREAKS <- FIVE_BREAKS(c(MIN_DATE_SHOWN, this_MAX_DATE))
   # X_AXIS_BREAKS <- SIX_BREAKS(c(MIN_DATE_SHOWN, this_MAX_DATE))
   X_AXIS_BREAKS <- X_AXIS_BREAKS[X_AXIS_BREAKS < this_MAX_DATE]
   
   # PLOT
   total_deaths_time_PLOT <- ggplot(data = filter(OWID_data_Taiwan_added, location %in% LOCATIONS_SHOWN, date >= MIN_DATE_SHOWN, date <= this_MAX_DATE), 
                                     mapping = aes(x = date, y = total_deaths_per_million, color = location)) + 
         
         # Custom Y AXIS
         annotate('segment', x = as.Date(-Inf), xend = this_MAX_DATE, y = Y_AXIS_BREAKS[2], yend = Y_AXIS_BREAKS[2], color = brewer.pal(9, "Greys")[4], linetype = "dotted", linewidth = 0.25) +
         annotate('segment', x = as.Date(-Inf), xend = this_MAX_DATE, y = Y_AXIS_BREAKS[3], yend = Y_AXIS_BREAKS[3], color = brewer.pal(9, "Greys")[4], linetype = "dotted", linewidth = 0.25) +
         annotate('segment', x = as.Date(-Inf), xend = this_MAX_DATE, y = Y_AXIS_BREAKS[4], yend = Y_AXIS_BREAKS[4], color = brewer.pal(9, "Greys")[4], linetype = "dotted", linewidth = 0.25) +
         annotate('segment', x = as.Date(-Inf), xend = this_MAX_DATE, y = Y_AXIS_BREAKS[5], yend = Y_AXIS_BREAKS[5], color = brewer.pal(9, "Greys")[4], linetype = "dotted", linewidth = 0.25) +
         
         # DATES
         # geom_segment(x = as.Date('2022-05-01'), y = -5, xend = as.Date('2022-05-01'), yend = 0, linetype = "solid", color = brewer.pal(9, "Greys")[6], size = 0.5) +
         # #annotate('text', x = as.Date('2022-05-01'), y = -2, label = "May 1", color = brewer.pal(9, "Greys")[6],
         # #         fontface = 'bold', hjust = 0.5, vjust = 1.25, size = 2) + #, size = 2.75) +
         # geom_segment(x = as.Date('2022-06-01'), y = -5, xend = as.Date('2022-06-01'), yend = 0, linetype = "solid", color = brewer.pal(9, "Greys")[6], size = 0.5) +
         
         # # TOTAL DEATHS
         # geom_bar(mapping = aes(y = Death), stat = "identity", color = "NA", position = position_dodge(0), alpha = 0.4, fill = '#660000') + 
         
         # Show 7-day window with line and error
      geom_line(aes(linewidth = ifelse(location == 'Taiwan', 0.6, 0.3))) + 
         
         # geom_text(data = label_positions, aes(label = location), nudge_x = DAYS_TO_NUDGE, hjust = 0, size = 3.25) +
         geom_text_repel(data = label_positions,
                         aes(label = location),  # x = date, 
                         # x = this_MAX_DATE + 2 * DAYS_TO_NUDGE,
                         # nudge_x = DAYS_TO_NUDGE,
                         direction = "y",
                         xlim = c(this_MAX_DATE + DAYS_TO_NUDGE, Inf),  # xlim = c(this_MAX_DATE + DAYS_TO_NUDGE * 4, Inf),
                         hjust = 0, size = 3.4,  # 3.25,
                         min.segment.length = 0.5,
                         segment.size = 0.2, segment.color = "lightgrey",  # segment.ncp = 5, segment.curvature = 0, segment.angle = 0,
                         box.padding = 0.25) +
         
         ## DEATHS
         #geom_bar(data = filter(data, date >= MIN_TIME_SHOWN_semirecent, imported == 'Local'),
         #         mapping = aes(y = Death), fill = "#841617", #  brewer.pal(9, "BuPu")[5], # "#EA5D64", 
         #         stat = "identity", color = "NA") + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8] #, alpha = 0.75  brewer.pal(9, 'Greys')[3]
         
         # LABS
         # ggtitle(label = paste0("Total deaths per million")) +
         labs(title = paste0("Cumulative confirmed COVID-19 deaths per million people"),
              subtitle = "“Due to varying protocols and challenges in the attribution of the cause of death, the number of confirmed deaths may\nnot accurately represent the true number of deaths caused by COVID-19” (Our World in Data).",
              caption = paste0("Author: @chasewnelson | Date: ", MAX_DATE, " | Design: OWID | Data: JHU, WHO/OWID, Taiwan CDC")) +  # **Data:**  and '  \n'
         
         # X AXIS
         geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6], linewidth = 0.5) +
         
         # Cover up extra space for labels
         # annotate('segment', x = as.Date(this_MAX_DATE), xend = as.Date(Inf), y = 0, yend = 0, color = 'white', linewidth = 1) +
         geom_segment(mapping = aes(x = max(date)), xend = Inf, y = 0, yend = 0, color = 'white') +
         
         # TODAY'S WINDOW VALUE
         geom_point(data = filter(OWID_data_Taiwan_added, location %in% LOCATIONS_SHOWN, date == this_MAX_DATE),
                    mapping = aes(x = date, y = total_deaths_per_million, color = location), inherit.aes = TRUE) +
         
         # Custom X AXIS labels: dates
         #annotate('text', x = as.Date('2022-01-01'), y = 2.5, label = "Jan 1", color = brewer.pal(9, 'Greys')[9], hjust = 0.5, vjust = -0.5, size = 2.25) +
         #geom_segment(x = as.Date('2022-01-01'), y = -2.5, xend = as.Date('2022-01-01'), yend = 2.5, linetype = "solid", color = brewer.pal(9, "Greys")[9], size = 0.75) +
         
         # #annotate('text', x = as.Date(-Inf), y = 50, label = "50 deaths", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 2.25) +
         # #annotate('text', x = as.Date(-Inf), y = 40, label = "40", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 2.25) +
         # annotate('text', x = as.Date(-Inf), y = 200, label = "200", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 1.5) +  
      # annotate('text', x = as.Date(-Inf), y = 100, label = "100", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 1.5) +
      # #annotate('text', x = as.Date(-Inf), y = 75, label = "75", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 1.5) +
      # #annotate('text', x = as.Date(-Inf), y = 50, label = "50", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 1.5) +
      # #annotate('text', x = as.Date(-Inf), y = 25, label = "25", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 1.5) +
      # #annotate('text', x = as.Date(-Inf), y = 10, label = "10", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 2.25) +
      
      # Custom Y AXIS labels: IMPORTED (below axis, negative)
      #annotate('text', x = as.Date(-Inf), y = -250, label = "250 daily cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.2, size = 2.25) +
      
      theme(
         #panel.grid = element_blank(),
         panel.background = element_blank(),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),  # element_line(colour = brewer.pal(9, 'Greys')[2], linetype = "solid", size = 0.2),  # 
         # panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
         plot.margin = unit(x = c(0.75, 0.25, 0.5, 0.5), units = "line"),
         legend.position = 'none',
         # legend.title = element_blank(),
         plot.title = element_text(size = 12, color = brewer.pal(9, "Greys")[9], family = "Arial", face = "bold",  # "Georgia"=owid "EB Garamond" face = "bold", 
                                   margin = unit(x = c(0, 0, 0, 0), units = "line")), #, size = 12),hjust = 0.5, 
         plot.subtitle = element_text(size = 6, color = brewer.pal(9, "Greys")[6], family = 'Arial', margin = unit(c(0.5, 0, 1, 0), units = "line")), #, size = 10), hjust = 0.5, 
         plot.caption = element_markdown(size = 5.5, color = brewer.pal(9, "Greys")[4], family = 'Arial'),  # element_text(color = brewer.pal(9, "Greys")[4], size = 6),
         #axis.text.x = element_text(size = 7),
         # axis.text.x = element_blank(),
         axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6], size = 7),
         axis.text.y = element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),  # element_blank(), # 
         axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6], linewidth = 0.5),
         axis.ticks.y = element_blank(),
         axis.line = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(), # element_text(size = 9),
         strip.background = element_blank()) +
         xlab("") + ylab("Total deaths per million") +
         # scale_color_viridis_d(option = "H") +  # D H
      scale_color_manual(values = OWID_colors_n8) +
         # scale_color_manual(values = c('#dc8665', '#534666', '#cd7672', '#eeb562', 'darkgrey', '#138086')) +
         # scale_color_viridis_d(option = "D") +  # D H
         # scale_color_manual(values = c('#d9d9d9', '#faa75b', '#9e9ac8', '#fc9272', '#fdae6b', '#bcbddc', '#fded91', '#31a354')) +
         # scale_color_manual(values = c('#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3', '#a6d854', '#ffd92f', '#e5c494', '#d53e4f')) +
         scale_linewidth_identity() +
         
         scale_x_date(labels = pretty_date_format(),  # date_format("%b%e, %Y"),
                      breaks = X_AXIS_BREAKS,  # pretty_breaks(5),
                      expand = expansion(mult = c(0, 0.2)),
                      limits = c(MIN_DATE_SHOWN - 2, this_MAX_DATE + DAYS_TO_NUDGE)) + #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
         #breaks = seq(as.Date(MIN_TIME_SHOWN_semirecent), as.Date(timenow), by = '15 day'))  + # by = "7 day"
         #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
         #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
         
         # CHANGETHIS <==
         scale_y_continuous(limits = c(0, Y_AXIS_MAX + 2),
                            breaks = Y_AXIS_BREAKS,  # seq(0, 200, 100),
                            expand = expansion(mult = c(0, 0.05)))
   # total_deaths_time_PLOT
   
   # SAVE
   # png(filename = paste0("time_lapse_frames/total_deaths_time_PLOT_", MIN_DATE_SHOWN, '_', this_MAX_DATE, ".png"), width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
   # jpeg(filename = paste0("time_lapse_frames/", image_name, ".jpeg"), width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
   png(filename = paste0("time_lapse_frames/", image_name, ".png"), width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
   print(total_deaths_time_PLOT)
   dev.off()
   
}

# Loop and print the last frame a bunch more times
num_additional_frames <- 100
for (i in 1:num_additional_frames) {
   image_counter <- image_counter + 1
   image_name <- paste0('image_', sprintf("%03d", image_counter))
   
   # SAVE
   # png(filename = paste0("time_lapse_frames/total_deaths_time_PLOT_", MIN_DATE_SHOWN, '_', this_MAX_DATE, ".png"), width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
   # jpeg(filename = paste0("time_lapse_frames/", image_name, ".jpeg"), width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
   png(filename = paste0("time_lapse_frames/", image_name, ".png"), width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
   print(total_deaths_time_PLOT)
   dev.off()
}


# ----------
# SAVE latest image static
# png(filename = paste0("cumulative_deaths_", str_replace_all(this_MAX_DATE, '-', ''), ".png"), width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
png(filename = "cumulative_deaths.png", width = 5.5, height = (9/16) * 5.5, units = 'in', res = 500)
png(filename = "cumulative_deaths_SMALL.png", width = 5.5, height = (9/16) * 5.5, units = 'in', res = 250)
print(total_deaths_time_PLOT)
dev.off()


# ------------------------------------------------------------------------------
# At Bash Command Line, to make video time lapse

# # for filename in $(ls *.png); do echo "file '$filename'"; done > images.txt
# # echo "file $(ls *.png | tail -n 1)" > last_image.txt

# ffmpeg -framerate 66 -i image_%03d.png -c:v libx264 -pix_fmt yuv420p cumulative_deaths_time_lapse.mp4


# ------------------------------------------------------------------------------
# Taiwan's current number
filter(OWID_data_Taiwan_added, location == 'Taiwan', date == MAX_DATE)$total_deaths  # 22659 | 22661 | 22690
max(filter(OWID_data_Taiwan_added, location == 'Taiwan')$total_deaths, na.rm = TRUE)  # 22659 | 22661 | 22725

filter(OWID_data_Taiwan_added, location == 'Taiwan', date == MAX_DATE)$total_deaths_per_million  # 948.3374 | 948.4211 | 949.6348
max(filter(OWID_data_Taiwan_added, location == 'Taiwan')$total_deaths_per_million, na.rm = TRUE)  # 948.3374 | 948.4211 | 951.0996

# # ----------
# # Compare China and Taiwan
# # view(arrange(filter(OWID_data_Taiwan_added, location %in% c('China', 'Taiwan')), date))
# # view(arrange(filter(OWID_data_Taiwan_added, location %in% c('Taiwan')), date))
# 
# # create side-by-side table
# 
# # CHN
# CHN_for_comparison <- dplyr::select(filter(OWID_data_Taiwan_added, location == 'China', date > as.Date('2023-01-01')),
#                                     date, total_cases, new_cases, total_deaths, new_deaths, population)
# names(CHN_for_comparison) <- c('date', paste0(setdiff(names(CHN_for_comparison), 'date'), '_CHN'))
# CHN_for_comparison  # 290 × 6
# 
# # TWN
# TWN_for_comparison <- dplyr::select(filter(OWID_data_Taiwan_added, location == 'Taiwan', date > as.Date('2023-01-01')),
#                                     date, total_cases, new_cases, total_deaths, new_deaths, population)
# names(TWN_for_comparison) <- c('date', paste0(setdiff(names(TWN_for_comparison), 'date'), '_TWN'))
# TWN_for_comparison  # 292 × 6
# 
# # join
# all(TWN_for_comparison$date %in% CHN_for_comparison$date)  # FALSE
# all(CHN_for_comparison$date %in% TWN_for_comparison$date)  # TRUE <== TWN left side
# (TWN_CHN_comparison <- left_join(TWN_for_comparison,
#                                  CHN_for_comparison,
#                                  by = 'date'))
# 
# # SAVE
# # write_csv(TWN_CHN_comparison, "TWN_CHN_comparison_2023.csv")


# # ------------------------------------------------------------------------------
# # POPULATION SIZES
# (OWID_pop_sizes <- OWID_data_Taiwan_added |>
#    group_by(location) |>
#    summarise(
#       min_pop_size = min(population, na.rm = TRUE),
#       max_pop_size = max(population, na.rm = TRUE),
#    ) |>
#     arrange(min_pop_size))
# 
# # write_tsv(OWID_pop_sizes, "OWID_pop_sizes.tsv")
# 
# pop_size_ecdf <- ecdf(OWID_pop_sizes$max_pop_size)
# (TWN_pop_size_percentile <- pop_size_ecdf(OWID_pop_sizes[OWID_pop_sizes$location == 'Taiwan',]$max_pop_size))
# # 0.7294118

