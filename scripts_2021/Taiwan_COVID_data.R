### Taiwan COVID-19 Data

# Chase W. Nelson
# cnelson@amnh.org

#Sources:
#(1) Tian Xia, https://web.cw.com.tw/covid-live-updates-2021-en/index.html
#(2) Taiwan CDC daily report photos, e.g. https://www.facebook.com/mohw.gov.tw/photos/?ref=page_internal
#(3) Taiwan CDC Daily Number of Cases Suspected SARS -CoV-2 Infection Tested, e.g. https://data.cdc.gov.tw/en/dataset/daily-cases-suspected-sars-cov-2-infection_tested 
#install.packages("ggimage")
#install.packages("rsvg")
#install.packages("svg")
#install.packages("emojifont")

#library(emojifont)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggimage)
library(jsonlite)
library(patchwork)
#library(lubridate)
#library(ggimage)
#library(rsvg)
#library(svg)
#library(png)
#library(grid)
#devtools::install_github("clauswilke/ggtext")
#library(ggtext)
#library(emo)


#install.packages("extrafont")
## https://www.r-bloggers.com/2019/03/adding-custom-fonts-to-ggplot-in-r/
#library(extrafont)
##font_import() # only once, ever
#
#install.packages("cowplot")
#library(cowplot)
#
#loadfonts(device = "pdf", quiet = TRUE) # every time
#loadfonts(device = "postscript", quiet = TRUE) # every time
#
## https://stackoverflow.com/questions/50558050/ggplot2-ggsave-keep-embedded-fonts-in-exported-png
#library(showtext)
#
## https://stackoverflow.com/questions/4094094/modifying-fonts-in-ggplot2
## https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/


case_data <- read_tsv("~/Desktop/Taiwan_COVID_data/covid19_tw_specimen.tsv")
# New_Local_Cases - the number of new local cases reported that day
# Total - SEEMINGLY the number of tests reported that day

# format dates
head(case_data$Date_of_Reporting)
# [1] "2020/1/15" "2020/1/16" "2020/1/17" "2020/1/18" "2020/1/19" "2020/1/20"

# format times
(case_data$Date_of_Reporting <- as.Date(case_data$Date_of_Reporting))

# sort by date
(case_data <- dplyr::arrange(case_data, Date_of_Reporting))

# format case counts
(case_data$New_Local_Cases <- as.integer(case_data$New_Local_Cases))
(case_data$New_Local_Cases_Revised <- as.integer(case_data$New_Local_Cases_Revised)) # TODAY'S
(case_data$New_Local_Cases_Original <- as.integer(case_data$New_Local_Cases_Original))

########## ADD TO THIS EVERY DAY ##########
# format case counts for individual dates, for DAYS BEFORE PRESENT
(case_data$New_Local_Cases_20210522 <- as.integer(case_data$New_Local_Cases_20210522))
(case_data$New_Local_Cases_20210523 <- as.integer(case_data$New_Local_Cases_20210523))
(case_data$New_Local_Cases_20210524 <- as.integer(case_data$New_Local_Cases_20210524))
(case_data$New_Local_Cases_20210525 <- as.integer(case_data$New_Local_Cases_20210525))
(case_data$New_Local_Cases_20210526 <- as.integer(case_data$New_Local_Cases_20210526))
(case_data$New_Local_Cases_20210527 <- as.integer(case_data$New_Local_Cases_20210527))
(case_data$New_Local_Cases_20210528 <- as.integer(case_data$New_Local_Cases_20210528))
(case_data$New_Local_Cases_20210529 <- as.integer(case_data$New_Local_Cases_20210529))
(case_data$New_Local_Cases_20210530 <- as.integer(case_data$New_Local_Cases_20210530))
(case_data$New_Local_Cases_20210531 <- as.integer(case_data$New_Local_Cases_20210531))
(case_data$New_Local_Cases_20210601 <- as.integer(case_data$New_Local_Cases_20210601))
(case_data$New_Local_Cases_20210602 <- as.integer(case_data$New_Local_Cases_20210602))
(case_data$New_Local_Cases_20210603 <- as.integer(case_data$New_Local_Cases_20210603))
(case_data$New_Local_Cases_20210604 <- as.integer(case_data$New_Local_Cases_20210604))
(case_data$New_Local_Cases_20210605 <- as.integer(case_data$New_Local_Cases_20210605))
(case_data$New_Local_Cases_20210606 <- as.integer(case_data$New_Local_Cases_20210606))
########## ADD TO THIS EVERY DAY ##########

# find minimum and maximum dates
(time0 <- min(case_data$Date_of_Reporting))
(timenow <- max(case_data$Date_of_Reporting))
(todaystring <- as.character(format(x = timenow, format = "%b %d")))

# label days since time0
(case_data$day <- case_data$Date_of_Reporting - time0 + 1) # as.integer()



###############################################################################
### sliding window of case statistics

# sliding window constants
WINDOW_SIZE <- 7 # days
MIN_DATA_COUNT <- 7

# initialize sliding window columns
case_data$sw_start <- NA
case_data$sw_center <- NA
case_data$sw_end <- NA
case_data$sw_num_days_with_data <- NA

case_data$sw_new_local_cases_sum <- NA
case_data$sw_new_local_cases_mean <- NA
case_data$sw_new_local_cases_SE <- NA

case_data$sw_new_local_cases_REVISED_sum <- NA
case_data$sw_new_local_cases_REVISED_mean <- NA
case_data$sw_new_local_cases_REVISED_SE <- NA

case_data$sw_new_tests_sum <- NA
case_data$sw_prop_tests_positive <- NA
case_data$sw_prop_tests_positive_CIlow <- NA
case_data$sw_prop_tests_positive_CIhigh <- NA
#View(case_data)

# perform sliding window
for(i in 1:(max(case_data$day) - WINDOW_SIZE + 1)) { # each window of time starting at day 1
  #i <- 491
  cat(paste0(i, ' '))
  
  # Extract window; analyze
  window_case_data <- filter(case_data, day >= i, day <= (i + WINDOW_SIZE - 1))
  #window_case_data <- filter(window_case_data, ! is.na(Total), Total > 0) # check this later
  lowest_day <- min(window_case_data$day)
  #highest_day <- max(window_case_data$day)
  highest_day <- lowest_day + WINDOW_SIZE - 1
  new_local_cases_data_count <- sum(! is.na(window_case_data$New_Local_Cases))
  new_local_cases_Revised_data_count <- sum(! is.na(window_case_data$New_Local_Cases))
  new_tests_data_count <- sum(! is.na(window_case_data$Total))
  
  #View(window_case_data)

  # windows
  if(new_local_cases_data_count >= MIN_DATA_COUNT) { #  && new_tests_data_count >= MIN_DATA_COUNT
    # Add results to table
    sw_new_local_cases_sum <- sum(window_case_data$New_Local_Cases, na.rm = TRUE)
    sw_new_local_cases_mean <- sw_new_local_cases_sum / new_local_cases_data_count
    sw_new_local_cases_SE <- sd(window_case_data$New_Local_Cases, na.rm = TRUE) / sqrt(new_local_cases_data_count)
    
    sw_new_local_cases_Revised_sum <- sum(window_case_data$New_Local_Cases_Revised, na.rm = TRUE)
    sw_new_local_cases_Revised_mean <- sw_new_local_cases_Revised_sum / new_local_cases_Revised_data_count
    sw_new_local_cases_Revised_SE <- sd(window_case_data$New_Local_Cases_Revised, na.rm = TRUE) / sqrt(new_local_cases_Revised_data_count)
    
    sw_new_tests_sum <- sum(window_case_data$Total, na.rm = TRUE)
    case_data[case_data$day == lowest_day, ]$sw_start <- lowest_day
    case_data[case_data$day == lowest_day, ]$sw_center <- (lowest_day + highest_day) / 2
    case_data[case_data$day == lowest_day, ]$sw_end <- highest_day
    case_data[case_data$day == lowest_day, ]$sw_num_days_with_data <- nrow(window_case_data)
    
    case_data[case_data$day == lowest_day, ]$sw_new_local_cases_sum <- sw_new_local_cases_sum
    case_data[case_data$day == lowest_day, ]$sw_new_local_cases_mean <- sw_new_local_cases_mean
    case_data[case_data$day == lowest_day, ]$sw_new_local_cases_SE <- sw_new_local_cases_SE
    
    case_data[case_data$day == lowest_day, ]$sw_new_local_cases_Revised_sum <- sw_new_local_cases_Revised_sum
    case_data[case_data$day == lowest_day, ]$sw_new_local_cases_Revised_mean <- sw_new_local_cases_Revised_mean
    case_data[case_data$day == lowest_day, ]$sw_new_local_cases_Revised_SE <- sw_new_local_cases_Revised_SE
    
    # binomial confidence interval for New_Local_Cases
    if(lowest_day == (highest_day - WINDOW_SIZE + 1) && new_tests_data_count >= MIN_DATA_COUNT) { # CONSIDER CHANGE
      case_data[case_data$day == lowest_day, ]$sw_new_tests_sum <- sw_new_tests_sum
      
      if(new_local_cases_data_count == new_tests_data_count && sw_new_local_cases_sum > 0) { # at least one case required
        binomial_result <- prop.test(x = sw_new_local_cases_sum, n = sum(window_case_data$Total), alternative = "two.sided", conf.level = 0.95)
        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive <- as.numeric(binomial_result$estimate)
        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIlow <- as.numeric(binomial_result$conf.int[1])
        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIhigh <- as.numeric(binomial_result$conf.int[2])
      } else {
        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive <- 0
        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIlow <- 0
        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIhigh <- 0
      }
    } else {
      case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive <- NA
      case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIlow <- NA
      case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIhigh <- NA
    }
    
#    # binomial confidence interval for New_Local_Cases_Revised
#    if(lowest_day == (highest_day - WINDOW_SIZE + 1)) {
#      if(new_local_cases_Revised_data_count == new_tests_data_count && sw_new_local_cases_Revised_sum > 0) { # at least one case required
#        binomial_result <- prop.test(x = sw_new_local_cases_Revised_sum, n = sum(window_case_data$Total), alternative = "two.sided", conf.level = 0.95)
#        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive <- as.numeric(binomial_result$estimate)
#        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIlow <- as.numeric(binomial_result$conf.int[1])
#        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIhigh <- as.numeric(binomial_result$conf.int[2])
#      } else {
#        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive <- 0
#        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIlow <- 0
#        case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIhigh <- 0
#      }
#    } else {
#      case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive <- NA
#      case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIlow <- NA
#      case_data[case_data$day == lowest_day, ]$sw_prop_tests_positive_CIhigh <- NA
#    }
    
  } # else leave it as NA
} # end last window
# WARNINGS are ok, indicating early days without data (NA)

# format sw_center as date
case_data$sw_start_date <- as.Date((time0 - 1) + case_data$sw_start)
case_data$sw_center_date <- as.Date((time0 - 1) + case_data$sw_center)
case_data$sw_end_date <- as.Date((time0 - 1) + case_data$sw_end)


###############################################################################
### NEW LOCATION IN SCRIPT

### PLOT GLOBAL VARIABLES
#MIN_DATE_DISPLAYED <- as.Date("2021-04-20")
LABOR_DAY <- as.Date("2021-04-30")
LEVEL3_DAY <- as.Date("2021-05-15")
LEVEL3_DAY_COUNTRY <- as.Date("2021-05-19")
#INCUBATION_TIME <- round(5.7) # Ferretti et al. 2021 preprint
#MIN_DATE_DISPLAYED <- timenow - 4 * INCUBATION_TIME
INCUBATION_TIME <- round(5) # Ferretti et al. 2021 preprint
#MIN_DATE_DISPLAYED <- timenow - 5 * INCUBATION_TIME
MIN_DATE_DISPLAYED <- as.Date("2021-05-01")


###############################################################################
### NEW:  FILTER just dates needed
#case_data <- dplyr::filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)

###############################################################################
# IMPORT case data 2 and JOIN /
###############################################################################
(case_data2 <- read_tsv("~/Desktop/Taiwan_COVID_data/covid19_tw_metadata.tsv"))

# format dates and sort
head(case_data2$date)
(case_data2$date <- as.Date(case_data2$date))
(case_data2 <- dplyr::arrange(case_data2, date))

# get min date
#(time0_data2 <- min(case_data2$date))
#View(case_data2)


### BOOKKEEPING
# we'll need:
# date
# unknown_source_prop
# total_num_cases
# cumulative_confirmed_number
# cumulative_deaths
# not_yet_interpreted_TotalInspection_MIN_Completed
# test_total_positive_rate_Confirmed_DIV_Completed
# not_inspected_on_the_day_SendForInspection_MIN_CompleteInterpretation
# total_unchecked_on_the_day
# same_day_test_positive_rate_DiagnosedOnDay_DIV_TestVolumeOnDay

# EXTRACT positive rates from case_data2
case_data2_positiveRates <- dplyr::select(filter(case_data2, date >= MIN_DATE_DISPLAYED), 
                                          date, cumulative_confirmed_number, total_interpretation_on_day_Confirmed_PLUS_Excluded, 
                                          same_day_test_positive_rate_DiagnosedOnDay_DIV_TestVolumeOnDay,
                                          change_in_cumulative_positive, test_pos_rate_redun)

# JOIN
names(case_data2_positiveRates)[names(case_data2_positiveRates) == "date"] <- "Date_of_Reporting"
case_data <- left_join(x = case_data, y = case_data2_positiveRates, by = "Date_of_Reporting")

# INSPECT
case_data
#View(case_data)

###############################################################################
# / end import case data 2
###############################################################################


###############################################################################
### NEW: daily prop tests positive
case_data$day_prop_tests_positive <- NA
case_data$day_prop_tests_positive_CIlow <- NA
case_data$day_prop_tests_positive_CIhigh <- NA

for(i in 1:(nrow(case_data))) { 
  if(! is.na(case_data[i, ]$change_in_cumulative_positive) && 
     ! is.na(case_data[i, ]$total_interpretation_on_day_Confirmed_PLUS_Excluded)) {
    
    binomial_result <- prop.test(x = case_data[i, ]$change_in_cumulative_positive, 
                                 n = case_data[i, ]$total_interpretation_on_day_Confirmed_PLUS_Excluded, 
                                 alternative = "two.sided", 
                                 conf.level = 0.95)
    case_data[i, ]$day_prop_tests_positive <- as.numeric(binomial_result$estimate)
    case_data[i, ]$day_prop_tests_positive_CIlow <- as.numeric(binomial_result$conf.int[1])
    case_data[i, ]$day_prop_tests_positive_CIhigh <- as.numeric(binomial_result$conf.int[2])
  } else {
    case_data[i, ]$day_prop_tests_positive <- NA
    case_data[i, ]$day_prop_tests_positive_CIlow <- NA
    case_data[i, ]$day_prop_tests_positive_CIhigh <- NA
  }
}
###############################################################################


# view
View(case_data)

## PLOT 1 - num cases vs. prop positive - NOT INFORMATIVE (?)
#PROP_DISPLAY_FACTOR <- max(case_data$New_Local_Cases, na.rm = TRUE) / max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) # get it to equal max cases
#(localCases_propPos_PLOT <- ggplot(data = filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED), mapping = aes(x = Date_of_Reporting, y = New_Local_Cases)) + # color = significant
#    geom_bar(stat = 'identity') +
#    
#    geom_line(data = filter(case_data, sw_center_date >= MIN_DATE_DISPLAYED), # MIN_DATE_DISPLAYED - (WINDOW_SIZE -1) / 2
#              mapping = aes(x = sw_center_date, y = PROP_DISPLAY_FACTOR * sw_prop_tests_positive), color = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
#    geom_ribbon(data = filter(case_data, sw_center_date >= MIN_DATE_DISPLAYED), # MIN_DATE_DISPLAYED - (WINDOW_SIZE -1) / 2
#                mapping = aes(x = sw_center_date, ymin = PROP_DISPLAY_FACTOR * sw_prop_tests_positive_CIlow, ymax = PROP_DISPLAY_FACTOR * sw_prop_tests_positive_CIhigh), 
#                alpha = 0.075, linetype = 0, fill = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
#    
#    geom_vline(xintercept = LABOR_DAY,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY, y = 750), color = 'darkgrey', label = "Labor Day", hjust = 1.1) + # , expression(italic('π')['S']), , hjust = -0.2)
#    
#    geom_vline(xintercept = LABOR_DAY + INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+1it", hjust = 1.1) +
#    
#    geom_vline(xintercept = LABOR_DAY + 2 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + 2 * INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+2it", hjust = 1.1) +
#    
#    geom_vline(xintercept = LABOR_DAY + 3 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + 3 * INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+3it", hjust = 1.1) +
#    
##    geom_vline(xintercept = LABOR_DAY + 4 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
##    geom_text(mapping = aes(x = LABOR_DAY + 4 * INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+4si", hjust = 1.1) +
#    
#    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED + 6, y = 220),
#              color = brewer.pal(9, 'Reds')[8], label = "Prop pos specimens\n(7-day window)", hjust = 0.5) + # , vjust = 1
#    
#    geom_text(mapping = aes(x = timenow - 5.5, y = 320), # <-- CHANGETHIS
#              color = brewer.pal(9, 'Greys')[7], label = "Daily\n(non-revised)", hjust = 0.5, vjust = -0.5) + # , vjust = 1
#    
#    #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
#    theme_classic() +
#    theme(panel.grid = element_blank(),
#          legend.position = 'none',
#          legend.title = element_blank(),
#          #axis.text.x = element_text(size = 7),
#          #axis.text.x = element_blank(),
#          axis.text.y = element_text(size = 9),
#          axis.title.y = element_text(size = 9),
#          strip.background = element_blank()) +
#    xlab("") + ylab("Cases") +
#    scale_x_date(labels = date_format("%b %d"),
#                 expand = expand_scale(mult = c(0, 0)),
#                 limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
#                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
#    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
#    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
#    scale_y_continuous(limits = c(0, 775),
#                       expand = expand_scale(mult = c(0, 0)))) #
##      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases, na.rm = TRUE)),
##                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)
#
##View(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED))
#
## SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/localCases_propPos_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
#localCases_propPos_PLOT
#dev.off()



# PLOT 1 - num cases vs. prop positive - CHANGE TO LAST 7 DAYS
PROP_DISPLAY_FACTOR <- max(case_data$New_Local_Cases, na.rm = TRUE) / max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) # get it to equal max cases
(localCases_propPos_PLOT <- ggplot(data = filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED), mapping = aes(x = sw_end_date, y = sw_new_local_cases_sum / WINDOW_SIZE)) + # color = significant
    geom_bar(stat = 'identity') +
    
    geom_line(mapping = aes(y = PROP_DISPLAY_FACTOR * sw_prop_tests_positive), color = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
    geom_ribbon(mapping = aes(ymin = PROP_DISPLAY_FACTOR * sw_prop_tests_positive_CIlow, ymax = PROP_DISPLAY_FACTOR * sw_prop_tests_positive_CIhigh), 
                alpha = 0.075, linetype = 0, fill = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
    
#    geom_vline(xintercept = LABOR_DAY,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY, y = 750), color = 'darkgrey', label = "Labor Day", hjust = 1.1) + # , expression(italic('π')['S']), , hjust = -0.2)
#    
#    geom_vline(xintercept = LABOR_DAY + INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+1it", hjust = 1.1) +
#    
#    geom_vline(xintercept = LABOR_DAY + 2 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + 2 * INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+2it", hjust = 1.1) +
#    
#    geom_vline(xintercept = LABOR_DAY + 3 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + 3 * INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+3it", hjust = 1.1) +
    
    #    geom_vline(xintercept = LABOR_DAY + 4 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
    #    geom_text(mapping = aes(x = LABOR_DAY + 4 * INCUBATION_TIME, y = 750), color = 'darkgrey', label = "+4si", hjust = 1.1) +
    
    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED, y = 175),
              color = brewer.pal(9, 'Reds')[8], label = "Prop pos specimens", hjust = -0.05) + # , vjust = 1, \n(7-day window)
    
    geom_text(mapping = aes(x = timenow - 7.75, y = 200), # <-- CHANGETHIS
              color = brewer.pal(9, 'Greys')[7], label = "Daily total\nspecimens", hjust = 0.5, vjust = -0.5) + # , vjust = 1
    
    #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = 'none',
          legend.title = element_blank(),
          #axis.text.x = element_text(size = 7),
          #axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          strip.background = element_blank()) +
    xlab("") + ylab("Cases") +
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0, 0)),
                 limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
    scale_y_continuous(limits = c(0, 775),
                       expand = expand_scale(mult = c(0, 0)))) #
#      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases, na.rm = TRUE)),
#                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)

#View(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED))

# ONE will be missing: the RED LINE TOTAL

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/propPos_PLOT_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
localCases_propPos_PLOT
dev.off()


# INTERPRETATION: The proportion of positive tests has increased. This is not necessarily bad,
# but it does highlight that (1) overall testing capacity is limited and (2) testing is highly targeted those those most likely to be cases. 
# Ideally we would have more testing and broader surveillance.


# PLOT 2 - Num cases vs. tests - NOT EASY TO INTERP
TESTS_DISPLAY_FACTOR <- max(case_data$sw_new_local_cases_sum, na.rm = TRUE) / max(case_data$sw_new_tests_sum, na.rm = TRUE) # get it to equal max cases
(localCases_TestsPerformed_PLOT <- ggplot(data = filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED), mapping = aes(x = sw_end_date, y = sw_new_local_cases_sum / WINDOW_SIZE)) + # color = significant
    geom_bar(stat = 'identity') +
    geom_line(data = filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED), # MIN_DATE_DISPLAYED - (WINDOW_SIZE -1) / 2
              mapping = aes(x = sw_end_date, y = TESTS_DISPLAY_FACTOR * sw_new_tests_sum / WINDOW_SIZE), color = brewer.pal(9, 'Set1')[2]) + # mapping = aes(x = date, y = dNdS), color = 'black'
    
#    geom_vline(xintercept = LABOR_DAY,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY, y = max(case_data$sw_new_local_cases_sum, na.rm = TRUE)), color = 'darkgrey', label = "Labor Day", hjust = 1.1, vjust = 1.5) + # , expression(italic('π')['S']), , hjust = -0.2)
#    
#    geom_vline(xintercept = LABOR_DAY + INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + INCUBATION_TIME, y = max(case_data$sw_new_local_cases_sum, na.rm = TRUE)), color = 'darkgrey', label = "+1si", hjust = 1.1, vjust = 1.5) +
#    
#    geom_vline(xintercept = LABOR_DAY + 2 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + 2 * INCUBATION_TIME, y = max(case_data$sw_new_local_cases_sum, na.rm = TRUE)), color = 'darkgrey', label = "+2si", hjust = 1.1, vjust = 1.5) +
#    
#    geom_vline(xintercept = LABOR_DAY + 3 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + 3 * INCUBATION_TIME, y = max(case_data$sw_new_local_cases_sum, na.rm = TRUE)), color = 'darkgrey', label = "+3si", hjust = 1.1, vjust = 1.5) +
#    
#    geom_vline(xintercept = LABOR_DAY + 4 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
#    geom_text(mapping = aes(x = LABOR_DAY + 4 * INCUBATION_TIME, y = max(case_data$sw_new_local_cases_sum, na.rm = TRUE)), color = 'darkgrey', label = "+4si", hjust = 1.1, vjust = 1.5) +
#    
    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED, 
                            y = 50),#min(filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED)$sw_new_local_cases_sum, na.rm = TRUE)), 
              color = brewer.pal(9, 'Set1')[2], label = "Test specimens submitted", hjust = 0) + # , vjust = 1
    
    #geom_text(mapping = aes(x = mean(range(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)$Date_of_Reporting)), y = Inf),
    #          label = "Tests and Local Cases\n(7 Day Averages)", vjust = 1.25, size = 4.25) +
    ggtitle(label = "Tests and Local Cases", subtitle = "(7 Day Average)") +
    
    #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = 'none',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          #axis.text.x = element_text(size = 7),
          #axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          strip.background = element_blank()) +
    xlab("") + ylab("Cases") +
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0, 0)),
                 limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
    scale_y_continuous(expand = expand_scale(mult = c(0, 0)))) #
# remove one row ok because blue line stops one row early
#      sec.axis = sec_axis(~.*(max(case_data$sw_new_tests_sum, na.rm = TRUE) / max(case_data$sw_new_local_cases_sum, na.rm = TRUE)),
#                                           name = "Tests performed (last 7 days)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)
#View(filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED))

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/testsPerformed_PLOT_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
localCases_TestsPerformed_PLOT
dev.off()

# correlation??
cor.test(filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED)$sw_new_local_cases_sum,
         filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED)$sw_new_tests_sum,
         method = 'spearman') 
# 2021-05-22
#S = 959.06, p-value = 1.005e-09
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.8397293 

# 2021-05-23
#S = 961.01, p-value = 1.466e-10
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.8531684 

# 2021-05-24
#S = 958.79, p-value = 4.25e-08
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.8066956 


cor.test(filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED)$sw_new_local_cases_sum,
         filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED)$sw_new_tests_sum,
         method = 'pearson') 
# 2021-05-22
#t = 49.259, df = 31, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.9870998 0.9969018
#sample estimates:
#  cor 
#0.9936727 

# 2021-05-23
#t = 49.834, df = 32, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.9871399 0.9968389
#sample estimates:
#  cor 
#0.9936189 

# 2021-05-24
#t = 39.031, df = 29, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.9804167 0.9955153
#sample estimates:
#  cor 
#0.9906159 


# INTERPRETATION: 
# The number of local cases reported is nearly perfectly correlated with the number of tests performed (r=0.994, p<0.001, Pearson's correlation).
# 
# 




# CUMULATIVE SUM
case_data$New_Local_Cases_Revised_CUMSUM <- NA
case_data[case_data$Date_of_Reporting >= MIN_DATE_DISPLAYED, ]$New_Local_Cases_Revised_CUMSUM <- 
  cumsum(replace_na(data = case_data[case_data$Date_of_Reporting >= MIN_DATE_DISPLAYED, ]$New_Local_Cases_Revised, replace = 0))

# PLOT 3 - num daily cases vs. cumsum - REVISED VALUES NOT RECOMMENDED, ARTIFACTUAL DROP
(localCases_cumSum_PLOT <- ggplot(data = filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED), mapping = aes(x = Date_of_Reporting, y = New_Local_Cases_Revised)) + # color = significant
    
    # Show cumulative with bars?
    geom_bar(mapping = aes(y = New_Local_Cases_Revised_CUMSUM), fill = 'lightgrey', stat = "identity") + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8]
    
    # Show cumulative with line?
    #geom_line(mapping = aes(y = New_Local_Cases_Revised_CUMSUM), color = brewer.pal(9, 'Greys')[5]) + # brewer.pal(9, 'Reds')[8]
    
    #    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED, 
    #                            y = 200),
    #              color = brewer.pal(9, 'Reds')[8], label = "Prop tests positive (7-day window)", hjust = 0) + # , vjust = 1
    #geom_ribbon(mapping = aes(x = Date_of_Reporting, ymin = 0, ymax = New_Local_Cases_Revised_CUMSUM), 
    #            alpha = 0.05, linetype = 0, fill = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
    
    # Show daily total
    geom_bar(stat = 'identity') +
    
    #    geom_vline(xintercept = LABOR_DAY,  linetype = "dashed", color = "grey") + # Labor Day
    #    geom_text(mapping = aes(x = LABOR_DAY, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "Labor Day", hjust = 1.1, vjust = 1.5) + # , expression(italic('π')['S']), , hjust = -0.2)
    #    
    #    geom_vline(xintercept = LABOR_DAY + INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
    #    geom_text(mapping = aes(x = LABOR_DAY + INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+1si", hjust = 1.1, vjust = 1.5) +
    #    
    #    geom_vline(xintercept = LABOR_DAY + 2 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
    #    geom_text(mapping = aes(x = LABOR_DAY + 2 * INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+2si", hjust = 1.1, vjust = 1.5) +
    #    
    #    geom_vline(xintercept = LABOR_DAY + 3 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
  #    geom_text(mapping = aes(x = LABOR_DAY + 3 * INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+3si", hjust = 1.1, vjust = 1.5) +
  #    
  #    geom_vline(xintercept = LABOR_DAY + 4 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
  #    geom_text(mapping = aes(x = LABOR_DAY + 4 * INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+4si", hjust = 1.1, vjust = 1.5) +
  
  #    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED, y = 200),
  #              color = brewer.pal(9, 'Reds')[8], label = "Cumulative", hjust = 0) + # , vjust = 1
  
    geom_text(mapping = aes(x = as.Date("2021-05-19"), y = 2200),
              color = brewer.pal(9, 'Greys')[5], label = paste0("Total"), hjust = 1) + # , vjust = 1  # "Cumulative", color = brewer.pal(9, 'Reds')[8]
    
    geom_text(mapping = aes(x = timenow - 5, y = max(case_data$New_Local_Cases_Revised, na.rm = TRUE)),
              color = brewer.pal(9, 'Greys')[7], label = "Daily", hjust = 0, vjust = -0.5) + # , vjust = 1
    
    #ggtitle("Local Cases in Taiwan Since April 23") +
    #geom_text(mapping = aes(x = mean(range(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)$Date_of_Reporting)), y = Inf),
    #         label = "Taiwan Local Cases\n(May 1-25)", vjust = 1.25, size = 4.25) +
    ggtitle(label = "Taiwan Cumulative Local COVID-19 Cases", subtitle = todaystring) +
    
    #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
          legend.position = 'none',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          #axis.text.x = element_text(size = 7),
          #axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          strip.background = element_blank()) +
    xlab("") + ylab("Cases") +
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0, 0)),
                 limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
    scale_y_continuous(expand = expand_scale(mult = c(0, 0)))) #
#      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)),
#                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cumSumRevised_PLOT_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/localCases_cumSum_PLOT_wLINE_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
localCases_cumSum_PLOT
dev.off()


#View(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED))

filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)$New_Local_Cases / filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)$Total



###############################################################################
# PLOT 4 - showing revised values - REVISED VALUE NOT RECOMMENDED, ARTIFACTUAL DROP
# Taiwan blue: #5B9CD6
# Taiwan orange: #EE7E32
(localCases_cumSumRevised_PLOT <- ggplot(data = filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED), mapping = aes(x = Date_of_Reporting, y = New_Local_Cases_Original)) + # color = significant
    
    # Show cumulative with bars?
    geom_bar(mapping = aes(y = New_Local_Cases_Revised_CUMSUM), fill = 'lightgrey', stat = "identity", color = "NA") + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8]
    
    # Show cumulative with line?
    geom_line(mapping = aes(y = New_Local_Cases_Revised_CUMSUM), color = brewer.pal(9, 'Greys')[5]) + # brewer.pal(9, 'Reds')[8]
    
    #    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED, 
    #                            y = 200),
    #              color = brewer.pal(9, 'Reds')[8], label = "Prop tests positive (7-day window)", hjust = 0) + # , vjust = 1
    #geom_ribbon(mapping = aes(x = Date_of_Reporting, ymin = 0, ymax = New_Local_Cases_Revised_CUMSUM), 
    #            alpha = 0.05, linetype = 0, fill = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
    
    # Show REVISED with bars
    geom_bar(mapping = aes(y = New_Local_Cases_Revised), fill = '#EE7E32', stat = "identity", color = "NA") + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8]
    
    
    # Show daily total
    geom_bar(stat = 'identity', fill = "#5B9CD6", color = "NA") +
    
    #    geom_vline(xintercept = LABOR_DAY,  linetype = "dashed", color = "grey") + # Labor Day
    #    geom_text(mapping = aes(x = LABOR_DAY, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "Labor Day", hjust = 1.1, vjust = 1.5) + # , expression(italic('π')['S']), , hjust = -0.2)
    #    
    #    geom_vline(xintercept = LABOR_DAY + INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
    #    geom_text(mapping = aes(x = LABOR_DAY + INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+1si", hjust = 1.1, vjust = 1.5) +
    #    
    #    geom_vline(xintercept = LABOR_DAY + 2 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
    #    geom_text(mapping = aes(x = LABOR_DAY + 2 * INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+2si", hjust = 1.1, vjust = 1.5) +
    #    
    #    geom_vline(xintercept = LABOR_DAY + 3 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
  #    geom_text(mapping = aes(x = LABOR_DAY + 3 * INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+3si", hjust = 1.1, vjust = 1.5) +
  #    
  #    geom_vline(xintercept = LABOR_DAY + 4 * INCUBATION_TIME,  linetype = "dashed", color = "grey") + # Labor Day
  #    geom_text(mapping = aes(x = LABOR_DAY + 4 * INCUBATION_TIME, y = max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)), color = 'darkgrey', label = "+4si", hjust = 1.1, vjust = 1.5) +
  
  #    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED, y = 200),
  #              color = brewer.pal(9, 'Reds')[8], label = "Cumulative", hjust = 0) + # , vjust = 1
  
  geom_text(mapping = aes(x = as.Date("2021-05-19"), y = 2400),
            color = brewer.pal(9, 'Greys')[5], label = paste0("Total"), hjust = 1) + # , vjust = 1  # "Cumulative", color = brewer.pal(9, 'Reds')[8]
    
    geom_text(mapping = aes(x = timenow - 5, y = max(case_data$New_Local_Cases_Revised, na.rm = TRUE)),
              color = brewer.pal(9, 'Greys')[7], label = "Daily", hjust = 0, vjust = -0.5) + # , vjust = 1
    
    #ggtitle("Local Cases in Taiwan Since April 23") +
    #geom_text(mapping = aes(x = mean(range(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)$Date_of_Reporting)), y = Inf),
    #          label = "Taiwan Local Cases\n(May 1-25)", vjust = 1.25, size = 4.25) +
    ggtitle(label = "Taiwan Cumulative Local COVID-19 Cases", subtitle = todaystring) +
    
    #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
          legend.position = 'none',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          #axis.text.x = element_text(size = 7),
          #axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          strip.background = element_blank()) +
    xlab("") + ylab("Cases") +
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0, 0)),
                 limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
    scale_y_continuous(expand = expand_scale(mult = c(0, 0)))) #
#      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)),
#                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)

# SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/localCases_cumSumRevised_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cumSumRevisedLINE_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
localCases_cumSumRevised_PLOT
dev.off()




###############################################################################
# PLOT 5 - original TOTALS as bars, their sliding window as line

#DAILY_PROP_POS_DISPLAY_FACTOR <- 1/10 * max(case_data$New_Local_Cases, na.rm = TRUE) / max(case_data$day_prop_tests_positive_CIhigh, na.rm = TRUE) # get it to equal max cases
(DAILY_PROP_POS_DISPLAY_FACTOR <- max(case_data$New_Local_Cases, na.rm = TRUE)) # get it to equal max cases
(localCases_originalTotalsWindow_PLOT <- ggplot(data = filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED), 
                                                mapping = aes(x = sw_end_date, y = sw_new_local_cases_mean)) + # color = significant
    
   # LEVEL 3 ALERT Taipei
   geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 555, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + #, size = 0.4) + # brewer.pal(9, "Set1")[2]
   geom_text(x = LEVEL3_DAY, y = 555, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
   # LEVEL 3 ALERT COUNTRYWIDE
   geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 630, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + # , size = 0.4) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY_COUNTRY, y = 630, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2) + #, size = 2.75) +
   
    # RED COLORS: lighter bars - Show daily reported with bars (& text?)
    geom_bar(mapping = aes(x = Date_of_Reporting, y = New_Local_Cases), fill = "#F8C9CB", stat = "identity", color = "NA") + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8] #, alpha = 0.75  brewer.pal(9, 'Greys')[3]
    geom_text(mapping = aes(x = Date_of_Reporting, y = New_Local_Cases, label = New_Local_Cases), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.375, size = 1.5) +
    
   ## GRAY COLORS 1: lighter bars - Show daily reported with bars (& text?)
   #geom_bar(mapping = aes(x = Date_of_Reporting, y = New_Local_Cases), fill = "#E0E0E0", stat = "identity", color = "NA") + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8] #, alpha = 0.75  brewer.pal(9, 'Greys')[3]
   #geom_text(mapping = aes(x = Date_of_Reporting, y = New_Local_Cases, label = New_Local_Cases), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.375, size = 1.5) +
   
   # GRAY COLORS 2: darker bars - Show daily reported with bars (& text?)
   #geom_bar(mapping = aes(x = Date_of_Reporting, y = New_Local_Cases), fill = brewer.pal(9, 'Greys')[5], stat = "identity", color = "NA", alpha = 0.75) + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8]
   #geom_text(data = filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED, New_Local_Cases > 100), 
   #          mapping = aes(x = Date_of_Reporting, y = New_Local_Cases, label = New_Local_Cases), color = "white", hjust = 0.5, vjust = 1.5, size = 1.5) +
   
   # Show 7-day window with line and error
   geom_line(color = brewer.pal(9, 'Reds')[7]) + # brewer.pal(9, 'Reds')[7]) + # brewer.pal(9, 'Reds')[8]
   #geom_ribbon(mapping = aes(ymin = sw_new_local_cases_mean - sw_new_local_cases_SE, ymax = sw_new_local_cases_mean + sw_new_local_cases_SE), 
   #            alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
   
   # Show TODAY'S WINDOW VALUE
   geom_point(data = filter(case_data, sw_end_date == timenow), color = brewer.pal(9, 'Reds')[8]) + 
   geom_text(data = filter(case_data, sw_end_date == timenow), 
             mapping = aes(label = round(x = sw_new_local_cases_mean, digits = 0)), color = 'black', fontface = "bold", hjust = -0.3, size = 3.4) + # , hjust = -1
   
   # Show DAILY positive rates
   geom_line(mapping = aes(x = Date_of_Reporting, y = DAILY_PROP_POS_DISPLAY_FACTOR * day_prop_tests_positive), color = brewer.pal(9, 'Greys')[5], size = 0.25) + # brewer.pal(9, 'Reds')[8]
   #geom_ribbon(mapping = aes(x = Date_of_Reporting, 
   #                          ymin = DAILY_PROP_POS_DISPLAY_FACTOR * day_prop_tests_positive_CIlow, 
   #                          ymax = DAILY_PROP_POS_DISPLAY_FACTOR * day_prop_tests_positive_CIhigh), 
   #            alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Greys')[8]) + # brewer.pal(9, 'Set1')[1]) +
    geom_point(mapping = aes(x = Date_of_Reporting, y = DAILY_PROP_POS_DISPLAY_FACTOR * day_prop_tests_positive), 
               size = 0.5, color = brewer.pal(9, 'Greys')[7]) + 
    geom_text(mapping = aes(x = Date_of_Reporting, y = DAILY_PROP_POS_DISPLAY_FACTOR * day_prop_tests_positive, 
                            label = percent(day_prop_tests_positive, accuracy = 1)), color = brewer.pal(9, 'Greys')[7], 
              hjust = 0.35, vjust = -1, size = 1.5) +
    
   # TITLE
   ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   Reported   |   ", todaystring)) + #, " / First-Day (Non-Backlogged) Values")) +
   
   #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
   #theme_classic() +
   #theme_bw() +
   
   # Custom axis labels
   geom_text(x = -Inf, y = 600, label = "600 daily cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 400, label = "400", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 200, label = "200", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   
   geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
   theme(#panel.grid = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
     axis.ticks.y = element_blank(),
     #axis.line = element_blank(),
         plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
         legend.position = 'none',
         legend.title = element_blank(),
         plot.title = element_text(size = 10, face = "bold"), #, size = 12),hjust = 0.5, 
         plot.subtitle = element_text(hjust = 0.5, size = 9), #, size = 10),
         #axis.text.x = element_text(size = 7),
         #axis.text.x = element_blank(),
     axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6]),
         axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
     axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
         axis.title.y = element_blank(), # element_text(size = 9),
         strip.background = element_blank()) +
   xlab("") + ylab("Reported cases") +
   scale_x_date(labels = date_format("%b %d"),
                expand = expand_scale(mult = c(0, 0.04)),
                limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
                breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
   #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
   #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
   scale_y_continuous(limits = c(0, 730),
                      breaks = c(200, 400, 600), expand = expand_scale(mult = c(0, 0.05)))) #
#      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)),
#                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_reported_7dayWindow_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
print(localCases_originalTotalsWindow_PLOT)
dev.off()

## Try to get another font working
#cairo_pdf(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_reported_7dayWindow_", timenow, ".pdf"), width = 5.5, height = 3.2)
#localCases_originalTotalsWindow_PLOT
#dev.off()
#
#pdf(file = paste0("~/Desktop/Taiwan_COVID_data/cases_reported_7dayWindow2_", timenow, ".pdf"), width = 5.5, height = 3.2)
#print(localCases_originalTotalsWindow_PLOT)
#dev.off()
#
#ggsave(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_reported_7dayWindow_GGSAVE_", timenow, ".png"), 
#       plot = localCases_originalTotalsWindow_PLOT, device = "png", width = 5.5, height = 3.2, units = 'in', dpi = 500, limitsize = TRUE)
#
#ggsave(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_reported_7dayWindow_GGSAVE_", timenow, ".pdf"), 
#       plot = localCases_originalTotalsWindow_PLOT, device = "pdf", width = 5.5, height = 3.2, units = 'in', dpi = 500, limitsize = TRUE)


###############################################################################
# PLOT 6 - showing revised values BY DAY
# Taiwan CECC blue: #5B9CD6
# Taiwan CECC orange: #EE7E32
# Taiwan CECC orange shades: 
# step by 2: EE7E32 F19455 F4AB7B F7C3A1 FADBC6
# step by 3: EE7E32 F3A068 F7C3A1 FCE7D9 
# start darker, step by 3: ED701D F19455 F6B78E FADBC6
# start darker, step by 2:
# start darkerx4, step by 3: BD550F ED701D F19455 F6B78E FADBC8


### MANUALLY ADD BARS EACH DAY
(localCases_dateRevised_PLOT <- ggplot(data = filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED), mapping = aes(x = Date_of_Reporting, y = New_Local_Cases_Original)) + # color = significant
   
   # Show cumulative with bars?
   #geom_bar(mapping = aes(y = New_Local_Cases_Revised_CUMSUM), fill = 'lightgrey', stat = "identity", color = "NA") + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8]
   
   # Show cumulative with line?
   #geom_line(mapping = aes(y = New_Local_Cases_Revised_CUMSUM), color = brewer.pal(9, 'Greys')[5]) + # brewer.pal(9, 'Reds')[8]
   
   #    geom_text(mapping = aes(x = MIN_DATE_DISPLAYED, 
   #                            y = 200),
   #              color = brewer.pal(9, 'Reds')[8], label = "Prop tests positive (7-day window)", hjust = 0) + # , vjust = 1
   #geom_ribbon(mapping = aes(x = Date_of_Reporting, ymin = 0, ymax = New_Local_Cases_Revised_CUMSUM), 
 #            alpha = 0.05, linetype = 0, fill = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
 
 # BARS BY DATE, DAILY
 # ORANGES, 
 # Taiwan CECC Orange: #EE7E32
 #step by 2: EE7E32 F19455 F4AB7B F7C3A1 FADBC6
 #start darkerx2, step by 2: E26612 EE7E32 F19455 F4AB7B F7C3A1
 #start darkerx3, step by 3: D05D11 EE7E32 F3A068 F7C3A1 FCE7D9 -- used on 5/26
 #start darkerx3, step by 2: D05D11 ED701D F08842 F3A068 F6B78E F9CFB4 -- for 5/27***
 #start darkerx4, step by 2: 97440C BD550F E26612 EE7E32 F19455 F4AB7B F7C3A1 FADBC6-- for 5/28
 #start darkerx4, step by 3: BD550F ED701D F19455 F6B78E FADBC6
 
 # FINAL NUMBER OF COLORS:
 # blue + 9 oranges: 
 # brewer.pal(9, "Greys")[2], FCE7D9 F7C3A1 F3A068 EE7E32 D05D11 97440C 5E2A08 261103
 
 # Show REVISED (TODAY, 6/6)
 geom_bar(mapping = aes(y = New_Local_Cases_Revised), fill = '#261103', stat = "identity", color = "NA") + # 
   
   ### MOVE DOWN AND ADD HERE ###
   # Show REVISED (6/5)
   geom_bar(mapping = aes(y = New_Local_Cases_20210605), fill = '#5E2A08', stat = "identity", color = "NA") + # 
   ################
   
 # Show REVISED (6/4)
 geom_bar(mapping = aes(y = New_Local_Cases_20210604), fill = '#97440C', stat = "identity", color = "NA") + # 
   
 # Show REVISED (6/3)
 geom_bar(mapping = aes(y = New_Local_Cases_20210603), fill = '#D05D11', stat = "identity", color = "NA") + # 
   
 # Show REVISED (6/2)
 geom_bar(mapping = aes(y = New_Local_Cases_20210602), fill = '#EE7E32', stat = "identity", color = "NA") + # 
   
 # Show REVISED (6/1)
 geom_bar(mapping = aes(y = New_Local_Cases_20210601), fill = '#F3A068', stat = "identity", color = "NA") + # 
   
 # Show REVISED (5/31)
 geom_bar(mapping = aes(y = New_Local_Cases_20210531), fill = '#F7C3A1', stat = "identity", color = "NA") + # 
   
 #Show REVISED (5/30)
 geom_bar(mapping = aes(y = New_Local_Cases_20210530), fill = '#FCE7D9', stat = "identity", color = "NA") + # 
   
 # Show REVISED (5/29)
 geom_bar(mapping = aes(y = New_Local_Cases_20210529), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") + # 
   
 # Show REVISED (5/28)
 geom_bar(mapping = aes(y = New_Local_Cases_20210528), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") + # 
   
 # Show REVISED (5/27)
 geom_bar(mapping = aes(y = New_Local_Cases_20210527), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") + # 
   
   # Show REVISED (5/26)
   geom_bar(mapping = aes(y = New_Local_Cases_20210526), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") + # 
   
   # Show REVISED (5/25)
   geom_bar(mapping = aes(y = New_Local_Cases_20210525), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") +
   
   # Show REVISED (5/24)
   geom_bar(mapping = aes(y = New_Local_Cases_20210524), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") +
   
   # Show REVISED (5/23)
   geom_bar(mapping = aes(y = New_Local_Cases_20210523), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") +
   
   # Show REVISED (5/22)
   geom_bar(mapping = aes(y = New_Local_Cases_20210522), fill = brewer.pal(9, "Greys")[2], stat = "identity", color = "NA") +
   
   # Show ORIGINAL
   geom_bar(stat = 'identity', fill = "#5B9CD6", color = "NA") +
   
   # Show ORIGINAL with line?
   #geom_line(color = "#215583") + # brewer.pal(9, 'Reds')[8]
   
   #geom_text(mapping = aes(x = timenow - 5, y = max(case_data$New_Local_Cases_Revised, na.rm = TRUE)),
   #          color = brewer.pal(9, 'Greys')[7], label = "Daily", hjust = 0, vjust = -0.5) + # , vjust = 1
   
   #ggtitle("Local Cases in Taiwan Since April 23") +
   #geom_text(mapping = aes(x = mean(range(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)$Date_of_Reporting)), y = Inf),
   #          label = "Taiwan Local Cases\n(May 1-25)", vjust = 1.25, size = 4.25) +
   ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   ", todaystring)) + # vjust = 1.25, , size = 4.25
   
   #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
   #theme_classic() +
   
   # Custom axis labels
   geom_text(x = -Inf, y = 500, label = "500 daily cases assigned", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 400, label = "400", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 300, label = "300", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 200, label = "200", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 100, label = "100", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   
   geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
   theme(#panel.grid = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
     axis.ticks.y = element_blank(),
     #axis.line = element_blank(),
         plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
         plot.title = element_text(face = "bold", size = 10), # hjust = 0.5, 
         #plot.subtitle = element_text(hjust = 0.5),
         legend.position = 'none',
         legend.title = element_blank(),
     axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6]),
     axis.text.y =element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
     axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
     axis.title.y = element_blank(), #element_text(size = 9),
     strip.background = element_blank()) +
   xlab("") + ylab("Daily cases") +
   scale_x_date(labels = date_format("%b %d"),
                expand = expand_scale(mult = c(0, 0)),
                limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
                breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
   #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
   #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
   scale_y_continuous(breaks = c(seq(100, 500, 100)), expand = expand_scale(mult = c(0, 0.05)))) #
#      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)),
#                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)
# MISSING VALUES OK -- days from which none assigned


# SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/localCases_cumSumRevised_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_by_date_added_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
#localCases_dateRevised_PLOT
#dev.off()



###############################################################################
# Now transpose add implement suggestion from Alexis:
# "Can you also make a chart where the colors match for "added with 1 day delay" "added with 2 day delay" etc?"
time0 # "2020-01-15"
timenow # changes, e.g., "2021-05-26"
case_data$day # 1 .. 498 and counting

# initialize a new column identical to New_Local_Cases_Revised but using the date instead
timenow_col <- paste0("New_Local_Cases_", timenow)
timenow_col <- str_replace_all(string = timenow_col, pattern = '-', replacement = '')
case_data[, timenow_col] <- case_data$New_Local_Cases_Revised

# initialize columns up to 15 NOW 17 days late
MAX_DAYS_LATE <- 20
num_delayed_column_names <- character()
sum_after_delay_column_names <- character()
for(i in 0:MAX_DAYS_LATE) {
  #i <- 0
  #cat(i, "")
  num_delayed_name <- paste0("num_delayed_", i)
  case_data[, num_delayed_name] <- NA
  num_delayed_column_names <- c(num_delayed_column_names, num_delayed_name)
  
  sum_after_delay_name <- paste0("sum_after_delay_", i)
  case_data[, sum_after_delay_name] <- NA
  sum_after_delay_column_names <- c(sum_after_delay_column_names, sum_after_delay_name)
  
}

# INITIALIZE sum_after_delay_* columns to original day's value
for(this_later_days_late in 0:MAX_DAYS_LATE) {
  this_later_sum_after_delay_col <- paste0("sum_after_delay_", this_later_days_late)
  case_data[, this_later_sum_after_delay_col] <- case_data[, "New_Local_Cases_Original"]
}

# Prepare to populate the new delay columns
DATE_LOOKUP_TABLE <- case_data$Date_of_Reporting
names(DATE_LOOKUP_TABLE) <- case_data$day

# initialize vector of the actual days late observed
days_late_observed <- c("0")

for(this_day in case_data$day) { # (this_date in case_data$Date_of_Reporting) { # }
  #this_day <- 494
  #this_day <- 489
  #cat(as.character(this_day), "")
  
  this_date <- as.Date(unname(DATE_LOOKUP_TABLE[this_day])) # e.g., 494 gives [1] "2021-05-22"
  #cat(as.character(this_date), "")
  #typeof(this_date) # double
  #sum(case_data$Date_of_Reporting == this_date) # 1
  
  for(this_days_late in 0:MAX_DAYS_LATE) {
    #cat(as.character(this_days_late), "")
    #this_days_late <- 1
    #this_days_late <- 8
    #this_days_late <- 9
    this_days_late_date <- this_date + this_days_late
    this_days_late_date_col <- paste0("New_Local_Cases_", this_days_late_date)
    this_days_late_date_col <- str_replace_all(string = this_days_late_date_col, pattern = '-', replacement = '')
    
    if(this_days_late_date_col %in% names(case_data)) { #  && ! all(is.na(case_data[, this_days_late_date_col]))
      # add new total after delay
      this_sum_after_delay_col <- paste0("sum_after_delay_", this_days_late)
      this_sum_after_delay <- as.integer(case_data[this_day, this_days_late_date_col]) # really simple
      case_data[this_day, this_sum_after_delay_col] <- this_sum_after_delay
      
      # update REMAINING delay columns (only later days) with this number
      #for(this_later_days_late in (this_days_late + 1):MAX_DAYS_LATE) {
      #  #this_later_days_late <- 9
      #  this_later_days_late_date <- this_date + this_later_days_late
      #  this_later_days_late_date_col <- paste0("New_Local_Cases_", this_later_days_late_date)
      #  this_later_days_late_date_col <- str_replace_all(string = this_later_days_late_date_col, pattern = '-', replacement = '')
      #  case_data[this_day, this_sum_after_delay_col]
      #}
      
      # update REMAINING delay columns (only later days) with this number
      for(this_later_days_late in (this_days_late + 1):MAX_DAYS_LATE) {
        #this_later_days_late <- 9
        this_later_sum_after_delay_col <- paste0("sum_after_delay_", this_later_days_late)
        case_data[this_day, this_later_sum_after_delay_col] <- this_sum_after_delay
      }
      
      # sum known DAY BEFORE, if exists
      this_day_before_date <- this_days_late_date - 1
      this_day_before_date_col <- paste0("New_Local_Cases_", this_day_before_date)
      this_day_before_date_col <- str_replace_all(string = this_day_before_date_col, pattern = '-', replacement = '')
      
      this_prev_day_sum <- NA
      if(this_day_before_date_col %in% names(case_data)) {
        this_prev_day_sum <- as.integer(case_data[this_day, this_day_before_date_col])
      } else {
        this_prev_day_sum <- as.integer(case_data[this_day, "New_Local_Cases_Original"])
      }
      
      # add to list of days late observed, if there was a delay
      if(! is.na(this_prev_day_sum) && this_sum_after_delay != this_prev_day_sum) {
        days_late_observed <- c(days_late_observed, this_days_late)
      }
      
      # add num delayed for exaclty this length of time by subtracting previous day's sum
      this_num_delayed_col <- paste0("num_delayed_", this_days_late)
      this_num_delayed <- as.integer(this_sum_after_delay - this_prev_day_sum) #as.integer(case_data[this_day, this_days_late_date_col] - this_prev_day_sum) # case_data[this_day, "New_Local_Cases_Original"])
      case_data[this_day, this_num_delayed_col] <- this_num_delayed
      
    }
  }
}

View(case_data)


#View(dplyr::select(case_data, 
#                   Date_of_Reporting, New_Local_Cases_Original, 
#                   New_Local_Cases_20210522, New_Local_Cases_20210523, New_Local_Cases_20210524, New_Local_Cases_20210525, 
#                   New_Local_Cases_Revised, 
#                   everything()))



# SAVE RESULTS
write_tsv(case_data, 
          paste0("~/Desktop/Taiwan_COVID_data/results_", timenow, ".tsv"))





###############################################################################
# PLOT 7 - showing revised values BY NUM DAYS DELAYED

# pivot to better format
(case_data_forDelayLONG <- dplyr::select(case_data, Date_of_Reporting, New_Local_Cases_Original, sum_after_delay_column_names) %>%
  pivot_longer(cols = sum_after_delay_column_names, names_to = "days_delayed", values_to = "sum_after_delay"))

# Format as integer
(case_data_forDelayLONG$days_delayed <- sort(as.integer(unique(str_replace(string = case_data_forDelayLONG$days_delayed, pattern = "sum_after_delay_", replacement = "")))))
(days_late_observed <- sort(as.integer(unique(days_late_observed)))) # 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 20

# filter out if the number of days actually wasn't observed
case_data_forDelayLONG <- filter(case_data_forDelayLONG, days_delayed %in% days_late_observed) # 5,478 x 4
unique(case_data_forDelayLONG$days_delayed) # [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17

case_data_forDelayLONG$days_delayed <- factor(x = case_data_forDelayLONG$days_delayed, 
                                              levels = days_late_observed,
                                              labels = c("On time", c(1:17, 20))) #0:MAX_DAYS_LATE)

levels(case_data_forDelayLONG$days_delayed)
# view
#View(dplyr::select(case_data, Date_of_Reporting, New_Local_Cases_Original, sum_after_delay_column_names))

## PLOT
#(localCases_daysDelayed_PLOT <- ggplot(data = filter(case_data_forDelayLONG, Date_of_Reporting >= MIN_DATE_DISPLAYED), 
#                                       mapping = aes(x = Date_of_Reporting, y = as.integer(sum_after_delay), 
#                                                     fill = as.factor(days_delayed))) + # color = significant
#    
#    # bar
#    geom_bar(stat = "identity", color = "NA", position = position_dodge(width = 0), width = 10) + # 
#    
#    ggtitle(label = "Taiwan Daily Local COVID-19 Cases", subtitle = todaystring) + # , " # vjust = 1.25, , size = 4.25
#    
#    #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
#    theme_classic() +
#    theme(panel.grid = element_blank(),
#          plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
#          plot.title = element_text(hjust = 0.5),
#          plot.subtitle = element_text(hjust = 0.5),
#          legend.position = c(0.25, 0.525), # 'none',
#          legend.key.size = unit(x = 0.75, units = 'line'), 
#          legend.title = element_text(size = 8), #legend.title = element_blank(),
#          legend.text = element_text(size = 7), #legend.title = element_blank(),
#          #axis.text.x = element_text(size = 7),
#          #axis.text.x = element_blank(),
#          axis.text.y = element_text(size = 9),
#          axis.title.y = element_text(size = 9),
#          strip.background = element_blank()) +
#    xlab("") + ylab("Cases") +
#    scale_x_date(labels = date_format("%b %d"),
#                 expand = expand_scale(mult = c(0, 0)),
#                 limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
#                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
#    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
#    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
#    scale_y_continuous(expand = expand_scale(mult = c(0, 0))) +
#    scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
#                      name = "Days late", guide = guide_legend(reverse = TRUE)))  #
#    #scale_fill_manual(values = c(brewer.pal(9, "PuBu")[c(8, 7, 6, 5)], brewer.pal(9, "BuPu")[c(5, 6)], brewer.pal(9, "OrRd")[c(5, 6, 7, 8, 9)]), name = "Days late"))  #
#    #scale_fill_manual(values = rev(c(rev(brewer.pal(7, "OrRd")), brewer.pal(4, "PuBu"))), name = "Days late")) 
#    #scale_fill_manual(values = rev(brewer.pal(11, "RdYlBu")), name = "Days late")) # #5B9CD6", 
##      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)),
##                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)
#
##display.brewer.all()
#
## SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/localCases_daysDelayed_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
#localCases_daysDelayed_PLOT
#dev.off()




###############################################################################
### TIME LAPSE
date_chart_start <- as.Date("2021-05-1")
date_range_start <- as.Date("2021-05-13")
date_range_end <- timenow

#LOCK_PNG <- png::readPNG("/Users/cwnelson88/Desktop/Taiwan_COVID_data/LOCK.png")
##LOCK_PNG <- readPNG(system.file("img", "/Users/cwnelson88/Desktop/Taiwan_COVID_data/LOCK.png", package="png"))

# which provides a wider than tall rectangle
#asp_ratio <- 1.618 
#then, in theme: aspect.ratio = 1 / asp_ratio,

# defined directory for saving charts
chart_dir <- "~/Desktop/Taiwan_COVID_data/TIME_LAPSE/"
MAX_CURR_REVISED <- max(case_data_forDelayLONG$sum_after_delay, na.rm = TRUE)

# Loop
this_curr_date <- date_range_start
while (this_curr_date <= date_range_end) {
  # this_curr_date begins at "2021-05-10"
  #this_curr_date <- as.Date("2021-05-22")
  #this_curr_date <- as.Date("2021-05-24")
  #this_curr_date <- as.Date("2021-05-26")
  #this_curr_date <- as.Date("2021-06-05")
  this_curr_date_data <- filter(case_data_forDelayLONG, Date_of_Reporting >= date_chart_start, Date_of_Reporting <= this_curr_date)
  
  # Determine (1) day 1 and (2) latest
  this_curr_date_data_CURR_DAY1 <- this_curr_date_data %>% 
    group_by(Date_of_Reporting) %>% 
    summarise(
      sum_day1 = min(sum_after_delay)
    )
  
  # create filtering variables
  this_curr_date_data$days_to_curr_date <- as.integer(this_curr_date - this_curr_date_data$Date_of_Reporting)
  this_curr_date_data$days_delayed_int <- as.character(this_curr_date_data$days_delayed)
  this_curr_date_data[this_curr_date_data$days_delayed_int == "On time", ]$days_delayed_int <- "0"
  this_curr_date_data$days_delayed_int <- as.integer(this_curr_date_data$days_delayed_int)
  
  # keep only if number of days delayed is equal or less than days to present
  #this_curr_date_data <- filter(this_curr_date_data, days_delayed_int < days_to_curr_date)
  this_curr_date_data[this_curr_date_data$days_delayed_int > this_curr_date_data$days_to_curr_date, ]$sum_after_delay <- 0
  
  this_curr_date_filename <- paste0(chart_dir, "time_lapse_created", timenow, "_", this_curr_date, ".png")
  
  # ADJUST DAYS LATE ACCORDING TO PRESENT
  #this_curr_date_data$days_delayed <- this_curr_date_data$days_delayed
  
  this_curr_date_data_CURR_MAX <- this_curr_date_data %>% 
    group_by(Date_of_Reporting) %>% 
    summarise(
      sum_after_delay_max = max(sum_after_delay)
    )
  
  # new todaystring
  this_curr_todaystring <- as.character(format(x = this_curr_date, format = "%b %d"))
  
  # number added for today
  today_count <- case_data[case_data$Date_of_Reporting == this_curr_date, ]$New_Local_Cases_Original
  
  # number added to backlogged dates
  backlog_count <- case_data[case_data$Date_of_Reporting == this_curr_date, ]$Num_Delayed
  if(is.na(backlog_count)) {
    backlog_count <- 0
  }
  
  # total number today
  #total_count <- case_data[case_data$Date_of_Reporting == this_curr_date, ]$New_Local_Cases
  total_count <- today_count + backlog_count
  #if(total_count != (today_count + backlog_count)) {
  #  cat("BIG PROBLEM ON", as.character(this_curr_date), "\n")
  #  cat("Local total is", total_count, "or", today_count + backlog_count, "\n\n")
  #}
  
  #FACTOR for only 11 categories, with days 10-12 pooled <-- CHANGETHIS
  this_curr_date_data$days_delayed <- as.character(this_curr_date_data$days_delayed)
  this_curr_date_data[this_curr_date_data$days_delayed %in% c('10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'), ]$days_delayed <- "10-20"
  this_curr_date_data$days_delayed <- factor(x = this_curr_date_data$days_delayed,
                                             levels = c("On time", 1:9, "10-20"))
  
  ### PLOT ###
  (localCases_daysDelayed_TIMELAPSE_PLOT <- ggplot(data = this_curr_date_data, 
                                                   mapping = aes(x = Date_of_Reporting, y = as.integer(sum_after_delay),  fill = as.factor(days_delayed))) +
      
      # LOCKED-IN bars
      #geom_bar(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = 'black', 
      #         fill = brewer.pal(9, "Reds")[9], inherit.aes = FALSE, stat = 'identity') + #, size = 1.0
      #geom_text(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = 'black', 
      #         inherit.aes = FALSE, label = sprintf('\u1f512')) + #, size = 1.0
      #
    
      # Latest revised sums
      geom_bar(stat = "identity", color = "NA", position = position_dodge(width = 0), width = 10) + # 
      #geom_text(aes(label = sum_after_delay), vjust = -0.2, size = rel(2)) +
      
      # Date
      geom_rect(xmin = as.Date("2021-05-04"), xmax = as.Date("2021-05-12"), ymin = 325, ymax = 425, fill = "#FFE033") +
      geom_text(x = as.Date("2021-05-08"), y = 375, label = "FINAL", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
      
      # Attribution
      geom_text(x = as.Date("2021-05-04"), y = 300, color = brewer.pal(9, "Greys")[4], hjust = 0, vjust = 0.5, size = 1.4, # fontface = "italic", family = 'serif',
                label = "Data: Taiwan CECC, MOHW\nAuthors: Chase W. Nelson & Mitch Lin") +
      
      # Counter for number added
      #geom_rect(xmin = as.Date("2021-05-07"), xmax = as.Date("2021-05-13"), ymin = 50, ymax = 250, fill = "white", color = "black") +
      #geom_text(x = as.Date("2021-05-10"), y = 200, label = "100", fontface = "bold", color = brewer.pal(9, "Reds")[8], hjust = 1.2, vjust = 0.5, size = 5) + #, size = 2.75) +
      #geom_text(x = as.Date("2021-05-10"), y = 200, label = "150", fontface = "bold", color = "#5B9CD6", hjust = -0.2, vjust = 0.5, size = 5) + #, size = 2.75) +
      #geom_text(x = as.Date("2021-05-10"), y = 200, label = "+", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5) + #, size = 2.75) +
      #geom_text(x = as.Date("2021-05-10"), y = 100, label = "=250", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
      geom_text(x = as.Date("2021-05-04"), y = 250-50, label = paste0("Total: ", total_count), fontface = "bold", color = brewer.pal(9, "Greys")[6], hjust = 0, vjust = 0.5, size = 4.75) + #, size = 2.75) +
      geom_text(x = as.Date("2021-05-04"), y = 205-50, label = paste0("Backlogged: ", backlog_count), fontface = "bold", color = brewer.pal(9, "Reds")[4], hjust = 0, vjust = 0.5, size = 2.6) + #, size = 2.75) +
      geom_text(x = as.Date("2021-05-04"), y = 175-50, label = paste0("Today: ", today_count), fontface = "bold", color = brewer.pal(9, "Blues")[5], hjust = 0, vjust = 0.5, size = 2.6) + # "#5B9CD6"
      
      ggtitle(label = "2021 Taiwan local COVID-19 cases") + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
      
      # LOCKED dates
      #geom_image(mapping = aes(image = "~/Desktop/Taiwan_COVID_data/LOCK.svg"), size = 1) +
      #annotation_custom(grob = rasterGrob(image = LOCK_PNG, width = 200, height = 200, -Inf, Inf, -Inf, Inf), 
      #                  xmin = as.Date("2021-05-07"), xmax = as.Date("2021-05-12"),
      #                  ymin = 200, ymax = 400) +
      # current trendline
      #geom_line(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE, size = 1.0) +
      #geom_point(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE) +
      
      #geom_text(x = LEVEL3_DAY, y = 300, label = emoji("key"), fill = "black", color = "black", family = "EmojiOne", size = 6) + # parse = TRUE, 
    #geom_text(x = LEVEL3_DAY, y = 300, label = "XX", fill = "black", color = "black", family = "EB Garamond", size = 6) + # parse = TRUE, LOCK: "1f512" in "Segoe UI Symbol"
    geom_image(data = filter(this_curr_date_data_CURR_MAX, Date_of_Reporting >= date_range_start, Date_of_Reporting < this_curr_date - 9), 
               mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max + 13),
               image = "~/Desktop/Taiwan_COVID_data/LOCK_skinny2.png", by = "width", size = 0.017, inherit.aes = FALSE) +
      
      # LOCKED-IN bars
    #geom_bar(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = 'black', 
    #         fill = brewer.pal(9, "Reds")[9], inherit.aes = FALSE, stat = 'identity') + #, size = 1.0
    #geom_text(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = 'black', 
    #          inherit.aes = FALSE, label = parse(text = '\u1f512'), parse = TRUE) + #, size = 1.0 # <-- ENDED HERE
    
      # this may be best bet
      #search_emoji("lock")
      #geom_emoji(alias = 'lock', size = 2, x = as.Date("2021-05-15"), y = 400, color = 'black', inherit.aes = FALSE) + # fill = 'black',
      
    #geom_richtext(aes(), fill = NA, label.color = NA, label = link_to_img("~/Desktop/Taiwan_COVID_data/LOCK.png"),
    #               label.padding = grid::unit(rep(0, 4), "pt")) +
    
      #geom_text(x = as.Date("2021-05-15"), y = 400, color = 'black', family="EmojiOne", size=10) +
    
      # original trendline
      #geom_line(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE, size = 1.0) +
      #geom_point(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE) +
      
      
      
      #theme_classic() +
      # Custom axis labels
      geom_text(x = -Inf, y = 500, label = "500 daily cases assigned", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
      geom_text(x = -Inf, y = 400, label = "400", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
      geom_text(x = -Inf, y = 300, label = "300", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
      geom_text(x = -Inf, y = 200, label = "200", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
      geom_text(x = -Inf, y = 100, label = "100", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
      
      geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
      theme(#panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
        axis.ticks.y = element_blank(),
        #axis.line = element_blank(),
            plot.margin = unit(x = c(1, 1, 1, 1), units = "line"),
            plot.title = element_text(face = "bold", size = 12), # hjust = 0.5, 
            #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            legend.position = 'right',# c(0.125, 0.525), # 'none',
        legend.margin = margin(l = -0.25, unit = 'line'),
            legend.key.size = unit(x = 0.7, units = 'line'), 
            legend.title = element_text(size = 8), #legend.title = element_blank(),
            legend.text = element_text(size = 6), #legend.title = element_blank(),
            #axis.text.x = element_text(size = 7),
            #axis.text.x = element_blank(),
        axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6]),
        axis.text.y = element_blank(), #element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
        axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
        axis.title.y = element_blank(), #element_text(size = 9),
        strip.background = element_blank()) +
      xlab("") + ylab("Daily cases") +
      scale_x_date(labels = date_format("%b %d"),
                   expand = expand_scale(mult = c(0, 0)),
                   limits = c(date_chart_start - 1, date_range_end + 1),
                   breaks = seq(date_chart_start, as.Date(timenow), by = INCUBATION_TIME)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
      #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
      #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
      #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
      scale_y_continuous(limits = c(0, MAX_CURR_REVISED + 25), breaks = seq(100, 500, 100), expand = expand_scale(mult = c(0, 0))) +
      scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
                        name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))
  
  # SAVE
  png(filename = this_curr_date_filename, width = 5.5, height = 3.2, units = 'in', res = 500)
  #png(filename = paste0(this_curr_date_filename, "_FINAL.png"), width = 5.5, height = 3.2, units = 'in', res = 500)
  print(localCases_daysDelayed_TIMELAPSE_PLOT)
  dev.off()
  
  
  #ggsave(filename = paste0(this_curr_date_filename, 'lala_ggsave.png'), plot = localCases_daysDelayed_TIMELAPSE_PLOT,
  #      width = 5.5, height = 3.3, device = 'png', units = 'in', dpi = 500)
  
  # increment the day
  this_curr_date <- this_curr_date + 1
}

#####
### SAVE LATEST TODAY'S
#this_curr_date_filename <- paste0("~/Desktop/Taiwan_COVID_data/cases_by_days_late_", timenow, ".png")
#png(filename = this_curr_date_filename, width = 5.5, height = 3.2, units = 'in', res = 500)
#print(localCases_daysDelayed_TIMELAPSE_PLOT)
#dev.off()
#####

this_curr_todaystring <- as.character(format(x = timenow, format = "%b %d"))


# SAVE SOURCE
#case_data_filtered <- filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)
#
#View(case_data_filtered)
#
#write_tsv(case_data_filtered, 
#          paste0("~/Desktop/Taiwan_COVID_data/data_processed_", timenow, ".tsv"))
#
## exact counts
#(New_Local_Cases_Original_sum <- sum(case_data_filtered$New_Local_Cases_Original))
#(New_Local_Cases_Revised_sum <- sum(case_data_filtered$New_Local_Cases_Revised))
#(New_Local_Cases_BackloggedCount <- New_Local_Cases_Revised_sum - New_Local_Cases_Original_sum) # 1087
#(New_Local_Cases_BackloggedCount_Check <- sum(case_data_filtered$New_Local_Cases_Revised - case_data_filtered$New_Local_Cases_Original))
#New_Local_Cases_BackloggedCount/New_Local_Cases_Revised_sum # 0.2577662
##0.281106 on 5/26
#
## another way
#case_data_filtered$backlogged_count <- case_data_filtered$New_Local_Cases_Revised - case_data_filtered$New_Local_Cases_Original
#sum(case_data_filtered$backlogged_count) # 1087
#case_data_filtered$backlogged_prop <- case_data_filtered$backlogged_count / case_data_filtered$New_Local_Cases_Revised
#
#View(case_data_filtered)


###############################################################################
###############################################################################
### PROJECTED DAILY TOTALS
###############################################################################
###############################################################################

# Thanks to Albert Lin and Samuel Liu and g0v open data compilation: https://docs.google.com/spreadsheets/d/12tQKCRuaiBZfc9yDd6tmlOdsm62ke_4AcKmNJ6q4gdU/htmlview#

###
# CASE DATA 2 ORIGINALLY IMPORTED HERE
###

# EXTRACT from case_data
case_data_projecting <- dplyr::select(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED), 
                                      Date_of_Reporting, New_Local_Cases, Num_Delayed, New_Local_Cases_Original, New_Local_Cases_Revised, Total)

# JOIN
names(case_data_projecting)[names(case_data_projecting) == "Date_of_Reporting"] <- "date"
case_data_projecting <- left_join(x = case_data_projecting, y = case_data2, by = "date")

# INSPECT
case_data_projecting
#View(case_data_projecting)

### PROJECT NUMBER OF CASES
# options:
# (1) the day's known positive rate X the day's growth in backlog
# (2) current oustanding allocated according to mean prop assigned to each day late in last 12 days

# calculate mean proportion assigned to each day back
case_data_forDelayTimeMeans <- dplyr::select(case_data, Date_of_Reporting, New_Local_Cases_Original, sum_after_delay_column_names, num_delayed_column_names)
days_late_sums <- integer(MAX_DAYS_LATE)
(names(days_late_sums) <- 1:MAX_DAYS_LATE)

# ONLY ONCE
for(this_days_delayed in names(days_late_sums)) {
  #this_days_delayed <- 1
  #cat(this_days_delayed)
  this_days_delayed_col <- paste0("num_delayed_", this_days_delayed)
  days_late_sums[this_days_delayed] <- sum(case_data_forDelayTimeMeans[, this_days_delayed_col], na.rm = TRUE)
}

# proportions
(days_late_proportions <- days_late_sums / sum(days_late_sums))

# calculate mean number outstanding expected to be reassigned to each day back
(NUM_OUTSTANDING1 <- case_data_projecting[case_data_projecting$date == timenow, ]$not_yet_interpreted_TotalInspection_MIN_Completed)
# compare to https://sites.google.com/cdc.gov.tw/2019ncov/taiwan
# 2021/06/01: 522816-488696-8511=25609 vs. 25.1
# 2021/06/02: 551478-524385-8842=18,251 vs. 0
#2021/06/03: 578935-556055-9974=12906 vs. 
#6/5: 9444
#(NUM_OUTSTANDING2 <- 18251) # go with the cdc website instead 6/2
#(NUM_OUTSTANDING2 <- 12906) # go with the cdc website instead 6.3
#(NUM_OUTSTANDING2 <- 11203) # go with the cdc website instead 6.3 # 606921-585272-10446=11203
#(NUM_OUTSTANDING2 <- 11312) # 6/5: 636500-614742-10446=11312
#(NUM_OUTSTANDING2 <- 11050) # 6/6: 656768-634762-10956=11050
#(NUM_OUTSTANDING2 <- 10828) # 6/7: 671160-648841-11491
(NUM_OUTSTANDING2 <- 11046) # 6/8: 698373-675836-11491

# mean
(NUM_OUTSTANDING <- round(mean(c(NUM_OUTSTANDING1, NUM_OUTSTANDING2))))

# 6/5: 10378
# 6/6: 10.2k
#6/7: 9542
# 6/8: 9658
(CURR_CUM_PROP_POS <- case_data_projecting[case_data_projecting$date == timenow, ]$test_total_positive_rate_Confirmed_DIV_Completed)

(CURR_DAY_PROP_POS <- case_data_projecting[case_data_projecting$date == timenow, ]$same_day_test_positive_rate_DiagnosedOnDay_DIV_TestVolumeOnDay)
(CURR_DAY_PROP_POS <- case_data_projecting[case_data_projecting$date == timenow, ]$total_interpretation_on_day_Confirmed_PLUS_Excluded)

(NUM_OUTSTANDING_PREDICT_POS <- NUM_OUTSTANDING * CURR_CUM_PROP_POS)
(days_late_PREDICTED_ADDITIONS <- round(days_late_proportions * NUM_OUTSTANDING_PREDICT_POS))

# INITIALIZE and ADD THEM
case_data_projecting$Num_Local_Cases_Projected <- case_data_projecting$New_Local_Cases_Revised

for(this_days_late in 1:MAX_DAYS_LATE) {
  #this_days_late <- 1
  this_date <- timenow - this_days_late + 1 # new, better method
  this_PREDICTED_ADDITION <- as.integer(days_late_PREDICTED_ADDITIONS[this_days_late])
  case_data_projecting[case_data_projecting$date == this_date, ]$Num_Local_Cases_Projected <- 
    case_data_projecting[case_data_projecting$date == this_date, ]$Num_Local_Cases_Projected + this_PREDICTED_ADDITION
}

# FINALLY, set current day's prediction to previous day's prediction so as not to bias trend - better method
#case_data_projecting[case_data_projecting$date == timenow, ]$Num_Local_Cases_Projected <- 
#  case_data_projecting[case_data_projecting$date == timenow - 1, ]$Num_Local_Cases_Projected

#View(case_data_projecting)


###############################################################################
### sliding window for PREDICTED case numbers

# initialize numbered days
case_data_projecting$day <- 1:nrow(case_data_projecting)

# find minimum and maximum dates
(time0_PREDICTED <- min(case_data_projecting$date))

# initialize sliding window columns
case_data_projecting$sw_start <- NA
case_data_projecting$sw_center <- NA
case_data_projecting$sw_end <- NA
case_data_projecting$sw_num_days_with_data <- NA

case_data_projecting$sw_PREDICTED_cases_sum <- NA
case_data_projecting$sw_PREDICTED_cases_mean <- NA
case_data_projecting$sw_PREDICTED_cases_sd <- NA
case_data_projecting$sw_PREDICTED_cases_SE <- NA

# NEW: REVISED!
case_data_projecting$sw_REVISED_cases_sum <- NA
case_data_projecting$sw_REVISED_cases_mean <- NA
case_data_projecting$sw_REVISED_cases_sd <- NA
case_data_projecting$sw_REVISED_cases_SE <- NA

# perform sliding window
for(i in 1:(nrow(case_data_projecting) - WINDOW_SIZE + 1)) { # each window of time starting at day 1
  #i <- 1
  cat(paste0(i, ' '))
  
  # Extract window; analyze
  window_case_data_projecting <- case_data_projecting[i:(i + WINDOW_SIZE - 1), ] # filter(case_data_projecting, day >= i, day <= (i + WINDOW_SIZE - 1))
  lowest_day <- min(window_case_data_projecting$day)
  #highest_day <- max(window_case_data_projecting$day)
  highest_day <- lowest_day + WINDOW_SIZE - 1
  new_PREDICTED_cases_data_count <- sum(! is.na(window_case_data_projecting$Num_Local_Cases_Projected))
  new_REVISED_cases_data_count <- sum(! is.na(window_case_data_projecting$New_Local_Cases_Revised))
  #View(window_case_data)
  
  if(new_PREDICTED_cases_data_count >= MIN_DATA_COUNT) { #  && new_tests_data_count >= MIN_DATA_COUNT
    # Add results to table
    
    # PROJECTED
    sw_new_PREDICTED_cases_sum <- sum(window_case_data_projecting$Num_Local_Cases_Projected, na.rm = TRUE)
    sw_new_PREDICTED_cases_mean <- sw_new_PREDICTED_cases_sum / new_PREDICTED_cases_data_count
    sw_new_PREDICTED_cases_sd <- sd(window_case_data_projecting$Num_Local_Cases_Projected, na.rm = TRUE)
    sw_new_PREDICTED_cases_SE <- sw_new_PREDICTED_cases_sd / sqrt(new_PREDICTED_cases_data_count)
    
    # REVISED
    sw_new_REVISED_cases_sum <- sum(window_case_data_projecting$New_Local_Cases_Revised, na.rm = TRUE)
    sw_new_REVISED_cases_mean <- sw_new_REVISED_cases_sum / new_REVISED_cases_data_count
    sw_new_REVISED_cases_sd <- sd(window_case_data_projecting$New_Local_Cases_Revised, na.rm = TRUE)
    sw_new_REVISED_cases_SE <- sw_new_REVISED_cases_sd / sqrt(new_REVISED_cases_data_count)
    
    # PROJECTED
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_start <- lowest_day
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_center <- (lowest_day + highest_day) / 2
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_end <- highest_day
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_num_days_with_data <- nrow(window_case_data_projecting)
    
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_sum <- sw_new_PREDICTED_cases_sum
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_mean <- sw_new_PREDICTED_cases_mean
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_sd <- sw_new_PREDICTED_cases_sd
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_SE <- sw_new_PREDICTED_cases_SE
    
    # REVISED
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_REVISED_cases_sum <- sw_new_REVISED_cases_sum
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_REVISED_cases_mean <- sw_new_REVISED_cases_mean
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_REVISED_cases_sd <- sw_new_REVISED_cases_sd
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_REVISED_cases_SE <- sw_new_REVISED_cases_SE
    
  } # else leave it as NA
} # end last window

# format sw_center as date
case_data_projecting$sw_start_date <- as.Date((time0_PREDICTED - 1) + case_data_projecting$sw_start)
case_data_projecting$sw_center_date <- as.Date((time0_PREDICTED - 1) + case_data_projecting$sw_center)
case_data_projecting$sw_end_date <- as.Date((time0_PREDICTED - 1) + case_data_projecting$sw_end)
###############################################################################

# correct negative SD to 0
#case_data_projecting[case_data_projecting$Num_Local_Cases_Projected - case_data_projecting$sw_PREDICTED_cases_sd < 0, ]$sw_PREDICTED_cases_sd <- 
#  0


# Notes
# Taiwan orange: #EE7E32
View(case_data_projecting)



###############################################################################
### PLOT REVISED ###
(localCases_REVISED_PLOT <- ggplot(data = case_data_projecting, 
                                     mapping = aes(x = date, y = New_Local_Cases_Revised)) +
    
    # LEVEL 3 ALERT - Taipei
    #geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 555, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + #, size = 0.4) +
    #geom_text(x = LEVEL3_DAY, y = 555, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 555, linetype = "dashed", color = brewer.pal(9, "Reds")[6], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY, y = 555, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Reds")[7], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
    # LEVEL 3 ALERT - COUNTRY
    #geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 630, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + #, size = 0.4) +
    #geom_text(x = LEVEL3_DAY_COUNTRY, y = 630, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 630, linetype = "dashed", color = brewer.pal(9, "Reds")[6], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY_COUNTRY, y = 630, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Reds")[7], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
   
   ### COLOR SCHEME CECC
   geom_bar(stat = "identity", fill = "#EE7E32", position = position_dodge(width = 0)) + # , width = 10
   geom_text(data = case_data_projecting, mapping = aes(label = New_Local_Cases_Revised), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
   
   # ORIGINAL values
   geom_bar(mapping = aes(y = New_Local_Cases_Original), stat = "identity", fill = "#5B9CD6", position = position_dodge(width = 0)) + # , width = 10
   
   
   ### COLOR SCHEME NEW
    # REVISED values <--- CHANGETHIS ***************
    #geom_bar(stat = "identity", fill = "#F9CFB4", position = position_dodge(width = 0)) + # , width = 10
    #geom_text(data = case_data_projecting, mapping = aes(label = New_Local_Cases_Revised), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
    #geom_text(data = filter(case_data_projecting, date != timenow), mapping = aes(label = Num_Local_Cases_Projected), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
    #geom_text(data = filter(case_data_projecting, date == timenow), mapping = aes(label = Num_Local_Cases_Projected), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -1.4, size = 1.5) + 
    
   
   ### COLOR SCHEME OLD
    # CURRENT REVISED values
    #geom_bar(mapping = aes(y = New_Local_Cases_Revised), stat = "identity", fill = "#EE7E32", position = position_dodge(width = 0)) + # , width = 10
    
    # ORIGINAL values
    #geom_bar(mapping = aes(y = New_Local_Cases_Original), stat = "identity", fill = "#5B9CD6", position = position_dodge(width = 0)) + # , width = 10
    
    # Show 7-day window with line and error (from plot 5)
    geom_line(mapping = aes(x = sw_end_date, y = sw_REVISED_cases_mean), color = brewer.pal(9, 'Greys')[6]) + # color = brewer.pal(9, "Oranges")[6]) + #
    #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_REVISED_cases_mean - sw_REVISED_cases_SE, ymax = sw_REVISED_cases_mean + sw_REVISED_cases_SE),
    #             alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Greys')[8]) + 
    geom_text(data = filter(case_data_projecting, sw_end_date == timenow), 
              mapping = aes(x = sw_end_date, y = sw_REVISED_cases_mean, label = round(x = sw_REVISED_cases_mean, digits = 0)), 
              color = 'black', fontface = "bold", hjust = -0.3, size = 3.4) + # , hjust = -1
    geom_point(data = filter(case_data_projecting, sw_end_date == timenow),
               mapping = aes(x = sw_end_date, y = sw_REVISED_cases_mean), color = brewer.pal(9, 'Greys')[7]) + #color = brewer.pal(9, "Oranges")[8]) + # 
    
    
    #    # REVISED
    #geom_rect(xmin = as.Date("2021-05-02"), xmax = as.Date("2021-05-10"), ymin = 425, ymax = 625, fill = brewer.pal(9, "YlGn")[3]) + # yellow
    #geom_text(x = as.Date("2021-05-6"), y = 525, label = "Projected\nTotals", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
    
    # Attribution
    #geom_text(x = as.Date("2021-05-02"), y = 335, color = brewer.pal(9, "Greys")[4], hjust = 0, vjust = 0.5, size = 1.6, 
    #          label = "Data: Taiwan CECC, MOHW\nAuthors: Chase W. Nelson & Mitch Lin") +
    
    #    # Counter for number added
  #    geom_text(x = as.Date("2021-05-07"), y = 250-50, label = paste0("Total: ", total_count), fontface = "bold", color = brewer.pal(9, "Greys")[6], hjust = 0, vjust = 0.5, size = 4.75) + #, size = 2.75) +
  #    geom_text(x = as.Date("2021-05-07"), y = 205-50, label = paste0("Backlogged: ", backlog_count), fontface = "bold", color = brewer.pal(9, "Reds")[4], hjust = 0, vjust = 0.5, size = 2.6) + #, size = 2.75) +
  #    geom_text(x = as.Date("2021-05-07"), y = 175-50, label = paste0("Today: ", today_count), fontface = "bold", color = brewer.pal(9, "Blues")[5], hjust = 0, vjust = 0.5, size = 2.6) + # "#5B9CD6"
  #    
  #    # current trendline
  #    #geom_line(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE, size = 1.0) +
  #    #geom_point(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE) +
  #    
  #    
  #    # original trendline
  #    #geom_line(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE, size = 1.0) +
  #    #geom_point(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE) +
  
  ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   Revised   |   ", this_curr_todaystring)) + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
    
    #theme_classic() +
    
    # Custom axis labels
    geom_text(x = -Inf, y = 600, label = "600 daily cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 400, label = "400", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 200, label = "200", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    
    
    geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
    theme(#panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
      axis.ticks.y = element_blank(),
      #axis.line = element_blank(),
      plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
      plot.title = element_text(face = "bold", size = 10), # family = "sans", hjust = 0.5, 
      #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      legend.position = 'none', # c(0.125, 0.525), # 'none',
      #legend.key.size = unit(x = 0.75, units = 'line'), 
      #legend.title = element_text(size = 8), #legend.title = element_blank(),
      #legend.text = element_text(size = 7), #legend.title = element_blank(),
      axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6]),
      axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
      axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
      axis.title.y = element_blank(), #element_text(size = 9),
      strip.background = element_blank()) +
    xlab("") + #ylab("Daily cases") +
    #            breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + 
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0, 0.04)),
                 limits = c(date_chart_start - 1, date_range_end + 1),
                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
    #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
    scale_y_continuous(limits = c(0, 730), 
                       breaks = c(200, 400, 600), expand = expand_scale(mult = c(0, 0.05))))# + , limits = c(0, 600)
#scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
#                  name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/revised_total_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
print(localCases_REVISED_PLOT)
dev.off()



# LATEST TESTS?
case_data_projecting$prop_backlogged <- NA
case_data_projecting[case_data_projecting$New_Local_Cases_Original != case_data_projecting$New_Local_Cases_Revised, ]$prop_backlogged <- 
  (case_data_projecting[case_data_projecting$New_Local_Cases_Original != case_data_projecting$New_Local_Cases_Revised, ]$New_Local_Cases_Revised -
   case_data_projecting[case_data_projecting$New_Local_Cases_Original != case_data_projecting$New_Local_Cases_Revised, ]$New_Local_Cases_Original) /
  case_data_projecting[case_data_projecting$New_Local_Cases_Original != case_data_projecting$New_Local_Cases_Revised, ]$New_Local_Cases_Revised
  
max(case_data_projecting$prop_backlogged, na.rm = TRUE)
sort(case_data_projecting$prop_backlogged)
filter(case_data_projecting, prop_backlogged > 0.492)


###############################################################################
### PLOT PROJECTED ###
(localCases_PROJECTED_PLOT <- ggplot(data = case_data_projecting, 
                                     mapping = aes(x = date, y = Num_Local_Cases_Projected)) +
    
    # LEVEL 3 ALERT - Taipei
    geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 525, linetype = "dashed", color = brewer.pal(9, "Greens")[5], size = 0.2) + #, size = 0.4) +
    geom_text(x = LEVEL3_DAY, y = 525, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Greens")[8], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
    
    # LEVEL 3 ALERT - COUNTRY
    geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 600, linetype = "dashed", color = brewer.pal(9, "Greens")[5], size = 0.2) + #, size = 0.4) +
    geom_text(x = LEVEL3_DAY_COUNTRY, y = 600, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Greens")[8], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
    
    # PREDICTED values <--- CHANGETHIS ***************
    geom_bar(stat = "identity", fill = "#F7C3A1", position = position_dodge(width = 0)) + # , width = 10
    geom_text(data = case_data_projecting, mapping = aes(label = Num_Local_Cases_Projected), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
    #geom_text(data = filter(case_data_projecting, date != timenow), mapping = aes(label = Num_Local_Cases_Projected), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
    #geom_text(data = filter(case_data_projecting, date == timenow), mapping = aes(label = Num_Local_Cases_Projected), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -1.4, size = 1.5) + 
   
    # CURRENT REVISED values
    geom_bar(mapping = aes(y = New_Local_Cases_Revised), stat = "identity", fill = "#EE7E32", position = position_dodge(width = 0)) + # , width = 10
    
    # ORIGINAL values
    geom_bar(mapping = aes(y = New_Local_Cases_Original), stat = "identity", fill = "#5B9CD6", position = position_dodge(width = 0)) + # , width = 10
    
    # Show 7-day window with line and error (from plot 5)
    geom_line(mapping = aes(x = sw_end_date, y = sw_PREDICTED_cases_mean), color = brewer.pal(9, 'Greys')[6]) +
   #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_PREDICTED_cases_mean - sw_PREDICTED_cases_SE, ymax = sw_PREDICTED_cases_mean + sw_PREDICTED_cases_SE),  
   #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_PREDICTED_cases_mean - sw_PREDICTED_cases_sd, ymax = sw_PREDICTED_cases_mean + sw_PREDICTED_cases_sd), 
   #             alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Greys')[8]) + 
   geom_text(data = filter(case_data_projecting, sw_end_date == timenow), 
             mapping = aes(x = sw_end_date, y = sw_PREDICTED_cases_mean, label = round(x = sw_PREDICTED_cases_mean, digits = 0)), color = 'black', fontface = "bold", hjust = -0.3, size = 3.4) + # , hjust = -1
    geom_point(data = filter(case_data_projecting, sw_end_date == timenow),
               mapping = aes(x = sw_end_date, y = sw_PREDICTED_cases_mean), color = "black") + # brewer.pal(9, 'Greys')[7]
   
   
#    # PROJECTED
    #geom_rect(xmin = as.Date("2021-05-02"), xmax = as.Date("2021-05-10"), ymin = 425, ymax = 625, fill = brewer.pal(9, "YlGn")[3]) + # yellow
    #geom_text(x = as.Date("2021-05-6"), y = 525, label = "Projected\nTotals", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +

    # Attribution
    #geom_text(x = as.Date("2021-05-02"), y = 335, color = brewer.pal(9, "Greys")[4], hjust = 0, vjust = 0.5, size = 1.6, 
    #          label = "Data: Taiwan CECC, MOHW\nAuthors: Chase W. Nelson & Mitch Lin") +
    
#    # Counter for number added
#    geom_text(x = as.Date("2021-05-07"), y = 250-50, label = paste0("Total: ", total_count), fontface = "bold", color = brewer.pal(9, "Greys")[6], hjust = 0, vjust = 0.5, size = 4.75) + #, size = 2.75) +
#    geom_text(x = as.Date("2021-05-07"), y = 205-50, label = paste0("Backlogged: ", backlog_count), fontface = "bold", color = brewer.pal(9, "Reds")[4], hjust = 0, vjust = 0.5, size = 2.6) + #, size = 2.75) +
#    geom_text(x = as.Date("2021-05-07"), y = 175-50, label = paste0("Today: ", today_count), fontface = "bold", color = brewer.pal(9, "Blues")[5], hjust = 0, vjust = 0.5, size = 2.6) + # "#5B9CD6"
#    
#    # current trendline
#    #geom_line(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE, size = 1.0) +
#    #geom_point(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE) +
#    
#    
#    # original trendline
#    #geom_line(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE, size = 1.0) +
#    #geom_point(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE) +

    ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   ", this_curr_todaystring)) + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
    
    #theme_classic() +
    
    # Custom axis labels
    geom_text(x = -Inf, y = 600, label = "600 cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 400, label = "400", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 200, label = "200", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    
    
    geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
    theme(#panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
      axis.ticks.y = element_blank(),
      #axis.line = element_blank(),
          plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
          plot.title = element_text(face = "bold", size = 10), # family = "sans", hjust = 0.5, 
          #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          legend.position = 'none', # c(0.125, 0.525), # 'none',
          #legend.key.size = unit(x = 0.75, units = 'line'), 
          #legend.title = element_text(size = 8), #legend.title = element_blank(),
          #legend.text = element_text(size = 7), #legend.title = element_blank(),
          #axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6]),
      axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
      axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
      axis.title.y = element_blank(), #element_text(size = 9),
      strip.background = element_blank()) +
    xlab("") + #ylab("Daily cases") +
   #            breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + 
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0, 0.04)),
                 limits = c(date_chart_start - 1, date_range_end + 1),
                 breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
    #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
    scale_y_continuous(limits = c(0, 650), breaks = c(200, 400, 600), expand = expand_scale(mult = c(0, 0.05))))# + , limits = c(0, 600)
    #scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
    #                  name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))

# SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/projected_total_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
#print(localCases_PROJECTED_PLOT)
#dev.off()


### PLOT THE BACK LOG PROPORTIONS

(backlog_additions_df <- tibble(
  days_late = as.integer(names(days_late_PREDICTED_ADDITIONS)),
  days_late_proportions = days_late_proportions,
  days_late_PREDICTED_ADDITIONS = days_late_PREDICTED_ADDITIONS
))
(backlog_additions_df$days_late_Percent <- percent(backlog_additions_df$days_late_proportions))

# Taiwan orange: #EE7E32

(localCases_ADDITIONS_PLOT <- ggplot(data = backlog_additions_df, 
                                     mapping = aes(x = days_late, y =  days_late_PREDICTED_ADDITIONS)) + #y = days_late_proportions)) +
    
    # PREDICTED values
    geom_bar(stat = "identity", fill = "#F7C3A1", position = position_dodge(width = 0)) + ##F4AB7B, width = 10, #F7C3A1 / #EE7E32 / F3A068 / F4AB7B
    geom_text(aes(label = days_late_Percent), vjust = -0.4, size = rel(1.75)) +
#    #    # Date
#    geom_rect(xmin = as.Date("2021-05-02"), xmax = as.Date("2021-05-12"), ymin = 375, ymax = 575, fill = "#FFE033") +
#    geom_text(x = as.Date("2021-05-7"), y = 475, label = "Predicted\nFinal Revision", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
#    
#    # Attribution
#    geom_text(x = as.Date("2021-05-02"), y = 335, color = brewer.pal(9, "Greys")[4], hjust = 0, vjust = 0.5, size = 1.6, 
#              label = "Data: Taiwan CECC, MOHW\nAuthors: Chase W. Nelson & Mitch Lin") +
    
  #ggtitle(label = "2021 Taiwan Local COVID-19 Cases") + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
    
    #theme_classic() +
  
  # Custom axis labels
  #geom_text(x = -Inf, y = 75, label = "75 projected positive cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 50, label = "50 projected positive cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 25, label = "25", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    
    
  geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
    theme(#panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
      axis.ticks.y = element_blank(),
      #axis.line = element_blank(),#panel.grid.minor = element_blank(),
          #panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "lightgray"),
          plot.margin = unit(x = c(1, 1, 1, 1), units = "line"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          legend.position = c(0.125, 0.525), # 'none',
          legend.key.size = unit(x = 0.75, units = 'line'), 
          legend.title = element_text(size = 8), #legend.title = element_blank(),
          legend.text = element_text(size = 7), #legend.title = element_blank(),
          #axis.text.x = element_text(size = 7),
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(), # element_text(size = 9),
          #axis.title.y = element_blank(), # element_text(size = 9),
          #axis.line.y = element_blank(), # 
          #axis.ticks.y = element_blank(), 
      axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6]),
      axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
      axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
      axis.title.y = element_blank(), # element_text(size = 9),
      strip.background = element_blank()) +
    xlab("Days ago") + #ylab("Projected positive cases") +
    scale_x_reverse(limits = c(20.5, 0.5), breaks = 20:1) + 
    #scale_x_date(labels = date_format("%b %d"),
    #             expand = expand_scale(mult = c(0, 0)),
    #             limits = c(date_chart_start - 1, date_range_end + 1)) + 
    #scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), labels = percent_format()))
    scale_y_continuous(limits = c(0, 55), breaks = c(25, 50), expand = expand_scale(mult = c(0, 0.05))))


#png(filename = paste0("~/Desktop/Taiwan_COVID_data/backlog_time_distribution_", timenow, ".png"), width = 5.5, height = 2.75, units = 'in', res = 500)
#print(localCases_ADDITIONS_PLOT)
#dev.off()



####

# SAVE RESULTS
write_tsv(case_data_projecting, 
          paste0("~/Desktop/Taiwan_COVID_data/results_and_projected_", timenow, ".tsv"))
write_tsv(backlog_additions_df, 
          paste0("~/Desktop/Taiwan_COVID_data/projected_backlog_distribution_", timenow, ".tsv"))



#### PLOT PROP POS and PROP UNKNOWN ###
#(localCases_PROPORTIONS_PLOT <- ggplot(data = case_data_projecting, 
#                                     mapping = aes(x = date, y = unknown_source_prop)) +
#    
#    geom_line(color = brewer.pal(9, 'Greys')[8]) + # mapping = aes(x = sw_end_date, y = sw_PREDICTED_cases_mean)
#    
#    geom_line(aes(y = same_day_test_positive_rate_DiagnosedOnDay_DIV_TestVolumeOnDay), color = brewer.pal(9, 'Reds')[8]) + # mapping = aes(x = sw_end_date, y = sw_PREDICTED_cases_mean)
#    
#    # Show 7-day window with line and error (from plot 5)
#    
#    #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_PREDICTED_cases_mean - sw_PREDICTED_cases_SE, ymax = sw_PREDICTED_cases_mean + sw_PREDICTED_cases_SE),  
#    #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_PREDICTED_cases_mean - sw_PREDICTED_cases_sd, ymax = sw_PREDICTED_cases_mean + sw_PREDICTED_cases_sd), 
#    #             alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Greys')[8]) + 
#    
#    #    # Date
#    #geom_rect(xmin = as.Date("2021-05-02"), xmax = as.Date("2021-05-12"), ymin = 375, ymax = 575, fill = brewer.pal(9, "YlGn")[3]) + # yellow
#    #geom_text(x = as.Date("2021-05-7"), y = 475, label = "Predicted\nFinal Revision", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
#    
#    # Attribution
#    #geom_text(x = as.Date("2021-05-02"), y = 335, color = brewer.pal(9, "Greys")[4], hjust = 0, vjust = 0.5, size = 1.6, 
#    #          label = "Data: Taiwan CECC, MOHW\nAuthors: Chase W. Nelson & Mitch Lin") +
#    
#    #    # Counter for number added
#    #    geom_text(x = as.Date("2021-05-07"), y = 250-50, label = paste0("Total: ", total_count), fontface = "bold", color = brewer.pal(9, "Greys")[6], hjust = 0, vjust = 0.5, size = 4.75) + #, size = 2.75) +
#    #    geom_text(x = as.Date("2021-05-07"), y = 205-50, label = paste0("Backlogged: ", backlog_count), fontface = "bold", color = brewer.pal(9, "Reds")[4], hjust = 0, vjust = 0.5, size = 2.6) + #, size = 2.75) +
#    #    geom_text(x = as.Date("2021-05-07"), y = 175-50, label = paste0("Today: ", today_count), fontface = "bold", color = brewer.pal(9, "Blues")[5], hjust = 0, vjust = 0.5, size = 2.6) + # "#5B9CD6"
#    #    
#    #    # current trendline
#  #    #geom_line(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE, size = 1.0) +
#  #    #geom_point(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE) +
#  #    
#  #    
#  #    # original trendline
#  #    #geom_line(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE, size = 1.0) +
#  #    #geom_point(data = this_curr_date_data_CURR_DAY1, mapping = aes(x = Date_of_Reporting, y = sum_day1), color = brewer.pal(9, "Blues")[8], inherit.aes = FALSE) +
#  
#  ggtitle(label = "2021 Taiwan Local COVID-19 Cases") + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
#    
#    theme_classic() +
#    theme(panel.grid = element_blank(),
#          plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
#          plot.title = element_text(hjust = 0.5),
#          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
#          legend.position = c(0.125, 0.525), # 'none',
#          legend.key.size = unit(x = 0.75, units = 'line'), 
#          legend.title = element_text(size = 8), #legend.title = element_blank(),
#          legend.text = element_text(size = 7), #legend.title = element_blank(),
#          #axis.text.x = element_text(size = 7),
#          #axis.text.x = element_blank(),
#          axis.text.y = element_text(size = 9),
#          axis.title.y = element_text(size = 9),
#          strip.background = element_blank()) +
#    xlab("Proportion") + ylab("Daily cases") +
#    scale_x_date(labels = date_format("%b %d"),
#                 expand = expand_scale(mult = c(0, 0)),
#                 limits = c(date_chart_start - 1, date_range_end + 1)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
#    #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
#    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
#    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
#    scale_y_continuous(expand = expand_scale(mult = c(0, 0))))# + , limits = c(0, 600)
##scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
##                  name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))
#
## SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/localCases_PROPORTIONS_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
#print(localCases_PROPORTIONS_PLOT)
#dev.off()
#
#



###############################################################################
# CASES BY AGE, DEATHS
###############################################################################
#library(jsonlite)
#https://rfaqs.com/reading-and-writing-json-files-in-r/

(cases_by_age <- as_tibble(fromJSON(txt = "~/Desktop/Taiwan_COVID_data/Day_Confirmation_Age_County_Gender_19CoV.json", simplifyDataFrame = TRUE)))
#typeof(cases_by_age)

# Create id
cases_by_age$id <- 1:nrow(cases_by_age)

# Order columns
(cases_by_age <- dplyr::select(cases_by_age, id, everything()))

# Rename columns in English
names(cases_by_age) <- c('id', 'disease_name', 'date', 'county_city', 'township', 'gender', 'immigration', 'age', 'num_cases')
cases_by_age

# Replace Chinese with English
unique(cases_by_age$disease_name) # "嚴重特殊傳染性肺炎"
cases_by_age[cases_by_age$disease_name == '嚴重特殊傳染性肺炎', ]$disease_name <- 'COVID-19'

unique(cases_by_age$date) # all look good

unique(cases_by_age$county_city) 
# "空值"   "彰化縣" "南投縣" "台中市" "新北市" "桃園市" "台南市" "台北市" "新竹市" "基隆市" "宜蘭縣" "高雄市" "新竹縣" "苗栗縣" "雲林縣" "屏東縣" "花蓮縣"
# "嘉義市" "嘉義縣" "台東縣" "連江縣" "澎湖縣"
cases_by_age[cases_by_age$county_city == '空值', ]$county_city <- 'Unknown'
cases_by_age[cases_by_age$county_city == '彰化縣', ]$county_city <- 'Changhua County'
cases_by_age[cases_by_age$county_city == '南投縣', ]$county_city <- 'Nantou County'
cases_by_age[cases_by_age$county_city == '台中市', ]$county_city <- 'Taichung City'
cases_by_age[cases_by_age$county_city == '新北市', ]$county_city <- 'New Taipei City'
cases_by_age[cases_by_age$county_city == '桃園市', ]$county_city <- 'Taoyuan City'
cases_by_age[cases_by_age$county_city == '台南市', ]$county_city <- 'Tainan City'
cases_by_age[cases_by_age$county_city == '台北市', ]$county_city <- 'Taipei City'
cases_by_age[cases_by_age$county_city == '新竹市', ]$county_city <- 'Hsinchu City'
cases_by_age[cases_by_age$county_city == '基隆市', ]$county_city <- 'Keelung City'
cases_by_age[cases_by_age$county_city == '宜蘭縣', ]$county_city <- 'Yilan County'
cases_by_age[cases_by_age$county_city == '高雄市', ]$county_city <- 'Kaohsiung City'
cases_by_age[cases_by_age$county_city == '新竹縣', ]$county_city <- 'Hsinchu County'
cases_by_age[cases_by_age$county_city == '苗栗縣', ]$county_city <- 'Miaoli County'
cases_by_age[cases_by_age$county_city == '雲林縣', ]$county_city <- 'Yunlin County'
cases_by_age[cases_by_age$county_city == '屏東縣', ]$county_city <- 'Pingtung County'
cases_by_age[cases_by_age$county_city == '花蓮縣', ]$county_city <- 'Hualien County'
cases_by_age[cases_by_age$county_city == '嘉義市', ]$county_city <- 'Chiayi City'
cases_by_age[cases_by_age$county_city == '嘉義縣', ]$county_city <- 'Chiayi County'
cases_by_age[cases_by_age$county_city == '台東縣', ]$county_city <- 'Taitung County'
cases_by_age[cases_by_age$county_city == '連江縣', ]$county_city <- 'Lianjiang County'
cases_by_age[cases_by_age$county_city == '澎湖縣', ]$county_city <- 'Penghu County'
unique(cases_by_age$county_city) 


unique(cases_by_age$township) 
townships_chinese <- c('空值', '和美鎮', '南投市', '彰化市', '太平區', '林口區', '新莊區', '新店區', '中和區', '蘆竹區', '桃園區', '龜山區', '北屯區', 
                       '新市區', '五股區', '三重區', '北區', '大同區', '內湖區', '信義區', '汐止區', '東區', '大安區', '仁愛區', '中壢區', '大里區',
                       '文山區', '淡水區', '平鎮區', '八德區', '大園區', '永和區', '蘆洲區', '土城區', '五結鄉', '冬山鄉', '羅東鎮', '萬華區', '頭城鎮', 
                       '中正區', '北投區', '板橋區', '萬里區', '樹林區', '蘇澳鎮', '小港區', '龍井區', '松山區', '南港區', '宜蘭市', '中山區', '八里區', 
                       '三峽區', '田中鎮', '溪湖鎮', '福興鄉', '士林區', '安樂區', '泰山區', '鶯歌區', '竹北市', '溪州鄉', '西屯區', '礁溪鄉', '後龍鎮', 
                       '七堵區', '石碇區', '金山區', '深坑區', '花壇鄉', '員林市', '楊梅區', '大寮區', '新興區', '元長鄉', '貢寮區', '永靖鄉', '埔心鄉',
                       '沙鹿區', '潭子區', '霧峰區', '新營區', '龍潭區', '三民區', '前金區', '鼓山區', '鳳山區', '暖暖區', '芳苑鄉', '大溪區', '新屋區', 
                       '林內鄉', '芬園鄉', '員山鄉', '草屯鎮', '潮州鎮', '左營區', '三芝區', '瑞芳區', '佳里區', '光復鄉', '花蓮市', '竹田鄉', '屏東市', 
                       '鹽埔鄉', '觀音區', '西螺鎮', '香山區', '新豐鄉', '伸港鄉', '鹿港鎮', '安南區', '麻豆區', '壯圍鄉', '名間鄉', '埔里鎮', '九如鄉', 
                       '東勢鄉', '西區', '溪口鄉', '大村鄉', '秀水鄉', '安平區', '竹山鎮', '長治鄉', '三灣鄉', '造橋鄉', '前鎮區', '石門區', '大雅區', 
                       '中區', '太麻里鄉', '成功鎮', '南澳鄉', '吉安鄉', '東港鎮', '萬巒鄉', '大社區', '麥寮鄉', '豐原區', '金峰鄉', '南區', '頭份市', 
                       '南竿鄉', '斗六市', '卑南鄉', '岡山區', '旗山區', '莒光鄉', '古坑鄉', '二水鄉', '石岡區', '南屯區', '白河區', '西港區', '二林鎮', 
                       '東勢區', '新埔鎮', '大林鎮', '竹南鎮', '北竿鄉', '田尾鄉', '埤頭鄉', '馬公市', '湖口鄉', '坪林區', '大城鄉', '苗栗市', '竹塘鄉',
                       '梧棲區', '清水區', '虎尾鎮', '水上鄉', '社頭鄉', '埔鹽鄉', '公館鄉', '台西鄉', '寶山鄉')
townships_english <- c('Unknown', 'Hemei Town', 'Nantou City', 'Changhua City', 'Taiping District', 'Linkou District', 'Xinzhuang District', 
                       'Xindian District', 'Zhonghe District', 'Luzhu District', 'Taoyuan District', 'Guishan District', 'Beitun District', 
                       'New Urban Area', 'Wugu District', 'Sanchong District', 'North District', 'Datong District', 'Neihu District', 'Xinyi District', 
                       'Xizhi District', 'East District', 'Daan District', 'Renai District', 'Zhongli District', 'Dali District', 'Wenshan District', 
                       'Tamsui District', 'Pingzhen District', 'Bade District', 'Dayuan District', 'Yonghe District', 'Luzhou District', 
                       'Tucheng District', 'Wujie Township', 'Dongshan Township', 'Luodong Town', 'Wanhua District', 'Head Town', 
                       'Zhongzheng District', 'Beitou District', 'Banqiao District', 'Wanli District', 'Forest area', 'Suao Town', 
                       'Xiaogang District', 'Longjing District', 'Songshan District', 'Nangang District', 'Yilan City', 'Zhongshan Area', 
                       'Bali District', 'Three Gorges District', 'Tianzhong Town', 'Xihu Town', 'Fuxing Township', 'Shilin District', 
                       'Anle District', 'Taishan District', 'Yingge District', 'Zhubei', 'Xizhou Township', 'Xitun District', 'Jiaoxi Township', 
                       'Houlong Town', 'Qidu District', 'Shiding District', 'Jinshan District', 'Shenkeng District', 'Huatan Township', 'Yuanlin', 
                       'Yangmei District', 'Daliao District', 'Xinxing District', 'Yuanchang Township', 'Gongliao District', 'Yongjing Township', 
                       'Puxin Township', 'Shalu District', 'Tanzi District', 'Wufeng District', 'Xinying District', 'Longtan District', 'Sanmin District', 
                       'Qianjin District', 'Gushan District', 'Fengshan District', 'Warm area', 'Fangyuan Township', 'Daxi District', 'Xinwu District', 
                       'Linnei Township', 'Fenyuan Township', 'Yuanshan Township', 'Caotun Town', 'Chaozhou Town', 'Zuoying District', 'Sanzhi District', 
                       'Ruifang District', 'Jiali District', 'Guangfu Township', 'Hualien City', 'Zhutian Township', 'Pingtung City', 'Yanpu Township', 
                       'Guanyin District', 'Xiluo Town', 'Xiangshan District', 'Xinfeng Township', 'Shengang Township', 'Lugang Town', 'Annan District', 
                       'Madou District', 'Zhuangwei Township', 'Mingjian Township', 'Puli Township', 'Jiuru Township', 'Dongshi Township', 'West End', 
                       'Xikou Township', 'Dacun Xiang', 'Xiushui Township', 'Anping District', 'Zhushan Town', 'Changzhi Township', 'Sanwan Township', 
                       'Zaoqiao Township', 'Qianzhen District', 'Shimen District', 'Daya District', 'Central District', 'Taimali Township', 
                       'Chenggong Town', 'Nanao Township', 'Jian Township', 'Donggang Town', 'Wanluan Township', 'Large community', 'Mailiao Township', 
                       'Fengyuan District', 'Jinfeng Township', 'South area', 'Toufen', 'Nangan Township', 'Douliu City', 'Beinan Township', 
                       'Gangshan District', 'Qishan District', 'Juguang Township', 'Gukeng Township', 'Ershui Township', 'Shigang District', 
                       'Nantun District', 'Baihe District', 'Westport District', 'Erlin Town', 'Dongshi District', 'Xinpu Town', 'Dalin Town', 
                       'Zhunan Town', 'Beigan Township', 'Tianwei Township', 'Pitou Township', 'Magong City', 'Hukou Township', 'Pinglin District', 
                       'Dacheng Country', 'Miaoli City', 'Zhutang Township', 'Wuqi District', 'Qingshui District', 'Huwei Town', 'Water Town', 
                       'Shetou Township', 'Puyan Township', 'Gongguan Township', 'Taixi Township', 'Baoshan Township')

township_key <- townships_english
names(township_key) <- townships_chinese

cases_by_age$township <- township_key[cases_by_age$township]
unique(cases_by_age$township)

unique(cases_by_age$gender) # "女" "男"
cases_by_age[cases_by_age$gender == '女', ]$gender <- "Female"
cases_by_age[cases_by_age$gender == '男', ]$gender <- "Male"
cases_by_age


unique(cases_by_age$immigration) # "是" "否"
cases_by_age[cases_by_age$immigration == '是', ]$immigration <- TRUE
cases_by_age[cases_by_age$immigration == '否', ]$immigration <- FALSE
cases_by_age

# Specify variable types
(cases_by_age$date <- as.Date(cases_by_age$date, format = "%Y%m%d"))
(cases_by_age$immigration <- as.logical(cases_by_age$immigration))

unique(cases_by_age$age)
# "0"
# "1"
# "2"
# "3"
# "4"
# "5-9"
# "10-14"
# "15-19"
# "20-24"
# "25-29"
# "30-34"
# "35-39"
# "40-44"
# "45-49"
# "50-54"
# "55-59"
# "60-64"
# "65-69"
# "70+"

# factor age cateogry
(cases_by_age$age <- factor(x = cases_by_age$age,
                            levels = c("0", "1", "2", "3", "4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70+")))

# New category pooling young
(cases_by_age$age_category <- as.character(cases_by_age$age))
cases_by_age[cases_by_age$age_category %in% c('0', '1', '2', '3', '4'), ]$age_category <- "0-4"
(cases_by_age$age_category <- factor(x = cases_by_age$age_category,
                            levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70+")))

(cases_by_age$age_min <- cases_by_age$age)
(cases_by_age$age_min <- str_replace(string = cases_by_age$age_min, pattern = "\\-\\d+", replacement = ""))
(cases_by_age$age_min <- as.integer(str_replace(string = cases_by_age$age_min, pattern = "\\+", replacement = "")))

(cases_by_age$age_max <- cases_by_age$age)
(cases_by_age$age_max <- str_replace(string = cases_by_age$age_max, pattern = "\\d+\\-", replacement = ""))
(cases_by_age$age_max <- as.integer(str_replace(string = cases_by_age$age_max, pattern = "\\+", replacement = "")))

(cases_by_age$age_spread <- cases_by_age$age_max - cases_by_age$age_min)
(cases_by_age$age_mean <- (cases_by_age$age_max + cases_by_age$age_min) / 2)

# Inspect
#View(cases_by_age)



###############################################################################
### SUMMARIZE LOCAL CASES BY AGE
(cases_by_immigration_SUMMARY <- cases_by_age %>%
    group_by(immigration) %>%
    summarize(
      count = n()
    ))

(cases_by_age_local_SUMMARY <- filter(cases_by_age, immigration == FALSE) %>%
  group_by(age_category) %>%
  summarize(
    count = n()
  ))



###############################################################################
### PLOT CASE AGE DISTRIBUTION ###
(localCases_CASES_BY_AGE_PLOT <- ggplot(data = cases_by_age_local_SUMMARY, 
                                   mapping = aes(x = age_category, y = count)) +
   
   # Counts
   geom_bar(stat = "identity", fill = "#E0E0E0", position = position_dodge(width = 0)) + # , width = 10
   geom_text(mapping = aes(label = count), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
   
   ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   Age distribution   |   ", this_curr_todaystring)) + 
   
   # Custom axis labels
   geom_text(x = -Inf, y = 800, label = "800 cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 600, label = "600", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 400, label = "400", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 200, label = "200", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   
   xlab("Age") +
   
   geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
   
   theme(#panel.grid = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
     axis.ticks.y = element_blank(),
     #axis.line = element_blank(),
     plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
     plot.title = element_text(face = "bold", size = 10), # family = "sans", hjust = 0.5, 
     #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
     legend.position = 'none', # c(0.125, 0.525), # 'none',
     #legend.key.size = unit(x = 0.75, units = 'line'), 
     #legend.title = element_text(size = 8), #legend.title = element_blank(),
     #legend.text = element_text(size = 7), #legend.title = element_blank(),
     axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6], size = 7.5),
     axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
     axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
     axis.title.y = element_blank(), #element_text(size = 9),
     axis.title.x = element_text(colour = brewer.pal(9, "Greys")[7], size = 9),
     strip.background = element_blank()) +
   xlab("Age") + #ylab("Daily cases") +
   scale_y_continuous(limits = c(0, 900), 
                      breaks = c(200, 400, 600, 800), expand = expand_scale(mult = c(0, 0.05))))

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_by_age_PLOT_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
print(localCases_CASES_BY_AGE_PLOT)
dev.off()

# DON'T sum to 10,000, why not?
#778+623+638+630+546+485+497+478+409+354+271+165+119+108+144 = 6245


###############################################################################
###############################################################################
### SUMMARIZE LOCAL DEATHS BY AGE
# download today's Excel: https://covid-19.nchc.org.tw/deathstatistics.php?dt_name=1&downloadall=yes
# paste as-is into CSV
deaths_by_age <- read_tsv("~/Desktop/Taiwan_COVID_data/covidtable_taiwan_cdc_death.tsv", skip = 2,
                          col_names = c('id', 'date', 'case_number', 'gender', 'age', 'history_of_chronic_disease', 'activity_contact_history',
                                        'date_onset', 'symptom', 'date_inspection_notes', 'date_hospitalization_isolation', 'date_diagnosis',
                                        'date_death'))
deaths_by_age

# Factor age
unique(deaths_by_age$age)
# "30"
# "40"
# "50"
# "60"
# "70"
# "80"
# "90"

# factor age cateogry
(deaths_by_age$age <- factor(x = deaths_by_age$age,
                            levels = c("30", "40", "50", "60", "70", "80", "90"),
                            labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")))

# New category pooling old into previous categories
(deaths_by_age$age_category <- as.character(deaths_by_age$age))
deaths_by_age[deaths_by_age$age_category %in% c('70-79', '80-89', '90+'), ]$age_category <- "70+"
(deaths_by_age$age_category <- factor(x = deaths_by_age$age_category,
                                     levels = c("0-9", "10-19", "20-29", "30-39", 
                                                "40-49", "50-59", "60-69", "70+")))

#(deaths_by_age$age_min <- deaths_by_age$age)
#(deaths_by_age$age_min <- str_replace(string = deaths_by_age$age_min, pattern = "\\-\\d+", replacement = ""))
#(deaths_by_age$age_min <- as.integer(str_replace(string = deaths_by_age$age_min, pattern = "\\+", replacement = "")))
#
#(deaths_by_age$age_max <- deaths_by_age$age)
#(deaths_by_age$age_max <- str_replace(string = deaths_by_age$age_max, pattern = "\\d+\\-", replacement = ""))
#(deaths_by_age$age_max <- as.integer(str_replace(string = deaths_by_age$age_max, pattern = "\\+", replacement = "")))
#
#(deaths_by_age$age_spread <- deaths_by_age$age_max - deaths_by_age$age_min)
#(deaths_by_age$age_mean <- (deaths_by_age$age_max + deaths_by_age$age_min) / 2)

# Replace Chinese with English
deaths_by_age

unique(deaths_by_age$gender) # "女" "男"
deaths_by_age[deaths_by_age$gender == '女', ]$gender <- "Female"
deaths_by_age[deaths_by_age$gender == '男', ]$gender <- "Male"
deaths_by_age

# can add specific disease later! <-- TODO
#table(deaths_by_age$history_of_chronic_disease) # most common are 有  or 無
unique(deaths_by_age$history_of_chronic_disease) 
deaths_by_age[! is.na(deaths_by_age$history_of_chronic_disease) & deaths_by_age$history_of_chronic_disease == '--', ]$history_of_chronic_disease <- NA
deaths_by_age[! is.na(deaths_by_age$history_of_chronic_disease) & deaths_by_age$history_of_chronic_disease != '無', ]$history_of_chronic_disease <- TRUE
deaths_by_age[! is.na(deaths_by_age$history_of_chronic_disease) & deaths_by_age$history_of_chronic_disease == '無', ]$history_of_chronic_disease <- FALSE
unique(deaths_by_age$history_of_chronic_disease) 

# activity_contact_history
unique(deaths_by_age$activity_contact_history)
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '接觸其他確診者', ]$activity_contact_history <- "contact_with_case"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '萬華區活動史', ]$activity_contact_history <- "Wanhua District"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '調查中', ]$activity_contact_history <- "Survey"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == 'NA', ]$activity_contact_history <- NA
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '萬華活動史', ]$activity_contact_history <- "Wanhua"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '菲律賓回台', ]$activity_contact_history <- "Philippines"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '緬甸回台', ]$activity_contact_history <- "Myanmar"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '境外移入（英國變種病毒）', ]$activity_contact_history <- "Immigration UK Variant"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '奧地利、捷克旅遊史', ]$activity_contact_history <- "Austria and Czech Republic"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '西班牙旅遊史', ]$activity_contact_history <- "Spain"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '埃及旅遊史', ]$activity_contact_history <- "Egypt"
deaths_by_age[! is.na(deaths_by_age$activity_contact_history) & deaths_by_age$activity_contact_history == '美國旅遊史', ]$activity_contact_history <- "USA"
unique(deaths_by_age$activity_contact_history)

# do later
unique(deaths_by_age$symptom)

#View(deaths_by_age)


# Separate notes from dates
unique(deaths_by_age$date_onset)
# 無症狀 is asymptomatic
# 調查中 is survey
deaths_by_age[! is.na(deaths_by_age$date_onset) & deaths_by_age$date_onset == "無症狀", ]$date_onset <- "Asymptomatic"
deaths_by_age[! is.na(deaths_by_age$date_onset) & deaths_by_age$date_onset == "調查中", ]$date_onset <- "Survey"

unique(deaths_by_age$date_inspection_notes)
deaths_by_age$date_inspection <- deaths_by_age$date_inspection_notes
deaths_by_age[! is.na(deaths_by_age$date_inspection) & deaths_by_age$date_inspection == "調查中", ]$date_inspection <- "0"
deaths_by_age[! is.na(deaths_by_age$date_inspection) & deaths_by_age$date_inspection == "--", ]$date_inspection <- NA
unique(deaths_by_age$date_inspection)
deaths_by_age$date_inspection <- str_replace_all(string = deaths_by_age$date_inspection, pattern = "\\D+", replacement = " ")
unique(deaths_by_age$date_inspection)
deaths_by_age$date_inspection <- str_replace(string = deaths_by_age$date_inspection, pattern = "^(\\d+) (\\d+) (\\d+)$", replacement = "\\1-\\2-\\3")
unique(deaths_by_age$date_inspection)
deaths_by_age$date_inspection <- str_replace(string = deaths_by_age$date_inspection, pattern = "\\s+$", replacement = "")
unique(deaths_by_age$date_inspection)
deaths_by_age$date_inspection <- str_replace(string = deaths_by_age$date_inspection, pattern = "^.+(\\d+) (\\d+)$", replacement = "2021-\\1-\\2")
deaths_by_age[! is.na(deaths_by_age$date_inspection) & deaths_by_age$date_inspection == "0", ]$date_inspection <- "Survey"
unique(deaths_by_age$date_inspection)

unique(deaths_by_age$date_diagnosis)
deaths_by_age[! is.na(deaths_by_age$date_diagnosis) & deaths_by_age$date_diagnosis == "5/2o", ]$date_diagnosis <- "2021/5/20"
unique(deaths_by_age$date_diagnosis)

unique(deaths_by_age$date_death) # all good


# Specify variable types
(deaths_by_age$date_onset <- as.Date(deaths_by_age$date_onset)) # , format = "%Y%m%d")
(deaths_by_age$date_inspection <- as.Date(deaths_by_age$date_inspection))
(deaths_by_age$date_diagnosis <- as.Date(deaths_by_age$date_diagnosis))
(deaths_by_age$date_death <- as.Date(deaths_by_age$date_death))

(deaths_by_age$history_of_chronic_disease <- as.logical(deaths_by_age$history_of_chronic_disease))


####################
#### SUMMARIZE
(deaths_gender_SUMMARY <- deaths_by_age %>%
   group_by(gender) %>%
   summarize(
     count = n()
   )) # lots of males

(deaths_history_SUMMARY <- deaths_by_age %>%
    group_by(history_of_chronic_disease) %>%
    summarize(
      count = n()
    ))

(deaths_activity_SUMMARY <- deaths_by_age %>%
    group_by(activity_contact_history) %>%
    summarize(
      count = n()
    ))

sort(unique(deaths_by_age$date)) # just do after MIN_DATE_DISPLAYED, 2021/05/01


### REFACTOR cases
# New category pooling young
(cases_by_age$age_category2 <- as.character(cases_by_age$age_category))
unique(cases_by_age$age_category2)
cases_by_age[cases_by_age$age_category2 %in% c('0-4', '5-9'), ]$age_category2 <- "0-9"
cases_by_age[cases_by_age$age_category2 %in% c('10-14', '15-19'), ]$age_category2 <- "10-19"
cases_by_age[cases_by_age$age_category2 %in% c('20-24', '25-29'), ]$age_category2 <- "20-29"
cases_by_age[cases_by_age$age_category2 %in% c('30-34', '35-39'), ]$age_category2 <- "30-39"
cases_by_age[cases_by_age$age_category2 %in% c('40-44', '45-49'), ]$age_category2 <- "40-49"
cases_by_age[cases_by_age$age_category2 %in% c('50-54', '55-59'), ]$age_category2 <- "50-59"
cases_by_age[cases_by_age$age_category2 %in% c('60-64', '65-69'), ]$age_category2 <- "60-69"
(cases_by_age$age_category2 <- factor(x = cases_by_age$age_category2,
                                     levels = c("0-9", "10-19", "20-29", "30-39", 
                                                "40-49", "50-59", "60-69", "70+")))

deaths_by_age



### SUMMARIZE BOTH
(cases_by_age_RecentLocal_SUMMARY <- filter(cases_by_age, immigration == FALSE, date >= as.Date(MIN_DATE_DISPLAYED)) %>%
    group_by(age_category2) %>%
    summarize(
      count = n()
    ))

(deaths_by_age_RecentLocal_SUMMARY <- filter(deaths_by_age, date_death >= as.Date(MIN_DATE_DISPLAYED), 
                                             activity_contact_history %in% c("contact_with_case", "Survey", "Wanhua", "Wanhua District")) %>%
    group_by(age_category) %>%
    summarize(
      count = n()
    ))



###############################################################################
### PLOT CASE AGE DISTRIBUTION ###
(local_cases_and_deaths_by_age_PLOT <- ggplot(data = cases_by_age_RecentLocal_SUMMARY, 
                                        mapping = aes(x = age_category2, y = count)) +
   
   # Case counts by age
   geom_bar(stat = "identity", fill = "#E0E0E0", position = position_dodge(width = 0)) + # , width = 10
   geom_text(mapping = aes(label = count), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
   
   # Death counts by age
   geom_bar(data = deaths_by_age_RecentLocal_SUMMARY, mapping = aes(x = age_category),
            stat = "identity", fill = "pink", position = position_dodge(width = 0)) + # , width = 10
   geom_text(data = deaths_by_age_RecentLocal_SUMMARY, mapping = aes(x = age_category, label = count), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
   
   ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   Age distribution   |   ", this_curr_todaystring)) + 
   
   # Custom axis labels
   geom_text(x = -Inf, y = 1250, label = "1250 cases or deaths", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 1000, label = "1000", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 750, label = "750", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 500, label = "500", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   #geom_text(x = -Inf, y = 250, label = "250", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   
   geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
   
   theme(#panel.grid = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
     axis.ticks.y = element_blank(),
     #axis.line = element_blank(),
     plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
     plot.title = element_text(face = "bold", size = 10), # family = "sans", hjust = 0.5, 
     #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
     legend.position = 'none', # c(0.125, 0.525), # 'none',
     #legend.key.size = unit(x = 0.75, units = 'line'), 
     #legend.title = element_text(size = 8), #legend.title = element_blank(),
     #legend.text = element_text(size = 7), #legend.title = element_blank(),
     axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6], size = 7.5),
     axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
     axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
     axis.title.y = element_blank(), #element_text(size = 9),
     axis.title.x = element_text(colour = brewer.pal(9, "Greys")[7], size = 9), 
     strip.background = element_blank()) +
   xlab("Age") + #ylab("Daily cases") +
   scale_y_continuous(#limits = c(0, 1400), 
                      breaks = c(250, 500, 750, 1000, 1250), expand = expand_scale(mult = c(0, 0.05))))

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_and_deaths_by_age_PLOT_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
print(local_cases_and_deaths_by_age_PLOT)
dev.off()

# DON'T sum to 10,000, why not?
#778+623+638+630+546+485+497+478+409+354+271+165+119+108+144 = 6245



###############################################################################
### sliding window for CASES and DEATHS

### CASES: FILTER for local, recent
cases_by_age_filtered <- filter(cases_by_age, immigration == FALSE, date >= as.Date(MIN_DATE_DISPLAYED))

### DEATHS: FILTER for local, recent
deaths_by_age_filtered <- filter(deaths_by_age, date_death >= as.Date(MIN_DATE_DISPLAYED), 
                                 activity_contact_history %in% c("contact_with_case", "Survey", "Wanhua", "Wanhua District"))

## initialize numbered days
#cases_by_age_filtered$day <- 1:nrow(cases_by_age_filtered)

# find minimum and maximum dates: WILL USE MIN_DATE_DISPLAYED
#(time0_PREDICTED <- min(cases_by_age_filtered$date))

# CREATE NEW SLIDING WINDOW DATA FRAME
cases_deaths_age_sw_data = tibble(
  sw_start_date = NA,
  sw_end_date = NA,
  #sw_num_days_with_data = NA,
  
  # Cases
  sw_cases_lt50yo_sum = NA,
  sw_cases_lt50yo_mean = NA,
  #sw_cases_lt50yo_sd = NA,
  sw_cases_lt50yo_n = NA,
  #sw_cases_lt50yo_SE = NA,
  
  sw_cases_50to59yo_sum = NA,
  sw_cases_50to59yo_mean = NA,
  #sw_cases_50to59yo_sd = NA,
  sw_cases_50to59yo_n = NA,
  #sw_cases_50to59yo_SE = NA,
  
  sw_cases_60to69yo_sum = NA,
  sw_cases_60to69yo_mean = NA,
  #sw_cases_60to69yo_sd = NA,
  sw_cases_60to69yo_n = NA,
  #sw_cases_60to69yo_SE = NA,
  
  sw_cases_gt70yo_sum = NA,
  sw_cases_gt70yo_mean = NA,
  #sw_cases_gt70yo_sd = NA,
  sw_cases_gt70yo_n = NA,
  #sw_cases_gt70yo_SE = NA,
  
  # Deaths
  sw_deaths_lt50yo_sum = NA,
  sw_deaths_lt50yo_mean = NA,
  #sw_deaths_lt50yo_sd = NA,
  sw_deaths_lt50yo_n = NA,
  #sw_deaths_lt50yo_SE = NA,
  
  sw_deaths_50to59yo_sum = NA,
  sw_deaths_50to59yo_mean = NA,
  #sw_deaths_50to59yo_sd = NA,
  sw_deaths_50to59yo_n = NA,
  #sw_deaths_50to59yo_SE = NA,
  
  sw_deaths_60to69yo_sum = NA,
  sw_deaths_60to69yo_mean = NA,
  #sw_deaths_60to69yo_sd = NA,
  sw_deaths_60to69yo_n = NA,
  #sw_deaths_60to69yo_SE = NA,
  
  sw_deaths_gt70yo_sum = NA,
  sw_deaths_gt70yo_mean = NA,
  #sw_deaths_gt70yo_sd = NA,
  sw_deaths_gt70yo_n = NA,
  #sw_deaths_gt70yo_SE = NA
)

#cases_deaths_age_sw_data_NEWROW <- cases_deaths_age_sw_data

# perform sliding window
this_curr_age_date <- MIN_DATE_DISPLAYED
this_curr_age_date_MAX <- min(c(max(cases_by_age_filtered$date), max(deaths_by_age_filtered$date_death))) # both have data

while (this_curr_age_date <= (this_curr_age_date_MAX - WINDOW_SIZE + 1)) { # possible start two days before because incomplete
  #this_curr_age_date <- as.Date("2021-05-01")
  #this_curr_age_date <- as.Date("2021-05-11")
  #this_curr_age_date <- as.Date("2021-05-28")
  #this_curr_age_date <- as.Date("2021-05-31")
  #this_curr_age_date <- as.Date("2021-06-01")
  cat(this_curr_age_date, "")
  
  this_sw_start <- this_curr_age_date
  this_sw_end <- this_curr_age_date + WINDOW_SIZE - 1
  
  
  ### CASES
  this_curr_age_date_data_cases <- filter(cases_by_age_filtered, date >= this_curr_age_date, date <= this_curr_age_date + WINDOW_SIZE - 1)
  #this_sw_num_days_with_data <- length(unique(this_curr_age_date_data_cases$date))
  
  # new age categories
  this_curr_age_date_data_cases$age_category3 <- as.character(this_curr_age_date_data_cases$age_category2)
  this_curr_age_date_data_cases[this_curr_age_date_data_cases$age_category2 %in% c('0-9', '10-19', '20-29', '30-39', '40-49'), ]$age_category3 <- "<50"
  this_curr_age_date_data_cases$age_category3 <- factor(this_curr_age_date_data_cases$age_category3, 
                                                             levels = c("<50", "50-59", "60-69", "70+"))
  
  # Summarize
  this_curr_age_date_data_cases_SUMMARIZED <- this_curr_age_date_data_cases %>% 
    group_by(date, age_category3) %>%
    summarize(
      count = n()
    ) # 1129 for 5/28, yes
  
  # Cases
  this_sw_cases_lt50yo_sum <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE)
  this_sw_cases_lt50yo_mean <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_cases_lt50yo_sd <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE)
  this_sw_cases_lt50yo_n <- WINDOW_SIZE
  #this_sw_cases_lt50yo_SE <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  this_sw_cases_50to59yo_sum <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE)
  this_sw_cases_50to59yo_mean <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_cases_50to59yo_sd <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE)
  this_sw_cases_50to59yo_n <- WINDOW_SIZE
  #this_sw_cases_50to59yo_SE <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  this_sw_cases_60to69yo_sum <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE)
  this_sw_cases_60to69yo_mean <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_cases_60to69yo_sd <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE)
  this_sw_cases_60to69yo_n <- WINDOW_SIZE
  #this_sw_cases_60to69yo_SE <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  this_sw_cases_gt70yo_sum <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE)
  this_sw_cases_gt70yo_mean <- sum(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_cases_gt70yo_sd <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE)
  this_sw_cases_gt70yo_n <- WINDOW_SIZE
  #this_sw_cases_gt70yo_SE <- sd(filter(this_curr_age_date_data_cases_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  
  ## DEATHS
  this_curr_age_date_data_deaths <- filter(deaths_by_age_filtered, date_death >= this_curr_age_date, date_death <= this_curr_age_date + WINDOW_SIZE - 1)
  
  # new age categories
  this_curr_age_date_data_deaths$age_category3 <- as.character(this_curr_age_date_data_deaths$age_category)
  if(nrow(filter(this_curr_age_date_data_deaths, age_category %in% c('0-9', '10-19', '20-29', '30-39', '40-49'))) > 0) {
    this_curr_age_date_data_deaths[this_curr_age_date_data_deaths$age_category %in% c('0-9', '10-19', '20-29', '30-39', '40-49'), ]$age_category3 <- "<50"
  }
  this_curr_age_date_data_deaths$age_category3 <- factor(this_curr_age_date_data_deaths$age_category3, 
                                                  levels = c("<50", "50-59", "60-69", "70+"))
  
  # Summarize
  this_curr_age_date_data_deaths_SUMMARIZED <- this_curr_age_date_data_deaths %>% 
    group_by(date_death, age_category3) %>%
    summarize(
      count = n()
    ) # 1129 for 5/28, yes
  
  # Deaths
  this_sw_deaths_lt50yo_sum <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE)
  this_sw_deaths_lt50yo_mean <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_deaths_lt50yo_sd <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE)
  this_sw_deaths_lt50yo_n <- WINDOW_SIZE
  #this_sw_deaths_lt50yo_SE <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "<50")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  this_sw_deaths_50to59yo_sum <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE)
  this_sw_deaths_50to59yo_mean <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_deaths_50to59yo_sd <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE)
  this_sw_deaths_50to59yo_n <- WINDOW_SIZE
  #this_sw_deaths_50to59yo_SE <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "50-59")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  this_sw_deaths_60to69yo_sum <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE)
  this_sw_deaths_60to69yo_mean <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_deaths_60to69yo_sd <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE)
  this_sw_deaths_60to69yo_n <- WINDOW_SIZE
  #this_sw_deaths_60to69yo_SE <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "60-69")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  this_sw_deaths_gt70yo_sum <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE)
  this_sw_deaths_gt70yo_mean <- sum(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE) / WINDOW_SIZE
  #this_sw_deaths_gt70yo_sd <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE)
  this_sw_deaths_gt70yo_n <- WINDOW_SIZE
  #this_sw_deaths_gt70yo_SE <- sd(filter(this_curr_age_date_data_deaths_SUMMARIZED, age_category3 == "70+")$count, na.rm = TRUE) / sqrt(WINDOW_SIZE)
  
  
  
  # ADD NEW ROW
  cases_deaths_age_sw_data_NEWROW = tibble(
    sw_start_date = this_sw_start,
    sw_end_date = this_sw_end,
    #sw_num_days_with_data = this_sw_num_days_with_data,
    
    # Cases
    sw_cases_lt50yo_sum = this_sw_cases_lt50yo_sum,
    sw_cases_lt50yo_mean = this_sw_cases_lt50yo_mean,
    #sw_cases_lt50yo_sd = this_sw_cases_lt50yo_sd,
    sw_cases_lt50yo_n = this_sw_cases_lt50yo_n,
    #sw_cases_lt50yo_SE = this_sw_cases_lt50yo_SE,
    
    sw_cases_50to59yo_sum = this_sw_cases_50to59yo_sum,
    sw_cases_50to59yo_mean = this_sw_cases_50to59yo_mean,
    #sw_cases_50to59yo_sd = this_sw_cases_50to59yo_sd,
    sw_cases_50to59yo_n = this_sw_cases_50to59yo_n,
    #sw_cases_50to59yo_SE = this_sw_cases_50to59yo_SE,
    
    sw_cases_60to69yo_sum = this_sw_cases_60to69yo_sum,
    sw_cases_60to69yo_mean = this_sw_cases_60to69yo_mean,
    #sw_cases_60to69yo_sd = this_sw_cases_60to69yo_sd,
    sw_cases_60to69yo_n = this_sw_cases_60to69yo_n,
    #sw_cases_60to69yo_SE = this_sw_cases_60to69yo_SE,
    
    sw_cases_gt70yo_sum = this_sw_cases_gt70yo_sum,
    sw_cases_gt70yo_mean = this_sw_cases_gt70yo_mean,
    #sw_cases_gt70yo_sd = this_sw_cases_gt70yo_sd,
    sw_cases_gt70yo_n = this_sw_cases_gt70yo_n,
    #sw_cases_gt70yo_SE = this_sw_cases_gt70yo_SE,
    
    # Deaths
    sw_deaths_lt50yo_sum = this_sw_deaths_lt50yo_sum,
    sw_deaths_lt50yo_mean = this_sw_deaths_lt50yo_mean,
    #sw_deaths_lt50yo_sd = this_sw_deaths_lt50yo_sd,
    sw_deaths_lt50yo_n = this_sw_deaths_lt50yo_n,
    #sw_deaths_lt50yo_SE = this_sw_deaths_lt50yo_SE,
    
    sw_deaths_50to59yo_sum = this_sw_deaths_50to59yo_sum,
    sw_deaths_50to59yo_mean = this_sw_deaths_50to59yo_mean,
    #sw_deaths_50to59yo_sd = this_sw_deaths_50to59yo_sd,
    sw_deaths_50to59yo_n = this_sw_deaths_50to59yo_n,
    #sw_deaths_50to59yo_SE = this_sw_deaths_50to59yo_SE,
    
    sw_deaths_60to69yo_sum = this_sw_deaths_60to69yo_sum,
    sw_deaths_60to69yo_mean = this_sw_deaths_60to69yo_mean,
    #sw_deaths_60to69yo_sd = this_sw_deaths_60to69yo_sd,
    sw_deaths_60to69yo_n = this_sw_deaths_60to69yo_n,
    #sw_deaths_60to69yo_SE = this_sw_deaths_60to69yo_SE,
    
    sw_deaths_gt70yo_sum = this_sw_deaths_gt70yo_sum,
    sw_deaths_gt70yo_mean = this_sw_deaths_gt70yo_mean,
    #sw_deaths_gt70yo_sd = this_sw_deaths_gt70yo_sd,
    sw_deaths_gt70yo_n = this_sw_deaths_gt70yo_n,
    #sw_deaths_gt70yo_SE = thisw_deaths_gt70yo_SE
  )
  
  if(is.na(cases_deaths_age_sw_data[1, ]$sw_start_date)) {
    cases_deaths_age_sw_data <- cases_deaths_age_sw_data_NEWROW
  } else {
    cases_deaths_age_sw_data <- rbind(cases_deaths_age_sw_data, cases_deaths_age_sw_data_NEWROW)
  }
  
  # increment the day
  this_curr_age_date <- this_curr_age_date + 1
}

#View(cases_deaths_age_sw_data)





###############################################################################
### CASES PLOT ###
(cases_byAge_PLOT <- ggplot(data = cases_deaths_age_sw_data, 
                             mapping = aes(x = sw_end_date, y = sw_cases_lt50yo_mean)) +
   
   # LEVEL 3 ALERT - Taipei
   geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 125, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY, y = 125, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
   # LEVEL 3 ALERT - COUNTRY
   geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 150, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY_COUNTRY, y = 150, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
   # REVISED values <--- CHANGETHIS ***************
   #geom_bar(stat = "identity", fill = "#F9CFB4", position = position_dodge(width = 0)) + # , width = 10
   #geom_text(data = case_data_projecting, mapping = aes(label = New_Local_Cases_Revised), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
   
   # CASES: y-day windows
   geom_line(mapping = aes(x = sw_end_date, y = sw_cases_lt50yo_mean), color = "#0072B2") + # color = brewer.pal(9, "Greys")[8]) + #color = brewer.pal(9, 'Greys')[6]) +
   geom_line(mapping = aes(x = sw_end_date, y = sw_cases_50to59yo_mean), color = "#009E73") + #color = brewer.pal(9, "Greys")[7]) + #color = brewer.pal(9, 'Greys')[6]) +
   geom_line(mapping = aes(x = sw_end_date, y = sw_cases_60to69yo_mean), color = "#D55E00") + #color = brewer.pal(9, "Greys")[6]) + #color = brewer.pal(9, 'Greys')[6]) +
   geom_line(mapping = aes(x = sw_end_date, y = sw_cases_gt70yo_mean), color = "#CC79A7") + #color = brewer.pal(9, "Greys")[5]) + #color = brewer.pal(9, 'Greys')[6]) +
 
   # Label lines
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_cases_lt50yo_mean), label = "<50 years", color = '#0072B2', hjust = -0.1, size = 2) +
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_cases_50to59yo_mean - 5), label = "50-59", color = '#009E73', hjust = -0.1, size = 2) +
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_cases_60to69yo_mean + 10), label = "60-69", color = '#D55E00', hjust = -0.1, size = 2) +
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_cases_gt70yo_mean - 8), label = ">70", color = '#CC79A7', hjust = -0.1, size = 2) +
   #geom_text_repel(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
   #          mapping = aes(x = sw_end_date, y = sw_cases_lt50yo_mean), label = "<50 years", color = '#0072B2', hjust = 0, size = 2,
   #          xlim = c(as.Date(timenow), Inf)) +
   #geom_text_repel(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
   #          mapping = aes(x = sw_end_date, y = sw_cases_50to59yo_mean - 2), label = "50-59", color = '#009E73', hjust = 0, size = 2,
   #          xlim = c(as.Date(timenow), Inf)) +
   #geom_text_repel(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
   #          mapping = aes(x = sw_end_date, y = sw_cases_60to69yo_mean + 6), label = "60-69", color = '#D55E00',hjust = 0, size = 2,
   #          xlim = c(as.Date(timenow), Inf)) +
   #geom_text_repel(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
   #          mapping = aes(x = sw_end_date, y = sw_cases_gt70yo_mean - 2), label = ">70", color = '#CC79A7', hjust = 0, size = 2,
   #          xlim = c(as.Date(timenow), Inf)) +
   
 #geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_lt50yo_mean), color = brewer.pal(9, "Reds")[8]) + #color = brewer.pal(9, 'Greys')[6]) +
 #  geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_50to59yo_mean), color = brewer.pal(9, "Oranges")[7]) + #color = brewer.pal(9, 'Greys')[6]) +
 #  geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_60to69yo_mean), color = brewer.pal(9, "Oranges")[6]) + #color = brewer.pal(9, 'Greys')[6]) +
 #  geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_gt70yo_mean), color = brewer.pal(9, "Oranges")[5]) + #color = brewer.pal(9, 'Greys')[6]) +
   
   # Show 7-day window with line and error (from plot 5)
   #geom_line(mapping = aes(x = sw_end_date, y = sw_cases_lt50yo_mean), color = brewer.pal(9, "Oranges")[6]) + #color = brewer.pal(9, 'Greys')[6]) +
   #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_REVISED_cases_mean - sw_REVISED_cases_SE, ymax = sw_REVISED_cases_mean + sw_REVISED_cases_SE),
   #             alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Greys')[8]) + 
   #geom_text(data = filter(case_data_projecting, sw_end_date == timenow), 
   #          mapping = aes(x = sw_end_date, y = sw_REVISED_cases_mean, label = round(x = sw_REVISED_cases_mean, digits = 0)), color = 'black', fontface = "bold", hjust = -0.3, size = 3.4) + # , hjust = -1
   #geom_point(data = filter(case_data_projecting, sw_end_date == timenow),
   #           mapping = aes(x = sw_end_date, y = sw_REVISED_cases_mean), color = brewer.pal(9, "Oranges")[8]) + # brewer.pal(9, 'Greys')[7]
   
   
 #ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   Cases by age   |   ", this_curr_todaystring)) + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
   
   #theme_classic() +
   
   # Custom axis labels
   geom_text(x = -Inf, y = 150, label = "150 cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 100, label = "100", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 50, label = "50", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   
   
   geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
   theme(#panel.grid = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
     axis.ticks.y = element_blank(),
     #axis.line = element_blank(),
     plot.margin=unit(x = c(1, 1, 0, 1), units = "line"),
     plot.title = element_text(face = "bold", size = 10), # family = "sans", hjust = 0.5, 
     #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
     legend.position = 'none', # c(0.125, 0.525), # 'none',
     #legend.key.size = unit(x = 0.75, units = 'line'), 
     #legend.title = element_text(size = 8), #legend.title = element_blank(),
     #legend.text = element_text(size = 7), #legend.title = element_blank(),
     axis.text.x = element_blank(), # element_text(colour = brewer.pal(9, "Greys")[6]),
     axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
     axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
     axis.title.y = element_blank(), #element_text(size = 9),
     strip.background = element_blank()) +
   xlab("") + #ylab("Daily cases") +
   #            breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + 
   scale_x_date(labels = date_format("%b %d"),
                expand = expand_scale(mult = c(0, 0.04)),
                limits = c(date_chart_start - 1, date_range_end + 1),
                breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
   #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
   #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
   #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
   scale_y_continuous(limits = c(0, 180), 
                      breaks = c(50, 100, 150), 
                      expand = expand_scale(mult = c(0, 0.05))))# + , limits = c(0, 600)
#scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
#                  name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))

# SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_byAge_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
#print(cases_byAge_PLOT)
#dev.off()



###############################################################################
### DEATH PLOT ###
(deaths_byAge_PLOT <- ggplot(data = cases_deaths_age_sw_data, 
                                   mapping = aes(x = sw_end_date, y = sw_deaths_lt50yo_mean)) +
   
   # LEVEL 3 ALERT - Taipei
   geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 555, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY, y = 555, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
   # LEVEL 3 ALERT - COUNTRY
   geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 630, linetype = "dashed", color = brewer.pal(9, "Blues")[4], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY_COUNTRY, y = 630, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Blues")[6], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
   # REVISED values <--- CHANGETHIS ***************
   #geom_bar(stat = "identity", fill = "#F9CFB4", position = position_dodge(width = 0)) + # , width = 10
   #geom_text(data = case_data_projecting, mapping = aes(label = New_Local_Cases_Revised), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
   
   
   # CASES: y-day windows
   #geom_line(mapping = aes(x = sw_end_date, y = sw_cases_lt50yo_mean), color = brewer.pal(9, "Greys")[9]) + #color = brewer.pal(9, 'Greys')[6]) +
   #geom_line(mapping = aes(x = sw_end_date, y = sw_cases_50to59yo_mean), color = brewer.pal(9, "Greys")[7]) + #color = brewer.pal(9, 'Greys')[6]) +
   #geom_line(mapping = aes(x = sw_end_date, y = sw_cases_60to69yo_mean), color = brewer.pal(9, "Greys")[5]) + #color = brewer.pal(9, 'Greys')[6]) +
   #geom_line(mapping = aes(x = sw_end_date, y = sw_cases_gt70yo_mean), color = brewer.pal(9, "Greys")[3]) + #color = brewer.pal(9, 'Greys')[6]) +
   
   geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_lt50yo_mean), color = "#0072B2") + # brewer.pal(9, "Reds")[8]) + #color = brewer.pal(9, 'Greys')[6]) +
   geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_50to59yo_mean), color = "#009E73") + #color = brewer.pal(9, "Oranges")[7]) + #color = brewer.pal(9, 'Greys')[6]) +
   geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_60to69yo_mean), color = "#D55E00") + #color = brewer.pal(9, "Oranges")[6]) + #color = brewer.pal(9, 'Greys')[6]) +
   geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_gt70yo_mean), color = "#CC79A7") + #color = brewer.pal(9, "Oranges")[5]) + #color = brewer.pal(9, 'Greys')[6]) +
   
   # Label lines
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_deaths_lt50yo_mean + 0.5), label = "<50", color = '#0072B2', hjust = -0.1, size = 2) +
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_deaths_50to59yo_mean + 1.75), label = "50-59", color = '#009E73', hjust = -0.1, size = 2) +
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_deaths_60to69yo_mean), label = "60-69", color = '#D55E00', hjust = -0.1, size = 2) +
   geom_text(data = filter(cases_deaths_age_sw_data, sw_end_date == this_curr_age_date_MAX), 
             mapping = aes(x = sw_end_date, y = sw_deaths_gt70yo_mean), label = ">70 years", color = '#CC79A7', hjust = -0.1, size = 2) +
   
   # Show 7-day window with line and error (from plot 5)
   #geom_line(mapping = aes(x = sw_end_date, y = sw_cases_lt50yo_mean), color = brewer.pal(9, "Oranges")[6]) + #color = brewer.pal(9, 'Greys')[6]) +
   #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_REVISED_cases_mean - sw_REVISED_cases_SE, ymax = sw_REVISED_cases_mean + sw_REVISED_cases_SE),
   #             alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Greys')[8]) + 
   #geom_text(data = filter(case_data_projecting, sw_end_date == timenow), 
   #          mapping = aes(x = sw_end_date, y = sw_REVISED_cases_mean, label = round(x = sw_REVISED_cases_mean, digits = 0)), color = 'black', fontface = "bold", hjust = -0.3, size = 3.4) + # , hjust = -1
   #geom_point(data = filter(case_data_projecting, sw_end_date == timenow),
   #           mapping = aes(x = sw_end_date, y = sw_REVISED_cases_mean), color = brewer.pal(9, "Oranges")[8]) + # brewer.pal(9, 'Greys')[7]
   
   
 #ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   Deaths by age   |   ", this_curr_todaystring)) + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
   
   #theme_classic() +
   
 # Two week ruler
 geom_text(x = as.Date("2021-05-15") + 7, y = 12, label = "Two weeks", color = brewer.pal(9, 'Greys')[6], hjust = 0.5, vjust = -0.4, size = 2) +
 geom_segment(x = as.Date("2021-05-15"), xend = as.Date("2021-05-15") + 14, y = 12, yend = 12, linetype = "solid", #lineend = "square",
              color = "#F5D000", size = 0.2) + # brewer.pal(9, "Greys")[6], # lighter yellow FFE033 # darker FFDA0A
 
   # Custom axis labels
   geom_text(x = -Inf, y = 15, label = "15 deaths", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 10, label = "10", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 5, label = "5", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   
   geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
   
   theme(#panel.grid = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
     axis.ticks.y = element_blank(),
     #axis.line = element_blank(),
     plot.margin=unit(x = c(0, 1, 0, 1), units = "line"),
     plot.title = element_text(face = "bold", size = 10), # family = "sans", hjust = 0.5, 
     #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
     legend.position = 'none', # c(0.125, 0.525), # 'none',
     #legend.key.size = unit(x = 0.75, units = 'line'), 
     #legend.title = element_text(size = 8), #legend.title = element_blank(),
     #legend.text = element_text(size = 7), #legend.title = element_blank(),
     axis.text.x = element_text(colour = brewer.pal(9, "Greys")[6]),
     axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
     axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
     axis.title.y = element_blank(), #element_text(size = 9),
     strip.background = element_blank()) +
   xlab("") + #ylab("Daily cases") +
   #            breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + 
   scale_x_date(labels = date_format("%b %d"),
                expand = expand_scale(mult = c(0, 0.04)),
                limits = c(date_chart_start - 1, date_range_end + 1),
                breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
   #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
   #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
   #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
   scale_y_continuous(limits = c(0, 16), 
                      breaks = c(5, 10, 15), 
                      expand = expand_scale(mult = c(0, 0.05))))# + , limits = c(0, 600)
#scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
#                  name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))

# SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/deaths_byAge_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
#print(deaths_byAge_PLOT)
#dev.off()

# SAVE TOGETHER

png(filename = paste0("~/Desktop/Taiwan_COVID_data/deathsANDcases_byAge_", timenow, ".png"), width = 5.5, height = 2.9, units = 'in', res = 500)
print(cases_byAge_PLOT / deaths_byAge_PLOT)
dev.off()




###############################################################################
# Day of week
case_data_projecting$interpretation_on_day_Confirmed_PLUS_Excluded_per1k <- case_data_projecting$interpretation_on_day_Confirmed_PLUS_Excluded / 1000
case_data_projecting$day_of_week <- as.character(format(case_data_projecting$date, "%a"))
case_data_projecting$is_Monday <- case_data_projecting$day_of_week == "Mon"
case_data_projecting$is_Monday <- factor(case_data_projecting$is_Monday, levels = c("TRUE", "FALSE"), labels = c("Monday", "Other"))

(tests_per_day_PLOT <- ggplot(data = filter(case_data_projecting, date >= as.Date("2021-05-17")), 
                              mapping = aes(x = date, y = interpretation_on_day_Confirmed_PLUS_Excluded_per1k)) +
    
    geom_bar(stat = 'identity', aes(fill = is_Monday)) +
    geom_text(aes(label = day_of_week), color = brewer.pal(9, "Greys")[7], hjust = 0.5, vjust = -0.25, size = 2) +
    geom_text(aes(label = paste0(round(interpretation_on_day_Confirmed_PLUS_Excluded_per1k), 'k')), color = brewer.pal(9, "Greys")[1], hjust = 0.5, vjust = 1.5, size = 2) +
    
    xlab("") + ylab("Tests reported") +
    
    # Custom axis labels
    geom_text(x = -Inf, y = 40, label = "40k daily tests reported", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 30, label = "30k", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 20, label = "20k", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    geom_text(x = -Inf, y = 10, label = "10k", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
    
    ggtitle(label = paste0("2021 Taiwan local COVID-19 cases   |   Daily tests reported   |   ", this_curr_todaystring)) + 
    
    geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
    
    theme(#panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
      axis.ticks.y = element_blank(),
      legend.key.size = unit(x = 1, units = 'line'),
      #axis.line = element_blank(),
      plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
      plot.title = element_text(face = "bold", size = 10), 
      legend.position = c(0.2, 0.65), # 'none',
      legend.title = element_blank(), # legend.title = element_text(size = 8), 
      #axis.text.x = element_blank(), # element_text(colour = brewer.pal(9, "Greys")[6]),
      axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
      axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
      axis.title.y = element_blank(), #element_text(size = 9),
      strip.background = element_blank()) +
    xlab("") + ylab("") +
    scale_fill_manual(values = brewer.pal(12, "Paired")[c(2,1)]) +
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0.04, 0.04))) + #,
                 #limits = c(date_chart_start - 1, date_range_end + 1),
                 #breaks = seq(as.Date("2021-05-17"), as.Date(timenow), by = INCUBATION_TIME)) + 
    scale_y_continuous(limits = c(0, 44), 
                       breaks = c(10, 20, 30, 40), 
                       expand = expand_scale(mult = c(0, 0.05))))

# SAVE TOGETHER
png(filename = paste0("~/Desktop/Taiwan_COVID_data/tests_per_day_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
print(tests_per_day_PLOT)
dev.off()



###############################################################################
###############################################################################
### UNITED STATES: comparison to a United States example
###############################################################################
###############################################################################

MIN_DATE_DISPLAYED_US <- as.Date("2020-03-01")
MAX_DATE_DISPLAYED_US <-  as.Date("2020-06-01") # as.Date("2020-04-15")

states_of_interest <- "Florida"


### case and death data: NYT GitHub
us_states_data <- read_csv("~/covid-19-data/us-states.csv")
(us_states_data <- filter(us_states_data, state %in% states_of_interest)) # 463 x 5

# should be equal numbers of dates and rows
nrow(us_states_data) # 463
length(unique(us_states_data$date)) # 463

# convert to NEW deaths and NEW cases, not cumulative
us_states_data <- dplyr::arrange(us_states_data, date)
us_states_data$new_cases <- 0
us_states_data$new_deaths <- 0

# initialized row 1
us_states_data[1, ]$new_cases <- us_states_data[1, ]$cases
us_states_data[1, ]$new_deaths <- us_states_data[1, ]$deaths

for(i in 2:nrow(us_states_data)) {
  us_states_data[i, ]$new_cases <- us_states_data[i, ]$cases - us_states_data[i-1, ]$cases
  us_states_data[i, ]$new_deaths <- us_states_data[i, ]$deaths - us_states_data[i-1, ]$deaths
  
}

#View(us_states_data)


### test data: https://beta.healthdata.gov/dataset/COVID-19-Diagnostic-Laboratory-Testing-PCR-Testing/j8mb-icvb/data
(us_states_test_data <- read_csv("~/Desktop/Taiwan_COVID_data/COVID-19_Diagnostic_Laboratory_Testing__PCR_Testing__Time_Series.csv")) # 73,988 x 9
names(us_states_test_data)[names(us_states_test_data) == "state"] <- "state_abbrev"
names(us_states_test_data)[names(us_states_test_data) == "state_name"] <- "state"
(us_states_test_data <- filter(us_states_test_data, state %in% states_of_interest)) #  1,380 x 9
(us_states_test_data <- pivot_wider(data = us_states_test_data, 
                                   names_from = overall_outcome, 
                                   values_from = c(new_results_reported, total_results_reported))) # 462 x 12
names(us_states_test_data)
#new_results_reported_Negative""total_results_reported_Negative"     
#"new_results_reported_Positive"           "total_results_reported_Positive"
#"new_results_reported_Inconclusive"   "total_results_reported_Inconclusive"

# should be equal numbers of dates and rows
nrow(us_states_test_data) # 462
length(unique(us_states_test_data$date)) # 462


### JOIN
us_states_data <- full_join(x = us_states_data, y = us_states_test_data, by = c("date", "state")) # 463 x 15

# add column for total tests performed
us_states_data$new_tests_performed <- us_states_data$new_results_reported_Negative + 
  us_states_data$new_results_reported_Positive +
  us_states_data$new_results_reported_Inconclusive
  



###############################################################################
### UNITED STATES: sliding window for CASES and DEATHS

### FILTER for recent
us_states_data_filtered <- filter(us_states_data, date >= MIN_DATE_DISPLAYED_US, date <= MAX_DATE_DISPLAYED_US)
# 62 x 15

# CREATE NEW SLIDING WINDOW DATA FRAME
cases_deaths_age_US_sw_data = tibble(
  sw_start_date = NA,
  sw_end_date = NA,
  
  # Cases
  sw_cases_sum = NA,
  sw_cases_mean = NA,
  sw_cases_n = NA,
  
  # Deaths
  sw_deaths_sum = NA,
  sw_deaths_mean = NA,
  sw_deaths_n = NA,
  
  # Tests
  sw_tests_sum = NA,
  sw_tests_mean = NA,
  sw_tests_n = NA
)

#cases_deaths_age_US_sw_data_NEWROW <- cases_deaths_age_US_sw_data

# perform sliding window
this_curr_age_date_US <- MIN_DATE_DISPLAYED_US
this_curr_age_date_US_MAX <- MAX_DATE_DISPLAYED_US

while (this_curr_age_date_US <= (this_curr_age_date_US_MAX - WINDOW_SIZE + 1)) {
  cat(this_curr_age_date_US, "")
  
  this_sw_start <- this_curr_age_date_US
  this_sw_end <- this_curr_age_date_US + WINDOW_SIZE - 1
  
  
  # Filter
  this_curr_age_date_US_data <- filter(us_states_data_filtered, date >= this_curr_age_date_US, date <= this_curr_age_date_US + WINDOW_SIZE - 1)
  #this_sw_num_days_with_data <- length(unique(this_curr_us_states_data_filtered$date))
  
  # Cases
  this_sw_cases_sum <- sum(this_curr_age_date_US_data$new_cases, na.rm = TRUE)
  this_sw_cases_mean <- sum(this_curr_age_date_US_data$new_cases, na.rm = TRUE) / WINDOW_SIZE
  this_sw_cases_n <- WINDOW_SIZE
  
  # Deaths
  this_sw_deaths_sum <- sum(this_curr_age_date_US_data$new_deaths, na.rm = TRUE)
  this_sw_deaths_mean <- sum(this_curr_age_date_US_data$new_deaths, na.rm = TRUE) / WINDOW_SIZE
  this_sw_deaths_n <- WINDOW_SIZE
  
  # Tests
  this_sw_tests_sum <- sum(this_curr_age_date_US_data$new_tests_performed, na.rm = TRUE)
  this_sw_tests_mean <- sum(this_curr_age_date_US_data$new_tests_performed, na.rm = TRUE) / WINDOW_SIZE
  this_sw_tests_n <- WINDOW_SIZE
  
  
  # ADD NEW ROW
  cases_deaths_age_US_sw_data_NEWROW = tibble(
    sw_start_date = this_sw_start,
    sw_end_date = this_sw_end,
    
    # Cases
    sw_cases_sum = this_sw_cases_sum,
    sw_cases_mean = this_sw_cases_mean,
    sw_cases_n = this_sw_cases_n,
    
    # Deaths
    sw_deaths_sum = this_sw_deaths_sum,
    sw_deaths_mean = this_sw_deaths_mean,
    sw_deaths_n = this_sw_deaths_n,
    
    # Tests
    sw_tests_sum = this_sw_tests_sum,
    sw_tests_mean = this_sw_tests_mean,
    sw_tests_n = this_sw_tests_n
  )
  
  if(is.na(cases_deaths_age_US_sw_data[1, ]$sw_start_date)) {
    cases_deaths_age_US_sw_data <- cases_deaths_age_US_sw_data_NEWROW
  } else {
    cases_deaths_age_US_sw_data <- rbind(cases_deaths_age_US_sw_data, cases_deaths_age_US_sw_data_NEWROW)
  }
  
  # increment the day
  this_curr_age_date_US <- this_curr_age_date_US + 1
}

#View(cases_deaths_age_US_sw_data)





###############################################################################
### CASES PLOT ###
(cases_byAge_US_PLOT <- ggplot(data = cases_deaths_age_US_sw_data, 
                            mapping = aes(x = sw_end_date, y = sw_tests_mean)) +
   
   # REVISED values <--- CHANGETHIS ***************
   #geom_bar(stat = "identity", fill = "#F9CFB4", position = position_dodge(width = 0)) + # , width = 10
   #geom_text(data = case_data_projecting, mapping = aes(label = New_Local_Cases_Revised), color = brewer.pal(9, 'Greys')[7], hjust = 0.5, vjust = -0.35, size = 1.5) +
   
   # CASES, DEATHS, TESTS: y-day windows
   #geom_bar(mapping = aes(x = sw_end_date, y = sw_tests_mean), stat = "identity", fill = brewer.pal(12, "Paired")[1], position = position_dodge(width = 0)) + # , width = 10
   #geom_line(mapping = aes(x = sw_end_date, y = sw_tests_mean), color = brewer.pal(12, "Paired")[2]) + #"#D55E00") + #color = brewer.pal(9, "Greys")[6]) + #color = brewer.pal(9, 'Greys')[6]) +
   
   geom_line(mapping = aes(x = sw_end_date, y = sw_cases_mean), color = brewer.pal(9, "Greys")[7]) + # "#0072B2") + # color = brewer.pal(9, "Greys")[8]) + #color = brewer.pal(9, 'Greys')[6]) +
   geom_line(mapping = aes(x = sw_end_date, y = sw_deaths_mean), color = brewer.pal(9, "Reds")[7]) + # color = "#009E73") + #color = brewer.pal(9, "Greys")[7]) + #color = brewer.pal(9, 'Greys')[6]) +
   
 
 # Custom axis labels
 geom_text(x = -Inf, y = 150, label = "150 cases", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 100, label = "100", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   geom_text(x = -Inf, y = 50, label = "50", color = brewer.pal(9, 'Greys')[5], hjust = 0, vjust = -0.4, size = 2.25) +
   
   
   geom_hline(yintercept = 0, color = brewer.pal(9, "Greys")[6]) +
   theme(#panel.grid = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted", size = 0.2),
     axis.ticks.y = element_blank(),
     #axis.line = element_blank(),
     plot.margin=unit(x = c(1, 1, 0, 1), units = "line"),
     plot.title = element_text(face = "bold", size = 10), # family = "sans", hjust = 0.5, 
     #plot.subtitle = element_text(hjust = 0.5, face = "bold"),
     legend.position = 'none', # c(0.125, 0.525), # 'none',
     #legend.key.size = unit(x = 0.75, units = 'line'), 
     #legend.title = element_text(size = 8), #legend.title = element_blank(),
     #legend.text = element_text(size = 7), #legend.title = element_blank(),
     axis.text.x = element_blank(), # element_text(colour = brewer.pal(9, "Greys")[6]),
     axis.text.y = element_blank(), # element_text(size = 9, colour = brewer.pal(9, "Greys")[6]),
     axis.ticks.x = element_line(colour = brewer.pal(9, "Greys")[6]),
     axis.title.y = element_blank(), #element_text(size = 9),
     strip.background = element_blank()) +
   xlab("") + #ylab("Daily cases") +
   #            breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + 
   scale_x_date(labels = date_format("%b %d"),
                expand = expand_scale(mult = c(0, 0.04)),
                limits = c(MIN_DATE_DISPLAYED_US - 1, MAX_DATE_DISPLAYED_US + 1),
                breaks = seq(as.Date(MIN_DATE_DISPLAYED_US), as.Date(MAX_DATE_DISPLAYED_US), by = INCUBATION_TIME)) +
   scale_y_continuous(#limits = c(0, 180), 
                      #breaks = c(50, 100, 150), 
                      expand = expand_scale(mult = c(0, 0.05))))# + , limits = c(0, 600)

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_byAge_US_", timenow, ".png"), width = 5.5, height = 3.2, units = 'in', res = 500)
print(cases_byAge_US_PLOT)
dev.off()


