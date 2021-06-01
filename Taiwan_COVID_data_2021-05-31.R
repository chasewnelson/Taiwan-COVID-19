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
#library(lubridate)
#library(ggimage)
#library(rsvg)
#library(svg)
#library(png)
#library(grid)
#devtools::install_github("clauswilke/ggtext")
#library(ggtext)
#library(emo)


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


# view
View(case_data)

# PLOT GLOBAL
#MIN_DATE_DISPLAYED <- as.Date("2021-04-20")
LABOR_DAY <- as.Date("2021-04-30")
LEVEL3_DAY <- as.Date("2021-05-15")
LEVEL3_DAY_COUNTRY <- as.Date("2021-05-19")
#INCUBATION_TIME <- round(5.7) # Ferretti et al. 2021 preprint
#MIN_DATE_DISPLAYED <- timenow - 4 * INCUBATION_TIME
INCUBATION_TIME <- round(5) # Ferretti et al. 2021 preprint
#MIN_DATE_DISPLAYED <- timenow - 5 * INCUBATION_TIME
MIN_DATE_DISPLAYED <- as.Date("2021-05-01")

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
png(filename = paste0("~/Desktop/Taiwan_COVID_data/propPos_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
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
#      sec.axis = sec_axis(~.*(max(case_data$sw_new_tests_sum, na.rm = TRUE) / max(case_data$sw_new_local_cases_sum, na.rm = TRUE)),
#                                           name = "Tests performed (last 7 days)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)
#View(filter(case_data, sw_end_date >= MIN_DATE_DISPLAYED))

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/testsPerformed_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
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
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cumSumRevised_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
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
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cumSumRevisedLINE_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
localCases_cumSumRevised_PLOT
dev.off()




###############################################################################
# PLOT 5 - original TOTALS as bars, their sliding window as line
(localCases_originalTotalsWindow_PLOT <- ggplot(data = filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED), 
                                                mapping = aes(x = sw_end_date, y = sw_new_local_cases_mean)) + # color = significant
   
   # LEVEL 3 ALERT Taipei
   geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 400, linetype = "dashed", color = brewer.pal(9, "Set1")[2], size = 0.3) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY, y = 400, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Set1")[2], hjust = 0.5, vjust = -0.25, size = 2.5) + #, size = 2.75) +
   
   # LEVEL 3 ALERT COUNTRYWIDE
   geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 550, linetype = "dashed", color = brewer.pal(9, "Set1")[2], size = 0.3) + # , size = 0.4) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY_COUNTRY, y = 550, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Set1")[2], hjust = 0.5, vjust = -0.25, size = 2.5) + #, size = 2) + #, size = 2.75) +
   
   
   # Show daily reported with bars
   geom_bar(mapping = aes(x = Date_of_Reporting, y = New_Local_Cases), fill = brewer.pal(9, 'Greys')[3], stat = "identity", color = "NA", alpha = 0.75) + # brewer.pal(9, 'Set1')[1]) + brewer.pal(9, 'Reds')[8]
   
   # Show 7-day window with line and error
   geom_line(color = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Reds')[8]
   geom_ribbon(mapping = aes(ymin = sw_new_local_cases_mean - sw_new_local_cases_SE, ymax = sw_new_local_cases_mean + sw_new_local_cases_SE), 
               alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Reds')[8]) + # brewer.pal(9, 'Set1')[1]) +
   
   ggtitle(label = "2021 Taiwan Local COVID-19 Cases Reported (7-Day Average)", subtitle = todaystring) + #, " / First-Day (Non-Backlogged) Values")) +
   
   #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
   theme_classic() +
   theme(panel.grid = element_blank(),
         plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
         legend.position = 'none',
         legend.title = element_blank(),
         plot.title = element_text(hjust = 0.5, size = 11), #, size = 12),
         plot.subtitle = element_text(hjust = 0.5, size = 9), #, size = 10),
         #axis.text.x = element_text(size = 7),
         #axis.text.x = element_blank(),
         axis.text.y = element_text(size = 9),
         axis.title.y = element_text(size = 9),
         strip.background = element_blank()) +
   xlab("") + ylab("Reported cases") +
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
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_reported_7dayWindow_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
localCases_originalTotalsWindow_PLOT
dev.off()




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
 
 # Show REVISED (TODAY, 5/31)
 geom_bar(mapping = aes(y = New_Local_Cases_Revised), fill = '#4B2206', stat = "identity", color = "NA") + # 
   
   ### MOVE DOWN AND ADD HERE ###
   # Show REVISED (5/30)
   geom_bar(mapping = aes(y = New_Local_Cases_20210530), fill = '#713309', stat = "identity", color = "NA") + # 
   ################
   
 # Show REVISED (5/29)
 geom_bar(mapping = aes(y = New_Local_Cases_20210529), fill = '#97440C', stat = "identity", color = "NA") + # 
   
 # Show REVISED (5/28)
 geom_bar(mapping = aes(y = New_Local_Cases_20210528), fill = '#BD550F', stat = "identity", color = "NA") + # 
   
 # Show REVISED (5/27)
 geom_bar(mapping = aes(y = New_Local_Cases_20210527), fill = '#E26612', stat = "identity", color = "NA") + # 
   
   # Show REVISED (5/26)
   geom_bar(mapping = aes(y = New_Local_Cases_20210526), fill = '#EE7E32', stat = "identity", color = "NA") + # 
   
   # Show REVISED (5/25)
   geom_bar(mapping = aes(y = New_Local_Cases_20210525), fill = '#F19455', stat = "identity", color = "NA") +
   
   # Show REVISED (5/24)
   geom_bar(mapping = aes(y = New_Local_Cases_20210524), fill = '#F4AB7B', stat = "identity", color = "NA") +
   
   # Show REVISED (5/23)
   geom_bar(mapping = aes(y = New_Local_Cases_20210523), fill = '#F7C3A1', stat = "identity", color = "NA") +
   
   # Show REVISED (5/22)
   geom_bar(mapping = aes(y = New_Local_Cases_20210522), fill = '#FADBC6', stat = "identity", color = "NA") +
   
   # Show ORIGINAL
   geom_bar(stat = 'identity', fill = "#5B9CD6", color = "NA") +
   
   # Show ORIGINAL with line?
   #geom_line(color = "#215583") + # brewer.pal(9, 'Reds')[8]
   
   #geom_text(mapping = aes(x = timenow - 5, y = max(case_data$New_Local_Cases_Revised, na.rm = TRUE)),
   #          color = brewer.pal(9, 'Greys')[7], label = "Daily", hjust = 0, vjust = -0.5) + # , vjust = 1
   
   #ggtitle("Local Cases in Taiwan Since April 23") +
   #geom_text(mapping = aes(x = mean(range(filter(case_data, Date_of_Reporting >= MIN_DATE_DISPLAYED)$Date_of_Reporting)), y = Inf),
   #          label = "Taiwan Local Cases\n(May 1-25)", vjust = 1.25, size = 4.25) +
   ggtitle(label = "2021 Taiwan Local COVID-19 Cases", subtitle = todaystring) + # vjust = 1.25, , size = 4.25
   
   #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "grey") +
   theme_classic() +
   theme(panel.grid = element_blank(),
         plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         legend.position = 'none',
         legend.title = element_blank(),
         #axis.text.x = element_text(size = 7),
         #axis.text.x = element_blank(),
         axis.text.y = element_text(size = 9),
         axis.title.y = element_text(size = 9),
         strip.background = element_blank()) +
   xlab("") + ylab("Daily cases") +
   scale_x_date(labels = date_format("%b %d"),
                expand = expand_scale(mult = c(0, 0)),
                limits = c(MIN_DATE_DISPLAYED - 1, timenow + 1), #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
                breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
   #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
   #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
   scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)))) #
#      sec.axis = sec_axis(~.*(max(case_data$sw_prop_tests_positive_CIhigh, na.rm = TRUE) / max(case_data$New_Local_Cases_Revised_CUMSUM, na.rm = TRUE)),
#                          name = "Prop positive (7-day window)"), expand = expand_scale(mult = c(0, .05)))) # + # expand = c(0, 0)
# MISSING VALUES OK -- days from which none assigned


# SAVE
#png(filename = paste0("~/Desktop/Taiwan_COVID_data/localCases_cumSumRevised_PLOT_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
png(filename = paste0("~/Desktop/Taiwan_COVID_data/cases_by_date_added_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
localCases_dateRevised_PLOT
dev.off()



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

# initialize columns up to 15 days late
MAX_DAYS_LATE <- 15
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
(days_late_observed <- sort(as.integer(unique(days_late_observed))))

# filter out if the number of days actually wasn't observed
case_data_forDelayLONG <- filter(case_data_forDelayLONG, days_delayed %in% days_late_observed) # 5,478 x 4
unique(case_data_forDelayLONG$days_delayed) # [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 15

case_data_forDelayLONG$days_delayed <- factor(x = case_data_forDelayLONG$days_delayed, 
                                              levels = days_late_observed,
                                              labels = c("On time", days_late_observed[2:length(days_late_observed)])) #0:MAX_DAYS_LATE)
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

# defined directory for saving charts
chart_dir <- "~/Desktop/Taiwan_COVID_data/TIME_LAPSE/"

# Loop
this_curr_date <- date_range_start
while (this_curr_date <= date_range_end) {
  # this_curr_date begins at "2021-05-10"
  #this_curr_date <- as.Date("2021-05-22")
  #this_curr_date <- as.Date("2021-05-24")
  #this_curr_date <- as.Date("2021-05-26")
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
  
  #FACTOR for only 11 categories, with days 10-12 pooled
  this_curr_date_data$days_delayed <- as.character(this_curr_date_data$days_delayed)
  this_curr_date_data[this_curr_date_data$days_delayed %in% c('10', '11', '12', '13', '14', '15'), ]$days_delayed <- "10-15"
  this_curr_date_data$days_delayed <- factor(x = this_curr_date_data$days_delayed,
                                             levels = c("On time", 1:9, "10-15"))
  
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
      geom_rect(xmin = as.Date("2021-05-07"), xmax = as.Date("2021-05-13"), ymin = 350, ymax = 450, fill = "#FFE033") +
      geom_text(x = as.Date("2021-05-10"), y = 400, label = this_curr_todaystring, fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
      
      # Attribution
      geom_text(x = as.Date("2021-05-07"), y = 325, color = brewer.pal(9, "Greys")[4], hjust = 0, vjust = 0.5, size = 1.6, 
                label = "Data: Taiwan CECC, MOHW\nAuthors: Chase W. Nelson & Mitch Lin") +
      
      # Counter for number added
      #geom_rect(xmin = as.Date("2021-05-07"), xmax = as.Date("2021-05-13"), ymin = 50, ymax = 250, fill = "white", color = "black") +
      #geom_text(x = as.Date("2021-05-10"), y = 200, label = "100", fontface = "bold", color = brewer.pal(9, "Reds")[8], hjust = 1.2, vjust = 0.5, size = 5) + #, size = 2.75) +
      #geom_text(x = as.Date("2021-05-10"), y = 200, label = "150", fontface = "bold", color = "#5B9CD6", hjust = -0.2, vjust = 0.5, size = 5) + #, size = 2.75) +
      #geom_text(x = as.Date("2021-05-10"), y = 200, label = "+", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5) + #, size = 2.75) +
      #geom_text(x = as.Date("2021-05-10"), y = 100, label = "=250", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
      geom_text(x = as.Date("2021-05-07"), y = 250-50, label = paste0("Total: ", total_count), fontface = "bold", color = brewer.pal(9, "Greys")[6], hjust = 0, vjust = 0.5, size = 4.75) + #, size = 2.75) +
      geom_text(x = as.Date("2021-05-07"), y = 205-50, label = paste0("Backlogged: ", backlog_count), fontface = "bold", color = brewer.pal(9, "Reds")[4], hjust = 0, vjust = 0.5, size = 2.6) + #, size = 2.75) +
      geom_text(x = as.Date("2021-05-07"), y = 175-50, label = paste0("Today: ", today_count), fontface = "bold", color = brewer.pal(9, "Blues")[5], hjust = 0, vjust = 0.5, size = 2.6) + # "#5B9CD6"
      
      # LOCKED dates
      #geom_image(mapping = aes(image = "~/Desktop/Taiwan_COVID_data/LOCK.svg"), size = 1) +
      #annotation_custom(grob = rasterGrob(image = LOCK_PNG, width = 200, height = 200, -Inf, Inf, -Inf, Inf), 
      #                  xmin = as.Date("2021-05-07"), xmax = as.Date("2021-05-12"),
      #                  ymin = 200, ymax = 400) +
      # current trendline
      #geom_line(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE, size = 1.0) +
      #geom_point(data = this_curr_date_data_CURR_MAX, mapping = aes(x = Date_of_Reporting, y = sum_after_delay_max), color = brewer.pal(11, "Spectral")[11], inherit.aes = FALSE) +
      
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
      
      ggtitle(label = "2021 Taiwan Local COVID-19 Cases") + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
      
      theme_classic() +
      theme(panel.grid = element_blank(),
            plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            legend.position = c(0.125, 0.525), # 'none',
            legend.key.size = unit(x = 0.75, units = 'line'), 
            legend.title = element_text(size = 8), #legend.title = element_blank(),
            legend.text = element_text(size = 7), #legend.title = element_blank(),
            #axis.text.x = element_text(size = 7),
            #axis.text.x = element_blank(),
            axis.text.y = element_text(size = 9),
            axis.title.y = element_text(size = 9),
            strip.background = element_blank()) +
      xlab("") + ylab("Daily cases") +
      scale_x_date(labels = date_format("%b %d"),
                   expand = expand_scale(mult = c(0, 0)),
                   limits = c(date_chart_start - 1, date_range_end + 1),
                   breaks = seq(date_chart_start, as.Date(timenow), by = INCUBATION_TIME)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
      #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
      #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
      #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
      scale_y_continuous(expand = expand_scale(mult = c(0, 0)), limits = c(0, 550)) +
      scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
                        name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))
  
  # SAVE
  png(filename = this_curr_date_filename, width = 5.5, height = 3.3, units = 'in', res = 500)
  print(localCases_daysDelayed_TIMELAPSE_PLOT)
  dev.off()
  
  
  #ggsave(filename = paste0(this_curr_date_filename, 'lala_ggsave.png'), plot = localCases_daysDelayed_TIMELAPSE_PLOT,
  #      width = 5.5, height = 3.3, device = 'png', units = 'in', dpi = 500)
  
  # increment the day
  this_curr_date <- this_curr_date + 1
}

####
## SAVE LATEST TODAY'S
this_curr_date_filename <- paste0("~/Desktop/Taiwan_COVID_data/cases_by_days_late_", timenow, ".png")
png(filename = this_curr_date_filename, width = 5.5, height = 3.3, units = 'in', res = 500)
print(localCases_daysDelayed_TIMELAPSE_PLOT)
dev.off()
####


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
(NUM_OUTSTANDING <- case_data_projecting[case_data_projecting$date == timenow, ]$not_yet_interpreted_TotalInspection_MIN_Completed)
(CURR_CUM_PROP_POS <- case_data_projecting[case_data_projecting$date == timenow, ]$test_total_positive_rate_Confirmed_DIV_Completed)
(NUM_OUTSTANDING_PREDICT_POS <- NUM_OUTSTANDING * CURR_CUM_PROP_POS)
(days_late_PREDICTED_ADDITIONS <- round(days_late_proportions * NUM_OUTSTANDING_PREDICT_POS))

# INITIALIZE and ADD THEM
case_data_projecting$Num_Local_Cases_Projected <- case_data_projecting$New_Local_Cases_Revised

for(this_days_late in 1:MAX_DAYS_LATE) {
  #this_days_late <- 1
  this_date <- timenow - this_days_late
  this_PREDICTED_ADDITION <- as.integer(days_late_PREDICTED_ADDITIONS[this_days_late])
  case_data_projecting[case_data_projecting$date == this_date, ]$Num_Local_Cases_Projected <- 
    case_data_projecting[case_data_projecting$date == this_date, ]$Num_Local_Cases_Projected + this_PREDICTED_ADDITION
}

# FINALLY, set current day's prediction to previous day's prediction so as not to bias trend
case_data_projecting[case_data_projecting$date == timenow, ]$Num_Local_Cases_Projected <- 
  case_data_projecting[case_data_projecting$date == timenow - 1, ]$Num_Local_Cases_Projected

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
  
  #View(window_case_data)
  
  if(new_PREDICTED_cases_data_count >= MIN_DATA_COUNT) { #  && new_tests_data_count >= MIN_DATA_COUNT
    # Add results to table
    sw_new_PREDICTED_cases_sum <- sum(window_case_data_projecting$Num_Local_Cases_Projected, na.rm = TRUE)
    sw_new_PREDICTED_cases_mean <- sw_new_PREDICTED_cases_sum / new_PREDICTED_cases_data_count
    sw_new_PREDICTED_cases_sd <- sd(window_case_data_projecting$Num_Local_Cases_Projected, na.rm = TRUE)
    sw_new_PREDICTED_cases_SE <- sw_new_PREDICTED_cases_sd / sqrt(new_PREDICTED_cases_data_count)
    
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_start <- lowest_day
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_center <- (lowest_day + highest_day) / 2
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_end <- highest_day
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_num_days_with_data <- nrow(window_case_data_projecting)
    
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_sum <- sw_new_PREDICTED_cases_sum
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_mean <- sw_new_PREDICTED_cases_mean
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_sd <- sw_new_PREDICTED_cases_sd
    case_data_projecting[case_data_projecting$day == lowest_day, ]$sw_PREDICTED_cases_SE <- sw_new_PREDICTED_cases_SE
    
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
#View(case_data_projecting)

### PLOT PROJECTED ###
(localCases_PROJECTED_PLOT <- ggplot(data = case_data_projecting, 
                                     mapping = aes(x = date, y = Num_Local_Cases_Projected)) +
    
    # PREDICTED values
    geom_bar(stat = "identity", fill = "#F7C3A1", position = position_dodge(width = 0)) + # , width = 10
    
    # CURRENT REVISED values
    geom_bar(mapping = aes(y = New_Local_Cases_Revised), stat = "identity", fill = "#EE7E32", position = position_dodge(width = 0)) + # , width = 10
    
    # ORIGINAL values
    geom_bar(mapping = aes(y = New_Local_Cases_Original), stat = "identity", fill = "#5B9CD6", position = position_dodge(width = 0)) + # , width = 10
    
    # Show 7-day window with line and error (from plot 5)
    geom_line(mapping = aes(x = sw_end_date, y = sw_PREDICTED_cases_mean), color = brewer.pal(9, 'Greys')[8]) +
   #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_PREDICTED_cases_mean - sw_PREDICTED_cases_SE, ymax = sw_PREDICTED_cases_mean + sw_PREDICTED_cases_SE),  
   #geom_ribbon(mapping = aes(x = sw_end_date, ymin = sw_PREDICTED_cases_mean - sw_PREDICTED_cases_sd, ymax = sw_PREDICTED_cases_mean + sw_PREDICTED_cases_sd), 
   #             alpha = 0.15, linetype = 0, fill = brewer.pal(9, 'Greys')[8]) + 
    
   # LEVEL 3 ALERT - Taipei
   geom_segment(x = LEVEL3_DAY, y = 0, xend = LEVEL3_DAY, yend = 525, linetype = "dashed", color = brewer.pal(9, "Greens")[8], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY, y = 525, label = "Level 3 Alert\n(Taipei)", color = brewer.pal(9, "Greens")[8], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
   # LEVEL 3 ALERT - COUNTRY
   geom_segment(x = LEVEL3_DAY_COUNTRY, y = 0, xend = LEVEL3_DAY_COUNTRY, yend = 585, linetype = "dashed", color = brewer.pal(9, "Greens")[8], size = 0.2) + #, size = 0.4) +
   geom_text(x = LEVEL3_DAY_COUNTRY, y = 585, label = "Level 3 Alert\n(countrywide)", color = brewer.pal(9, "Greens")[8], hjust = 0.5, vjust = -0.25, size = 2) + #, size = 2.75) +
   
#    # Date
    geom_rect(xmin = as.Date("2021-05-02"), xmax = as.Date("2021-05-10"), ymin = 425, ymax = 625, fill = brewer.pal(9, "YlGn")[3]) + # yellow
    geom_text(x = as.Date("2021-05-6"), y = 525, label = "Projected\nTotal", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +

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

    ggtitle(label = "2021 Taiwan Local COVID-19 Cases") + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
    
    theme_classic() +
    theme(panel.grid = element_blank(),
          plot.margin=unit(x = c(1, 1, 1, 1), units = "line"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          legend.position = c(0.125, 0.525), # 'none',
          legend.key.size = unit(x = 0.75, units = 'line'), 
          legend.title = element_text(size = 8), #legend.title = element_blank(),
          legend.text = element_text(size = 7), #legend.title = element_blank(),
          #axis.text.x = element_text(size = 7),
          #axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          strip.background = element_blank()) +
    xlab("") + ylab("Daily cases") +
    scale_x_date(labels = date_format("%b %d"),
                 expand = expand_scale(mult = c(0, 0)),
                 limits = c(date_chart_start - 1, date_range_end + 1)) + #??, drop = FALSE # , #  c(as.Date(time0 + 7), as.Date(time0 + 91 + 7)),
    #breaks = seq(as.Date(MIN_DATE_DISPLAYED), as.Date(timenow), by = INCUBATION_TIME))  + # by = "7 day"
    #limits = c(as.Date(time0 + 7), as.Date(time0 + 91)),#  + 7
    #breaks = seq(as.Date(time0 + 7), as.Date(time0 + 91), by = "14 day")) +#  + 7
    scale_y_continuous(limits = c(0, 650), expand = expand_scale(mult = c(0, 0.05))))# + , limits = c(0, 600)
    #scale_fill_manual(values = c("#5B9CD6", brewer.pal(9, "Purples")[3], brewer.pal(9, "RdPu")[2:3], brewer.pal(9, "Reds")[3:9]), 
    #                  name = "Days late", guide = guide_legend(reverse = TRUE), drop = FALSE))

# SAVE
png(filename = paste0("~/Desktop/Taiwan_COVID_data/projected_total_", timenow, ".png"), width = 5.5, height = 3.3, units = 'in', res = 500)
print(localCases_PROJECTED_PLOT)
dev.off()


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
    geom_text(aes(label = days_late_Percent), vjust = -0.2, size = rel(2.5)) +
#    #    # Date
#    geom_rect(xmin = as.Date("2021-05-02"), xmax = as.Date("2021-05-12"), ymin = 375, ymax = 575, fill = "#FFE033") +
#    geom_text(x = as.Date("2021-05-7"), y = 475, label = "Predicted\nFinal Revision", fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5, size = 5.5) + #, size = 2.75) +
#    
#    # Attribution
#    geom_text(x = as.Date("2021-05-02"), y = 335, color = brewer.pal(9, "Greys")[4], hjust = 0, vjust = 0.5, size = 1.6, 
#              label = "Data: Taiwan CECC, MOHW\nAuthors: Chase W. Nelson & Mitch Lin") +
    
  #ggtitle(label = "2021 Taiwan Local COVID-19 Cases") + #, subtitle = this_curr_todaystring) + # , " # vjust = 1.25, , size = 4.25
    
    theme_classic() +
    theme(#panel.grid.minor = element_blank(),
          #panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "lightgray"),
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
          #axis.ticks.y = element_blank(), # 
          strip.background = element_blank()) +
    xlab("Days ago") + ylab("Projected positive cases") +
    scale_x_reverse(limits = c(15.5, 0.5), breaks = 15:1) + 
    #scale_x_date(labels = date_format("%b %d"),
    #             expand = expand_scale(mult = c(0, 0)),
    #             limits = c(date_chart_start - 1, date_range_end + 1)) + 
    #scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), labels = percent_format()))
    scale_y_continuous(limits = c(0, 120), expand = expand_scale(mult = c(0, 0.05))))


png(filename = paste0("~/Desktop/Taiwan_COVID_data/backlog_time_distribution_", timenow, ".png"), width = 5.5, height = 2.75, units = 'in', res = 500)
print(localCases_ADDITIONS_PLOT)
dev.off()



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
