<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/cover.png?raw=true" title="Generic trendline" alt="Generic trendline" align="left" size="small">

# Taiwan COVID-19 Data
Analysis and visualization of Taiwan’s COVID-19 data

* [Data for 2023](#data-2023)
* [Data for 2021](#data-2021)
* [Acknowledgments](#acknowledgments-2021)
* [Contact](#contact-2021)

**QUICK SUMMARY:** you can find the following two useful files in this repository:

1. `Taiwan-COVID19-data-2023.xlsx` - manual compilation of COVID-19 death data from Chinese-language Taiwan CDC press releases, along with URL sources. Note that many dates have two sources because press releases often repeat the numbers for the previous week. There are some slight discrepancies in date ranges, e.g., typos. Also available as a <a target="_blank" href="https://docs.google.com/spreadsheets/d/1TwjSULsSgCfXSxsyhMbkrYSgGk_68MeMRA54M7abRxU/edit?usp=sharing">Google Sheet</a>.
2. `owid-covid-data-Taiwan-only.csv` - a Taiwan-only version of the ***Our World in Data* COVID-19 dataset** that includes Taiwan's data from Johns Hopkins University (previously accessible) and death data from the Taiwan CDC (new). Simply replace Taiwan’s rows at OWID with those provided in this file. (Note: rows for Taiwan have been added.)

**LATEST COVID-19 UPDATE:** 2023-11-21

* Week of Nov 14–20: **24 deaths** 
* Cumulative deaths: **22,806 deaths (954.5 per million people)**

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/cumulative_deaths.png?raw=true" title="Cumulative deaths as of latest update" alt="Cumulative deaths as of latest update" align="left" size="small">

# <a name="data-2023"></a>Data for 2023

## <a name="contents-2023"></a>Contents

* [Description](#description-2023)
* [Data Sources](#data-sources-2023)
* [Methods](#methods-2023)
* [Results](#results-2023)

## <a name="description-2023"></a>Description

Taiwan's COVID-19 situation has become increasingly difficult to ascertain in 2023, due to several events:

1. **Feb 10:** JHU announces it will stop collecting COVID data
2. **Feb 24:** Taiwan stops daily reporting
3. **Feb 28:** OWID announces switch from JHU to WHO database
4. **Mar 8:** OWID switches to WHO; Taiwan disappears
5. **Mar 17:** Taiwan stops reporting cumulative deaths, etc.


This update has two purposes:

1. Estimate the cumulative number of COVID-19 deaths in Taiwan, using Taiwan CDC press releases for various time intervals
2. Provide a version of the ***Our World in Data* COVID-19 dataset** that includes Taiwan's data from Johns Hopkins University (accessible prior to Mar 8) and death data from the Taiwan CDC (new)

## <a name="data-sources-2023"></a>Data Sources

Data were retrieved from the following sources:

1. **JHU** (Johns Hopkins University), accessed 2023/10/21

`https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv`
`https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv`

2. **OWID** (Our World in Data), accessed 2023/10/21

`https://covid.ourworldindata.org/data/owid-covid-data.csv`

3. **TWCDC** (Taiwan CDC), assembled 2023/10/21

`https://www.cdc.gov.tw/`

## <a name="methods-2023"></a>Methods

Data were downloaded or manually recorded from the aforementioned sources. All statistical analyses were performed in R version 4.3.1 (2023-06-16) "Beagle Scouts" (<a target="_blank" href="https://www.R-project.org/">R Development Core Team</a>). Specifically, the R script `Taiwan-COVID19-data-2023.R` was used to analyze and join data and produce visualizations. Note that the R script is meant to be run interactively, line-by-line in RStudio.

**Estimating numbers of deaths.** The Taiwan CDC provided cumulative (total) numbers of deaths until <a target="_blank" href="https://www.cdc.gov.tw/Bulletin/Detail/AsyuhuNb0uZMzoJxccOugA?typeid=9">March 16, 2023</a> (18,656 deaths). Since that time, it has not provided cumulative numbers to the public. Instead, ~weekly Chinese-language press releases report integer-rounded averages per day. For example, in an <a target="_blank" href="https://www.cdc.gov.tw/Bulletin/Detail/WcWHyfVbjF05aooxyeUsQw?typeid=9">October 17, 2023 press release</a>, it was reported that there were an average of 6 COVID-19 deaths per day over the period 10/10–10/16. Because this period is 7 days long, it can be estimated that there were a total of 7 * 6 = 42 total deaths over this time. This calculation was performed for every period since March 16, with the estimated number of new deaths added to the previous period's total. 

Some non-disjoint (i.e., overlapping) date ranges were reported in the period 4/14–4/29; these were resolved as follows:

1. 13 deaths per day for the period 4/14–4/20 (7 days, total of 91 deaths)
2. 13 deaths per day for the period 4/21–4/27 (7 days, total of 91 deaths)
3. 12 deaths per day for the period 4/29–4/30 (2 days, total of 24 deaths)

Two days (8/20 and 8/21) were missing from reports; these were resolved as follows:

1. 13 deaths per day for the period 8/20–8/21 (2 days, total of 26 deaths); this was determined as the average of the previous (8/13–8/19, 11 deaths per day) and following (8/22–8/28, 15 deaths per day) periods

Note that because the reported averages are integer-rounded (e.g., 13 instead of 13.4) and cumulative numbers are sometimes subject to revision, these estimates may not exactly match the true cumulative numbers. For dates where both new and total cases or deaths were reported, any discpreancies between the two were resolved in favor of reported cumulative totals.

## <a name="results-2023"></a>Results & Files

The following files are included in this repository:

1. `Taiwan-COVID19-data-2023.xlsx` - manual compilation of COVID-19 death data from Chinese-language Taiwan CDC press releases, along with URL sources. Note that many dates have two sources because press releases often repeat the numbers for the previous week. There are some slight discrepancies in date ranges, e.g., typos. Also available as a <a target="_blank" href="https://docs.google.com/spreadsheets/d/1TwjSULsSgCfXSxsyhMbkrYSgGk_68MeMRA54M7abRxU/edit?usp=sharing">Google Sheet</a>.
2. `owid-covid-data-Taiwan-only.csv` - a Taiwan-only version of the ***Our World in Data* COVID-19 dataset** that includes Taiwan's data from Johns Hopkins University (previously accessible) and death data from the Taiwan CDC (new). Simply replace Taiwan’s rows at OWID with those provided in this file. (Note: rows for Taiwan have been added.)

Current results for various countries that continue to report are shown as of 2023-10-23 in OWID style below.

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/cumulative_deaths.png?raw=true" title="Cumulative deaths as of latest update" alt="Cumulative deaths as of latest update" align="left" size="small">

Time lapse and static versions of this comparison can be found in the files `cumulative_deaths.png` and `cumulative_deaths_time_lapse.mp4`.

***

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/cover.png?raw=true" title="Generic trendline" alt="Generic trendline" align="left" size="small">

# <a name="data-2021"></a>Data for 2021
Data, methods, and results of analyses tracking the 2021 COVID-19 outbreak in Taiwan.

## <a name="contents-2021"></a>Contents

* [Description](#description-2021)
* [Data Sources](#data-sources-2021)
* [Methods](#methods-2021)
* [Results](#results-2021)
	* [Backlogging](#backlogging-2021)
	* [Time-Lapse of Cases](#time-2021)
	* [Cases By Date Added](#cases-2021)
	* [Seven-Day Average of Total Reported Cases](#seven-2021)
	* [Backlog Time Distribution](#backlog-2021)
	* [Projected Totals After Revision](#projected-2021)
* [Notes and Clarifications](#notes-2021)

## <a name="description-2021"></a>Description

These charts depict the ongoing case numbers in Taiwan since an outbreak began in early May 2021, with Taipei declaring a Level 3 Alert on May 15. Data are recorded as they are announced by the [Ministry of Health and Welfare](https://www.facebook.com/mohw.gov.tw) (MOHW) each day at 2pm (14:00) Taipei Time. 

This repository collects the data shown each day and the analysis methods used. Specifically:

* The `data` folder contains data displayed on each date
* The `visualizations_2021` folder contains visualizations by date
* The R script `Taiwan_COVID19_data.R` contains the code used to produce numerical results and visualizations

I focus on a few key metrics, and make no attempt to be thorough. For example, I do not currently track deaths or *R*<sub>t</sub>, which are [metrics that lag far behind cases](https://www.nature.com/articles/d41586-020-02009-w/). My purpose is not to provide a complete picture, but rather clear ways of understanding the case counts as they are revised, and what they might tell us about the trajectory of the outbreak. All opinions are my own.


## <a name="data-sources-2021"></a>Data Sources

Raw data were retrieved from the following sources:

1. Taiwan CDC 

`https://data.cdc.gov.tw/dataset/daily-cases-suspected-sars-cov-2-infection_tested`
`https://data.cdc.gov.tw/en/dataset/covid19_tw__stats`
`https://data.gov.tw/dataset/120451`
`https://data.cdc.gov.tw/dataset/daily-cases-suspected-sars-cov-2-infection_tested`
`https://data.cdc.gov.tw/en/dataset/agsdctable-day-19cov`

2. Taiwan Ministry of Health and Welfare (MOHW)

`https://www.facebook.com/mohw.gov.tw`
`https://twitter.com/MOHW_Taiwan`

3. 天下 (Commonwealth)

`https://web.cw.com.tw/covid-live-updates-2021-en/index.html`

4. Open Data Public Collaborations (g0v, etc.)

`https://docs.google.com/spreadsheets/d/1qh20J-5rGVIEjLcGKJnfj7huAp-nCxsd-fJdmh3yZKY/htmlview#`
`https://docs.google.com/spreadsheets/d/12tQKCRuaiBZfc9yDd6tmlOdsm62ke_4AcKmNJ6q4gdU/htmlview#`

4. NHCH (death data)

`https://covid-19.nchc.org.tw/deathstatistics.php?dt_name=1&downloadall=yes`


## <a name="methods-2021"></a>Methods

Data were downloaded or manually recorded from the aforementioned sources. All statistical analyses were performed in Microsoft Excel and R version 3.5.2 (2018-12-20) "Eggshell Igloo" (<a target="_blank" href="https://www.R-project.org/">R Development Core Team</a>). Specifically, the R script `Taiwan_COVID19_data.R` was used to analyze data and produce raw visualizations, which were then modified in Microsoft PowerPoint. GIFs were created using [GIFMaker.me](https://gifmaker.me/). Note that the R script is meant to be run manually, line-by-line in an interactive program such as RStudio. This forces the user (e.g., me!) to inspect the results as they are analyzed, check for errors, and make any changes necessary due to peculiarities in each day's new results.


## <a name="results-2021"></a>Results

I here provide demonstrations of the results produced for the date of June 5, 2021 (20210605). The charts will not be regularly updated; instead, updates will be posted to <a target="_blank" href="https://twitter.com/chasewnelson">Twitter</a> and <a target="_blank" href="https://www.facebook.com/chasewnelson">Facebook</a>. 

### <a name="backlogging-2021"></a>Backlogging

Because of an initially limited testing capacity, tests have been delayed and results routinely assigned to previous days, a method referred to as 'backlogging'. Backlogging achieves increasingly accurate counts for individual days. However, it also introduces difficulties for comparing *between* different days. Specifically, while results are still unfinished, it can create the illusion of a downward trend (drop in case numbers) as an artifact, because recent days will be disproportionately underestimated. This has been brilliantly explained by [Linc_tw](https://twitter.com/Linc_tw/status/1397207338059276297).

The visualizations below demonstrate the effect backlogging may be having on perceived trends, and to clarify the meaning of the revised data being reported. Finally, we provide a projection of finalized counts as a function of the current backlog and its known time distribution.


### <a name="time-2021"></a>Time-Lapse of Cases

Results below are depicted by **days late** (shade of red). Late additions to previous days (backlogging) can create the illusion of a downward trend, because recent days will be disproportionately underestimated, having had fewer chances (days) to 'score points' from backlogging. Put another way, many more tests are 1-day-late than 10-days-late. Thus, these lighter bar portions grow larger closer to the present (right) because they are predicted to be less complete: future revisions should change today’s total a lot more than totals from a week ago. Based on the backlogging pattern observed thus far, we considered dates >9 days in the past relatively ‘locked in’ (unlikely to change substantially).

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/visualizations_2021/time_lapse_20210605.gif?raw=true" title="Time-lapse of Cases" alt="Time-lapse of Cases" align="left" size="small">


### <a name="cases-2021"></a>Cases by Date Added

Results below are instead depicted by **date added** (shade of orange) during backlogging. This is helpful because the original same-day values (blue bars, following the MOHW color scheme), a better metric of trend than the revised values, can be clearly seen.

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/visualizations_2021/cases_by_date_added_20210605.png?raw=true" title="Cases by date added" alt="Cases by date added" align="left" size="small">


### <a name="seven-2021"></a>Seven-Day Average of Total Reported

Results below are shown as 7-day averages (red line) using non-backlogged TOTALS REPORTED each day (gray bars). Each day is the mean of itself and the previous 6 days. This provides a decent measure of trend when outstanding tests remain unfinished and the extent of possible revisions to previous dates is uncertain.

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/visualizations_2021/cases_reported_7dayWindow_20210605.png?raw=true" title="Seven-Day Average of Total Reported" alt="Seven-Day Average of Total Reported" align="left" size="small">


### <a name="backlog-2021"></a>Backlog Time Distribution

On the day shown (June 5, 2021) the backlog was ~10k tests. To that point, 24% of tests had been assigned to the previous day; 19% to the day before that; and so on. This allowed a projection of how the current backlog would be distributed (next section).

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/visualizations_2021/backlog_time_distribution_20210605.png?raw=true" title="Backlog Time Distribution" alt="Backlog Time Distribution" align="left" size="small">


### <a name="projected-2021"></a>Projected Totals After Revision

For this date (June 5, 2021), 1.8% of recent targeted tests (likely cases) had been positive. Given this, a simple estimate was that 1.8% of the ~10k backlog would test positive and be distributed to previous dates in the same manner (distribution) observed up to that point in time. Based on this reasoning, Albert Lin 林書弘 and I made the projection below, which is an estimate of the final counts if the full current backlog were completed the next day. Conceptually, it simply takes the projected backlog numbers implied by the [backlog time distribution](#backlog) (above) and places it on top of the current revised counts.

<img src="https://github.com/chasewnelson/Taiwan-COVID-19/blob/main/visualizations_2021/projected_total_20210605.png?raw=true" title="Projected Totals After Revision" alt="Projected Totals After Revision" align="left" size="small">


## <a name="notes-2021"></a>Notes and Clarifications 2021

1. For the date of May 30, 2021 (20210530), the MOHW retracted the backlogged numbers provided at press time. The analyses for that day use their [updated values](https://twitter.com/MOHW_Taiwan/status/1398166785363349505), released later that day shortly before 5pm (17:00) Taipei Time.


## <a name="acknowledgments-2021"></a>Acknowledgments

Mitch (Ming-Hsueh) Lin 林明學 was critical for conceiving visualizations. Albert Lin 林書弘, Mitch (Ming-Hsueh) Lin 林明學, and Alexis (surname unknown) provided critical insights and feedback on the 2021 data. The Taiwan CDC and MOHW, as well as the open source data sharing community in Taiwan (including Samuel Liu and g0v) were essential for openly sharing and compiling data. Finally, a big thanks to everyone in Taiwan, a country that <a target="_blank" href="https://www.nature.com/articles/d41586-020-00693-2">should be part of the World Health Organization (WHO)</a>.


## <a name="contact-2021"></a>Contact and troubleshooting

If you have questions about the methods or results, please first thoroughly read the description and in-line comments relevant to the analysis of interest. If these do not answer your question, please click on the <a target="_blank" href="https://github.com/chasewnelson/Taiwan-COVID-19/issues">Issues</a> tab at the top of this page and search to see if your question has already been answered; if not, please begin a new issue, so that others might benefit from the discussion.

Other queries should be addressed to the author: 

*  Chase W. Nelson 倪誠志, cnelson <**AT**> amnh <**DOT**> org