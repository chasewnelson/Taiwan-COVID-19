<img src="https://github.com/chasewnelson/SARS-CoV-2-ORF3d/blob/master/images/cover_image.png?raw=true" title="Overlapping gene products" alt="Overlapping gene products" align="left" size="small">

# Taiwan COVID-19 Analyses (2021)
Data, methods, and results of analyses tracking the May 2021 COVID-19 outbreak in Taiwan.


## <a name="contents"></a>Contents

* [Description](#description)
* [Data Sources](#data-sources)
* [Methods](#methods)
* [Results](#results)
* [Acknowledgments](#acknowledgments)
* [Contact](#contact)


## <a name="description"></a>Description

These charts depict the ongoing case numbers in Taiwan since an outbreak occurred in May, with Taipei declaring a Level 3 Alert on May 15. Data are recorded as they are announced by the Ministry of Health and Welfare (MOHW) each day at 14:00 Taipei Time. 

This repository collects the data shown each day and the analysis methods (R script) used. Specifically:

* The `data` folder contains the data displayed, by date
* The `visualizations` folder contains the visualizations, by date
* The R script `Taiwan_COVID19_data.R` contains the code used to produce the numerical results and visualizations

I focus on a few key metrics, and make no attempt to be thorough (for example, I do not currently track deaths). My purpose is not to provide a complete picture, but simply provide new ways of understanding the case counts and what they might tell us about the trajectory of the outbreak. All opinions my own.


## <a name="data-sources"></a>Data Sources

Raw data were retrieved from the following sources:

1. Taiwan CDC website: 

`https://data.cdc.gov.tw/dataset/daily-cases-suspected-sars-cov-2-infection_tested`
`https://data.cdc.gov.tw/en/dataset/covid19_tw__stats`

2. Taiwan Ministry of Health and Welfare (MOHW)

`https://www.facebook.com/mohw.gov.tw`
`https://twitter.com/MOHW_Taiwan`

2. Open Data Public Collaborations

`https://docs.google.com/spreadsheets/d/1qh20J-5rGVIEjLcGKJnfj7huAp-nCxsd-fJdmh3yZKY/htmlview#`
`https://docs.google.com/spreadsheets/d/12tQKCRuaiBZfc9yDd6tmlOdsm62ke_4AcKmNJ6q4gdU/htmlview#`


## <a name="methods"></a>Methods

Data were downloaded or manually recorded from the aforemenetioned sources. All statistical analyses were performed in Micosoft Excel and R version 3.5.2 (2018-12-20) Eggshell Igloo (<a target="_blank" href="https://www.R-project.org/">R Development Core Team</a>). Specifically, the R script `Taiwan_COVID19_data.R` was used to analyze them and produce raw visualizations. Note that this script is meant to be run manually, line-by-line in a program such as RStudio. This forces the user (e.g., me!) to inspect the results as they are analyzed, check for error, and make any changes necessary due to a new day's results. Visualizations were produced in R and modified in Microsoft PowerPoint.

## <a name="results"></a>Results

This section demonstrates the results produced for the data of May 30, 2021 (20210530). This section will not be regularly updated; instead, results will be posted to <a target="_blank" href="https://twitter.com/chasewnelson">Twitter</a> and <a target="_blank" href="https://www.facebook.com/chasewnelson">Facebook</a>. 

Because of a limited testing capacity, case numbers have been regularly assigned to previous days, process referred to as 'backlogging'. This method achieves more accurate counts for individual days. However, it is not necessarily more accurate for comparing between different days. Specifically, while results are still unfinished, it can create illusion of a downward trend (drop in case numbers) as an artifact, as brilliantly explained by [Linc_tw](https://twitter.com/Linc_tw/status/1397207338059276297).

These results attempt to demonstrate the effect backlogging may be having on perceived trends, and to clarify the meaning of the revised data being reported.


### Time-lapse of Cases

<img src="https://github.com/chasewnelson/SARS-CoV-2-ORF3d/blob/master/images/cover_image.png?raw=true" title="Overlapping gene products" alt="Overlapping gene products" align="left" size="small">

Results are depicted by DAYS LATE (shade of red). Late additions to previous days (backlogging) can create the illusion of a downward trend, because recent days will be disproportionately underestimated, having had fewer chances (days) to 'score points' from backlogging. Put another way, many more tests are 1-day-late than 10-days-late. Thus, these lighter bar portions grow larger closer to the present (right) because they are predicted to be less complete: future revisions should change today’s total a lot more than totals from a week ago.


### Seven-Day Average of Total Reported

<img src="https://github.com/chasewnelson/SARS-CoV-2-ORF3d/blob/master/images/cover_image.png?raw=true" title="Overlapping gene products" alt="Overlapping gene products" align="left" size="small">

Seven (7)-day averages (red line) using non-backlogged totals REPORTED each day (gray bars). Each day is the mean of itself & the previous 6.


### Backlog Time Distribution

<img src="https://github.com/chasewnelson/SARS-CoV-2-ORF3d/blob/master/images/cover_image.png?raw=true" title="Overlapping gene products" alt="Overlapping gene products" align="left" size="small">

On this day the backlog is ~35k tests. To that point, 21.3% of tests had been assigned to the previous day; 17.5% to the day before that; and so on. This should allows a guess of how the current backlog will be distributed.


### Projected Totals After Revision

<img src="https://github.com/chasewnelson/SARS-CoV-2-ORF3d/blob/master/images/cover_image.png?raw=true" title="Overlapping gene products" alt="Overlapping gene products" align="left" size="small">

For this date, given 1.8% of the ~35k backlog will test positive and be distributed to previous dates in the same manner observed so far, Albert Lin 林書弘 and I made this projection.


## <a name="acknowledgments"></a>Acknowledgments

Mitch (Ming-Hsueh) Lin 林明學 was critical for conceiving visualizations. Albert Lin 林書弘, Mitch (Ming-Hsueh) Lin 林明學, and Alexis (surname unknown) provided critical insights and feedback. The Taiwan CDC and MOHW, as well as the open source sharing sharing community (including Samuel Liu) in Taiwan were essential for openly sharing and compiling data. Finally, a big thanks to the government, medical, and science workers who have made Taiwan — still — the Safest Place On Earth. We're lucky to live in this country. It should be <a target="_blank" href="https://www.nature.com/articles/d41586-020-00693-2">part of the World Health Organization (WHO)</a>.


## <a name="contact"></a>Contact and troubleshooting

If you have questions about the scripts or result, please first thoroughly read the documentation and in-line comments relevant to the script of interest. If these do not answer your question, please click on the <a target="_blank" href="https://github.com/chasewnelson/SARS-CoV-2-ORF3d/issues">Issues</a> tab at the top of this page and search to see if your question has already been answered; if not, begin a new issue, so that others might benefit from the discussion.

Other queries should be addressed to the corresponding author: 

*  Chase W. Nelson, cnelson <**AT**> gate <**DOT**> sinica <**DOT**> edu <**DOT**> tw
