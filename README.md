# FSANZ Foodborne Disease Costing Model
This repository contains the R code and data for estimating the burden and cost of food-borne illness, ten priority pathogens and four sequel illnesses in Australia circa 2019.

The code estimates the burden (cases, hospitalisations, and deaths) and estimates the associated costs (direct medical costs, lost productivity, pain and suffering, and fatality). Estimates can be run by executing `Rfiles/BuildCostTable.R`. Cost tables are outputted as .csv files, but can also be explored in an interactive shiny app (`app.R`). The app also allows users to estimate costs for foodborne disease outbreaks based on estimated number of cases, hospitalisations, and deaths using the same costing framework for the main estimates.

Web-hosted version of interactive table can be found at https://angusmclure.shinyapps.io/AusFBDCosting/

The development of this code was commissioned by Food Standards Australia New Zealand (FSANZ), conducted by Angus McLure at the Australian National University.

This code was used to calculate the estimates presented in the [FSANZ report]( https://www.foodstandards.gov.au/publications/Documents/ANU%20Foodborne%20Disease%20Final%20Report.pdf) *The annual cost of foodborne disease in Australia* (published 2022). The model and results are available as a [peer-reviewed journal article](https://doi.org/10.1089/fpd.2023.0015) in *Foodborne Pathogens and Disease*.

# Updating Data

This app uses the best data available to the ANU team in 2021 to estimate burden and cost of disease around the year 2019. There were many data inputs, some up-to-date as of the time of analysis, but others dating back to the early 2000s. Going forward it may be beneficial to update some or all of these data inputs to make better estimates of burden and cost. The estimation process is complex but involves three broad steps: estimation of burden, estimation of number of cost items, and multiplication of cost items by unit costs. Each of these steps require data, many of them specific to each pathogen being considered. The lists below describe the data, the source and list the names of the files where these data are stored in the model, with notes on how to update them.

Most data for the models are stored in the `Data` directory. Some of the pathogen-specific assumptions, especially those that are not easy to update regarding the number of cost items per case are written into the R code in `RFiles/Diseases.R` which also indicates which method (and therefore data sources) should be used to estimate the burden of the disease. If an additional pathogen needs to be added to the list it would require it's own entry in  `RFiles/Diseases.R` and matching input data (e.g. notifications for the pathogen). The functions that extract the data from the input files are mostly defined in `RFiles/loadData.R` and called at the start of the the main function of the programme (`Rfiles/BuildCostTable.R`).

Data for burden estimates include: 

- **Number of notifications for nationally notifiable pathogens (2019, NNDSS)**

  Each disease has a `caseMethod` attribute that determines how cases numbers are estimated. If `caseMethod = 'Notifications'`, then notifications for the year must be provided in the `Data` directory.

  For nationally notifiable diseases, these data are stored in `Data/NNDSS Disease by AgeGroup and Year.xslx` and read into the model by `getCasesNNDSSAgeGroup` in `Rfiles/loadData.R`. There are rows for the name of the disease and reporting year, followed by columns for each 5 year age-group (and 85+). Updating the data requires adding rows for each notifiable pathogen for each year for which estimates are required.

  For some data where surveillance data was only available or only how high quality from certain states, we used older notifications data for those states and performed population adjustment to get national estimates circa 2019. The data are read into the model and population adjusted by `getCasesStateAgeGroup` in `Rfiles/loadData.R`. It reads into population data for jurisdiction (from separate files for each jurisdiction in `Data`) both for the reporting years (2013-2015) and the estimation year (2019) by state, sex, and agegroup and performs adjustments accordingly. The format of the data are the same once read into the model and are combined into a single object `NotificationsAgeGroup`. Currently *Yersinia entercolictica* and STEC use this method. Updating this data will likely be on a case-by-case basis. It might be appropriate in the future for estimates to use national notifications data which will simplify the process.


- **Number of foodborne gastroenteritis events per person per year from the second national gastroenteritis survey (2008, NGSII)**

  This is hard-coded into `Rfiles/Diseases.R` as the `gastroRate` as an `rdist` object specifying the uncertainty distribution for this number.

- **The proportion of gastroenteritis cases due to specific pathogens from the Victorian water quality study (2001, Hellard) — used only for non-notifiable diseases.**

  This is hard-coded into `Rfiles/Diseases.R` for each disease. Each disease has a `caseMethod` attribute that determines how cases numbers are estimated. If `caseMethod = 'GastroFraction'`, then the disease also requires an attribute `gastroFraction` which is another `rdist` object specifying the fraction of gastro that is due to this pathogen.  

- **Seroprevalence studies for *Toxoplasma gondii* infection collected 2005-2007 from Busselton, Western Australia (2020, Molan)**

  This data is used to estimate the force of infection of the disease (units of infections per person per year) by fitting a very simple SIR model to the disease assuming the population is at endemic equilibrium and the force of infection is constant over time. The fitting is performed separately from this code. Each disease has a `caseMethod` attribute that determines how cases numbers are estimated. If `caseMethod = 'Seroprevalence'`, then the disease also requires an attribute `FOI` which is a `rdist` object specifying the force of infection estimate. Currently this is only used for Toxoplasmosis.

- **National population estimates (2019, ABS)**

  These are used to calculate the total number of gastro cases and the burden of the non-notifiable pathogens. These are stored in `AustralianPopulationByAge.xls` in the format published by the ABS (3101.0 Australian Demographic Statistics; TABLE 59. Estimated Resident Population By Single Year Of Age, Australia). Assuming that the ABS continues to use the same format going forward, one should be able to update the file by downloading the latest version of this and replacing the file. The data is read in by `getAusPopSingleYearAge` and further processed by `getAusPopAgeGroup` in `Rfiles/loadData.R`, and modifications may be required here if the ABS format changes.

- **Number of hospitalisations (mostly 2019 but some earlier, AIHW)**

  These data are used to estimate the number of hospitalisation for each disease. Each disease in `Rfiles/Diseases.R` has an attribute called `hospMethod`. If `hospMethod = 'AllCases'` then we assume that every case is hospitalised and therefore do not require additional data. If `hospMethod = 'AIHW'` then the reported number of hospitalisations with the principle diagnosis matching the codes listed in attribute `hospCodes` is extracted from AIHW datasets, then multiplied by `underdiagnosis` and divided by`hospPrincipalDiagnosis` attributes for the disease. If there are no hospitalisations reported for the estimation year, the program will use the average number of reported hospitalisations listed across all datasets available to the programme. AIHW reports by financial year and the current version of the software has data for financial years 2015-16 through 2019-20. For circa 2019 estimates we used FY 2018-2019 as the starting point, but expanded to all datasets if there were no hospitalisations reported for FY 2018-19. The data are stored in `Data` with names of the format `Principle_diagnosis_data_cube_20XX-YY.xlsx`. The data are read in by `getHospitalisationsAgeGroup` in `Rfiles/loadData.R` which assumes that all relevant data will be named with exact matching format. File format is  exactly as downloaded from AIHW website (e.g. [Admitted patient care NMDS 2019-20 (aihw.gov.au)](https://meteor.aihw.gov.au/content/699728)). Updating the model will require downloading the latest data and changing the file name to the matching format. If the format that AIHW uses changes in future releases there may be a requirement to re-write `getHospitalisationsAgeGroup`.

- **Deaths (2001-2010, ABS)**

  Each diseases listed in `Rfiles/Diseases.R` has a an attribute `mortCodes` listing the codes associated with this disease. Given the small number of deaths, there are sensitivities around these data and they are only available on request and with small random errors introduced intentionally by the ABS. Furthermore, as cause of death coding is not always complete, the number of deaths are underreported. We proceed by applying under-diagnosis multipliers but if the reported number of deaths were 0 for a given year, even multiplying by a large multiplier would have no effect. To deal with these complexities we used data from across a number of years, averaged and population adjusted up to the reporting years. Though we made no attempt to correct the random errors introduced by the ABS, we attempted to make a set of figures that were at least internally consistent (i.e. when looking at sums). This whole process is rather complicated and performed by `getABSDeaths` in `Rfiles/loadData.R` which reads from the file `Data/ABSDeathTable2001-2010.xlsx` and returns death rates by age-group for different causes of death that can be multiplied by population later as required. To update for future years this can either be left as is (at which the population adjustment will be performed for the latest population data), or reworked with newer/better data.

- **Multipliers (Various sources and assumptions)**

  There are a number of multipliers used to estimate burden. These are hard-coded for each disease in `Rfiles/Diseases.R` as attributes of each disease. Though different diseases use different multipliers depending on the method used to estimate the burden, these multipliers are `domestic` (fraction of cases that are domestically acquired) `underreporting` (factor to account for underreporting of cases) `foodborne` (fraction of cases that are foodborne), `hospPrincipalDiagnosis` (fraction of hospitalisations where the disease or related code is listed as the principle diagnosis) `underdiagnosis` (factor to account for under-diagnosis of the disease as the cause of death or hospitalisation). There are many different sources for each of these multipliers, and they can be updated by editing the matching attributes in `Rfiles/Diseases.R`.

Estimates of cost involve two broad steps. In step one we estimate the number of cost items (e.g. GP visits, doses of medication, time off work) based on burden estimates. In the second step we multiply cost items by unit prices. There are many cost items. Hospitalisation and deaths are two key cost items that are outlined in the previous section, however other cost items are calculated per case:

* **Direct costs**

  There are many kinds of direct costs considered and these are sufficiently different for each disease that they are hard-coded directly into `Rfiles/Diseases.R`. The sources and values are summarised in this file and report. 

* **Duration of illness**

  These are also inputted directly for each disease into the `Rfiles/Diseases.R` under the `duration` attribute, with entries for hospitalised and non-hospitalised cases

- **Days of lost work**

  For non-hospitalised cases the time off work was assumed to be the same for all initial infections and estimated from the Second National Gastroenteritis Survey (NGSII). The data is stored in and read in by `getMissedDaysGastro` from `Rfiles/loadData.R`. If this data were updated this would probably have to be reworked through the whole system as it is quite specific to NGSII. The number of days taken off work for hospitalised cases is based on the mean length of stay, extracted from the same AIHW files reporting the number of hospitalisation discussed in the previous section. This is combined with workforce participation rates (and average daily earnings) by age-group reported adapted from standard reports from the ABS. The data is stored in a custom format in `Data/WorkforceAssumptions.csv` and read into the model by `getWorkforceAssumptions` from `Rfiles/loadData.R`.  The data can be updated for the latest values by editing `Data/WorkforceAssumptions.csv`. If future updates wish to perform calculations for multiple timepoints, the csv file to include the published workforce data value for a range of years and modify `getWorkforceAssumptions` to read in all of these in a format where the program can select from a list of years.

Data to inform unit costs come from a range of sources:

- **Deaths**

  The cost of death used is Value of Statistical Life published by the Department of Prime Minister and Cabinet, in 2019. This is stored in `Data/ValueStatisticalLife.csv` and read in by `getValueStatisticalLife` from `Rfiles/loadData.R`. If future updates wish to perform calculations for multiple timepoints, the csv file to include the published VSL value for a range of years and modify `getValueStatisticalLife` to read in all of these  in a format where the program can select from a list of years.

- **Direct costs**

  As described above, the cost items for each diseases are listed in `Rfiles/Diseases.R` in a custom format. This format includes codes that can be used to lookup costs. The costs themselves are stored in `Data/Costs.xlsx` and read in by `getCosts` from `Rfiles/loadData.R`. The datasource is noted in the excel sheet and the report. In summary, MBS and PBS fees are taken from data published by Medicare (2019), and hospitalisation costs are linked to a AR-DRG (or an average of two such costs) deemed appropriate for the condition. IHCPA publishes costs for each AR-DRG for public hospitals that can be used to update this data. If future updates wish to perform calculations for multiple timepoints, the xlsx file to include the published costs  for a range of years and modify `getCosts` to read in all of these  in a format where the program can select from a list of years. Another option may be to read in directly from datasets provided by Medicare and IHCPA directly. However, there are some costs (e.g. for generic classes of drugs or non-PBS medicines) that are not published in public datasets and would need to be estimated an imported into the model separately.

- **Average weekly earnings (2020, ABS)** 

  This is a key cost that is multiplied by number of days of lost work. Data is stored in `Data/WorkforceAssumptions.csv` and read in by `getWorkforceAssumptions` from `Rfiles/loadData.R`.  If future updates wish to perform calculations for multiple timepoints, the csv file to include the published workforce data value for a range of years and modify `getWorkforceAssumptions` to read in all of these in a format where the program can select from a list of years.

- **Willingness to pay to avoid pain grief and suffering associated with disease estimated from a discrete choice experiment in Australia (2014, CHERE)**

  The willingness to be pay values (elicited in units of per day or per year) are multiplied by total estimated duration of illness (estimated as above) to get total cost of pain and suffering. As these values come from a study, any update will likely update the format of the data and thus the data importation and/or calculation process. However, currently the WTP values and descriptions of uncertainty are stored in `Data/WTPvaluesUncertainty.xlsx` and read into the model by `getWTP` in `Rfiles/loadData.R`.

  

# Adjusting for Inflation

The interactive shiny app includes the option to adjust all cost estimates for inflation. Using Dec 2019 as the baseline, users can select the financial quarter to adjust up to. To do this the program reads in quarterly inflation rates from `Data/CPI-ABS.csv` in the opening lines of `app.R`. This file is exactly in the format downloaded from the ABS from [here](https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release) (Accessed June 16 2023). Assuming that the ABS continues to use the same format for these downloads and continues to include dates back to Dec 2019, the `Data/CPI-ABS.csv` can be replaced by the latest release to include the latest CPI rates. However, if the ABS changes the format, the input file can be updated by appending the CPI values for the latest financial quarters in the a consistent format. Note that the program uses the 'change from previous quarter' column of the file, so any appended CPI rates should *not* be annualised.

Note also that this inflation adjustment only effects the cost estimates presented in the shiny app. It does not change any of the estimates in the core model or saved outputs of the model in `Outputs/` or `Report/`.  Even in the shiny app it does *not* update any of the underlying data, estimates of burden, or perform population adjustment to the latest population data. 

 

