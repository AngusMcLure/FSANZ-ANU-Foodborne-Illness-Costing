
## Introduction

This application summarises Australian national estimates of burden and cost of foodborne disease for ten priority pathogens and four sequel illnesses circa 2019. The work was commissioned by Food Standards Australia New Zealand and the Australian Commonwealth Department of Health. The full report and methodology has been published by FSANZ and can be found [here](https://www.foodstandards.gov.au/publications/Documents/ANU%20Foodborne%20Disease%20Final%20Report.pdf).

There are five tabs:

-   **Info**: (Current tab) Provides an overview of the tool, a summary of data inputs, and allows for inflation rate adjustments.

-   **Epi Summaries**: Summarises the estimated burden (cases, hospitalisations, and deaths) by age group, disease, and sequel illness.

-   **Cost Summaries**: Summarises the estimated cost of foodborne disease as totals and by four broad categories (Direct Costs, Lost Productivity, Pain and Suffering, and Deaths), by age-group, by disease, and by sequel illness.

-   **Cost Comparisons**: Allows exploration and comparison of the estimated costs broken down into nine components (and total), and by age group, disease, and sequel illness.

-   **Outbreak**: Allows users to use the same assumptions and methodology as used in national annual estimates. Users input the number of cases. If known, the user can input number of deaths or hospitalisations, otherwise the expected number of such events are estimated based on national rates estimated from the main analyses. Estimates of cost are provided in the same format as in 'Cost Summaries' tab and include uncertainty intervals*.

*Note: The uncertainty intervals (UI) in the 'Outbreak' tab capture only the uncertainty in the estimated expected cost of an outbreak with the provided inputs, not the range of costs that might be associated with a particular outbreak. This is most notable for small outbreaks and for rare but costly events like deaths. For instance, given the number of cases but an unknown number of deaths, the model may estimate an expected 0.10 deaths (UI 0.08 - 0.012 deaths) with an associated cost of \$490,000 (UI \$392,000 - \$588,000), calculated from Value of Statistical Life of $4.9 million. However an individual outbreak with the given parameters must have an integer number of deaths. An reasonably range might be 0-2 deaths (with zero deaths being most likely) so the reasonable range of death related costs would actually be \$0 - \$9,800,000 — much wider than the uncertainty in the expected cost of an outbreak.*

## Data

This app use the best data available to the team in 2021 to estimate burden and cost of disease around the year 2019. There were many data inputs, some up-to-date as of 2019-20, but others dating back to the early 2000s. Data for burden estimates include: 

-   Number of notifications for nationally notifiable pathogens (2019, NNDSS)

-   Number of foodborne gastroenteritis events per person per year from the second national gastroenteritis survey (2008, NGSII)

-   The proportion of gastroenteritis cases due to specific pathogens from the Victorian water quality study (2001, Hellard) --- used only for non-notifiable diseases.

-   Seroprevalence studies for Toxoplasma gondii infection collected 2005-2007 from Busselton, Western Australia (2020, Molan)

-   National population estimates (2019, ABS)

-   Number of hospitalisations (2019, AIHW)

-   Deaths (2001-2010, ABS)


Estimates of cost involve two broad steps. In step one we estimate the number of cost items (e.g. GP visits, doses of medication, time off work) based on burden estimates. In the second step we multiply cost items by unit prices. Data to inform the number of cost items comes from a range of data sources:

-   Number of days taken off work as estimated from the second national gastroenteritis survey (2008, NGSII)

-   Workforce participation rates (2020, ABS)

-   Expert elicitation (Various sources)

Data to inform unit costs come from a range of sources:

-   Statistical value of life (2019, Department of Prime Minister and Cabinet)

-   MBS and PBS fees (2019, Medicare)

-   Average weekly earnings (May 2020, ABS)

-   Willingness to avoid pain grief and suffering associated with disease estimated from a discrete choice experiment in Australia (2014, CHERE)

-   Hospitalisation costs based on the average cost of the best matching Australian Refined Diagnosis Related Group (2019, IHACPA)


## Inflation adjustment

All costs (across tabs `Cost Summaries`, `Cost Comparisons`, and `Outbreak`) can be adjusted for inflation by selecting the quarter from below. Inflation adjustment uses quarterly all group CPI rates published by the ABS to adjust costs estimates and 90% uncertainty intervals. This does **not** update any of the underlying data listed above or the burden estimate and does **not** adjust for population growth.
