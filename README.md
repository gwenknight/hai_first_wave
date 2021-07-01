# hai_first_wave
Code to support estimates of the contribution of hospital-associated transmission to the English COVID-19 epidemic in the first wave

# Paper information: 

Title: The contribution of hospital-acquired infections to the COVID-19 epidemic in England in the first half of 2020

Authors: G M Knight, Thi Mui Pham, James Stimson, Sebastian Funk, Yalda Jafari, Diane Pople, Stephanie Evans, Mo Yin, Alex Bhattacharya, Russell Hope, Malcolm G Semple, ISARIC4C Investigators, CMMID working group, Jonathan M Read, Ben Cooper, Julie Robotham

Location: TBD (medrXiv)

# Aims and context
The code uses COVID-19 hospital case and length of stay data from two non-open datasets (CO-CIN and SUS) to infer how many hospital-acquired infections may have happened in English acute Trusts in the first wave (Jan - July 2020). 

## Overview_all.R 
This outlines the steps in the analysis with the linked code. Each part of the analysis has a separate file which are listed and explained in this overview file. These stages broadly reflect the stages of the analysis in Figure 2 of the paper. 

# mock_results.R
To generate results for a mock trust this code runs the simulation given the mock data in the data file. 

# Required packages: 
here / lubridate / tidyverse / reshape2 / data.table / zoo / patchwork / RColorBrewer / boot

# Data
We cannot provide access to CO-CIN and SUS due to the sensitive nature of this clinical data. 

Access to CO-CIN can be requested through the ISARIC consortium: https://isaric4c.net/

Access to SUS is through the NHS: https://digital.nhs.uk/services/secondary-uses-service-sus

The data folder includes rt estimates and mock data for the length of stay and incidence for a "mock" trust to allow the final results to be generated.

