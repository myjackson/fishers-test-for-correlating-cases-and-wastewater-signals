### Title : Fisher's test for correlating wastewater SARS-CoV-2 data and clinical cases
## Written by Jangwoo Lee, PhD, Postdoctoral Fellow, University of Calgary, Calgary, AB, Canada (jangwoo.lee@ucalgary.ca)
## Related publication : Lee et al., 2023, Campus node-based wastewater surveillance enables COVID-19 case localization and confirms lower SARS-CoV-2 burden relative to the surrounding community, Submitted to Water Research

### Overview : This package includes an example of Fisher's test employed in Lee et al., 2023. For instance, the R code in this package correlates wastewater SARS-CoV-2 abundance with clinical cases under 'Day-0' scenario. For more details on this scenario, please refer to the supplementary document of our publication (Lee et al., 2023; submitted to Water Research).   

### Input Files :
#UofC_Covid_Tracking_Info_Weekly_Data.csv : This data include weekly averaged concentrations for SARS-CoV-2 N2, also PMMoV. This also includes weekly averaged cases for wastewater treatment plant (WWTP) that receives wastewaters coming from the university campus, also surrounding communities.
#UofC_Covid_Tracking_Info_Cases_Data_Summarized_PeriodTotal_v3_WeeklyCases.csv : Weekly averaged clinical cases for each wastewater monitoring catchment.  

### R scripts for Fisher's test : 
#UofC_SARS-CoV-2_Fischer_Test_Weekly_v2_PeriodAandB_N2_Day_0.R : This code performed Fisher's test for each wastewater monitoring catchment, and a consolidated data summary table where p-values are shown. 