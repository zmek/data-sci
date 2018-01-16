---
title: "Analysis of over 65s in South East England"
author: "Zella King"
date: "1/9/2018"
---

##Aims of this analysis 

This analysis focuses on how the population of older people, and their care needs, are expected to change over the 18 years. Britain has an ageing population, with higher levels of disease and longer periods of dependence towards end of life. It has been estimated that by 2025 there will be an additional 353K older people with substantial care needs.  

In this analysis we examine population changes at a very local level. We set out to identify which areas of the South East of England will experience the greatest demand for the delivery of care in the home. Since care at home is a very localised service to deliver, with agencies and staff limited in the distances they can cover, we wanted to see which geographical areas would have the highest need. Our intention is to help service providers interested in delivering care at home identify where the opportunity is greatest. 

This is an exploratory report, testing a data methodology, so it focuses on the South East of England only. However, the methodology is easily scalable to other regions. It can easily be extended to encompass more specific information, such as projections by gender, ethnic group, household composition, health needs. 

The data shown here could also be combined with other publicly available data such as on the wealth or education level of each district, immigration levels, or workforce data. 

##About the data used in this analysis

The data used in this analysis are taken from the Projecting Older People Population Information System (POPPI) database. Originally developed for the Department of Health, this provides population data by age band, gender, ethnic group, and tenure, for English local authorities.

POPPI estimates numbers of older people by: those living alone, living in care home, provision of unpaid care, their ability to carry out domestic tasks and self care.

Prevalence rates from research are used to estimate the numbers of people suffering from: limiting long term illness, depression, severe depression, dementia, heart attack, stroke, bronchitis, emphysema, falls, continence, visual impairment, hearing impairment, mobility, obesity, diabetes and learning disabilities including Down's syndrome and autistic spectrum disorders (ASD).

Anyone with an academic affiliation can access the POPPI data.

More information on POPPI can be found at: http://www.poppi.org.uk/

##How the data were accessed

Data were downloaded for the South East of England, and all the authorities within that region, by district. For each district, the following data were accessed:

* Population aged 65 and over, by age, for 2017, 2018, 2019, 2020, 2021, 2025, 2030 and 2035  
* People aged 65 and over with a limiting long-term illness, by age, projected for the same years as above  

There is variation between counties in the South East in terms of how they are organised. Some local authorities cover a whole county (e.g. Oxfordshire, Surrey) while other counties (e.g. Berkshire) have been divided into unitary authorities (e.g. Reading). The county level data needd to be broken down into smaller units in order for it to be comparable with unitary authority level data.

To achieve this, multiple downloads from POPPI were conducted at different levels of analysis:

1. South East of England level and the authorities within the South East (some of these are county level, others are district/unitary authority)  
1. For local authorities that cover a whole county, data at the district level was also downloaded  

Separate programmes have been written to process the whole population data and the illness-specific data. To use these programmes, you first need to download the relevant files from POPPI into a single folder for each (one folder containing downloads for population, one for illness) and edit the programmes to look for the files in the correct directory. 

##About the R programming files

 

