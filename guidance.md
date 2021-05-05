---
title: "Guidance"
author: "Colin Mahony"
date: "26/04/2021"
output: html_document
---

# Model and Scenario Selection Guidance for ClimateBC

Colin Mahony<br>
Research Climatologist<br>
BC Ministry of Forests, Lands, Natural Resource Operations and Rural Development<br>
colin.mahony@gov.bc.ca

Released: April 26, 2021

### Purpose

This guidance is developed by the BC Ministry of Forests, Lands, Natural Resource Operations and Rural Development (FLNRORD) to provide best practices for users of ClimateBC and those evaluating projects prepared using ClimateBC data. This guidance is professional opinion and does not represent government policy. 

### Model Selection

There is broad scientific agreement that an ensemble of at least eight independent climate models are required to represent modeling uncertainties about climate change outcomes over large regions (Pierce et al. 2009, McSweeney et al. 2014, Cannon 2015, Wilcke and Bärring 2016). However, small ensembles of 3-5 GCMs may be adequate for studies that are limited to a small area or a single time of year. 

There is insufficient evidence to rank the GCM projections in terms of their realism for the BC climate. If a single representative projection is desired, the 13-GCM ensemble mean is likely more reliable than any single GCM projection (Pierce et al. 2009). 



```r
library(knitr)
kkzRank <- read.csv("data/kkzRank.csv")
kkzRank <- data.frame(Order=1:13, kkzRank)
kable(kkzRank, caption="Table 1. Predefined order of selection for subsets of the 13-model ClimateBC ensemble, for BC (first column) and individual ecoprovinces (subsequent columns)")
```



Table: Table 1. Predefined order of selection for subsets of the 13-model ClimateBC ensemble, for BC (first column) and individual ecoprovinces (subsequent columns)

| Order|BC            |BOP           |CEI           |COM           |GED           |NBM           |SBI           |SIM           |SOI           |TAP           |
|-----:|:-------------|:-------------|:-------------|:-------------|:-------------|:-------------|:-------------|:-------------|:-------------|:-------------|
|     1|EC-Earth3     |IPSL-CM6A-LR  |BCC-CSM2-MR   |MRI-ESM2-0    |MRI-ESM2-0    |ACCESS-ESM1-5 |EC-Earth3     |MRI-ESM2-0    |MRI-ESM2-0    |ACCESS-ESM1-5 |
|     2|CanESM5       |CanESM5       |CanESM5       |UKESM1-0-LL   |UKESM1-0-LL   |CanESM5       |CanESM5       |UKESM1-0-LL   |UKESM1-0-LL   |UKESM1-0-LL   |
|     3|MPI-ESM1-2-HR |UKESM1-0-LL   |UKESM1-0-LL   |ACCESS-ESM1-5 |ACCESS-ESM1-5 |MPI-ESM1-2-HR |GISS-E2-1-G   |CanESM5       |CanESM5       |MRI-ESM2-0    |
|     4|UKESM1-0-LL   |GISS-E2-1-G   |GISS-E2-1-G   |CanESM5       |CanESM5       |MRI-ESM2-0    |UKESM1-0-LL   |MPI-ESM1-2-HR |BCC-CSM2-MR   |CanESM5       |
|     5|GISS-E2-1-G   |MPI-ESM1-2-HR |EC-Earth3     |BCC-CSM2-MR   |IPSL-CM6A-LR  |EC-Earth3     |MPI-ESM1-2-HR |ACCESS-ESM1-5 |MPI-ESM1-2-HR |GFDL-ESM4     |
|     6|BCC-CSM2-MR   |EC-Earth3     |INM-CM5-0     |MPI-ESM1-2-HR |MPI-ESM1-2-HR |IPSL-CM6A-LR  |BCC-CSM2-MR   |INM-CM5-0     |IPSL-CM6A-LR  |MPI-ESM1-2-HR |
|     7|MIROC6        |GFDL-ESM4     |MPI-ESM1-2-HR |EC-Earth3     |BCC-CSM2-MR   |BCC-CSM2-MR   |MIROC6        |EC-Earth3     |INM-CM5-0     |GISS-E2-1-G   |
|     8|ACCESS-ESM1-5 |MIROC6        |IPSL-CM6A-LR  |INM-CM5-0     |INM-CM5-0     |UKESM1-0-LL   |CNRM-ESM2-1   |IPSL-CM6A-LR  |EC-Earth3     |BCC-CSM2-MR   |
|     9|CNRM-ESM2-1   |ACCESS-ESM1-5 |ACCESS-ESM1-5 |IPSL-CM6A-LR  |CNRM-ESM2-1   |MIROC6        |MRI-ESM2-0    |GFDL-ESM4     |ACCESS-ESM1-5 |EC-Earth3     |
|    10|MRI-ESM2-0    |MRI-ESM2-0    |MRI-ESM2-0    |GISS-E2-1-G   |GISS-E2-1-G   |CNRM-ESM2-1   |IPSL-CM6A-LR  |BCC-CSM2-MR   |GISS-E2-1-G   |MIROC6        |
|    11|IPSL-CM6A-LR  |INM-CM5-0     |CNRM-ESM2-1   |CNRM-ESM2-1   |EC-Earth3     |GISS-E2-1-G   |ACCESS-ESM1-5 |GISS-E2-1-G   |CNRM-ESM2-1   |IPSL-CM6A-LR  |
|    12|INM-CM5-0     |BCC-CSM2-MR   |GFDL-ESM4     |GFDL-ESM4     |MIROC6        |INM-CM5-0     |INM-CM5-0     |MIROC6        |MIROC6        |INM-CM5-0     |
|    13|GFDL-ESM4     |CNRM-ESM2-1   |MIROC6        |MIROC6        |GFDL-ESM4     |GFDL-ESM4     |GFDL-ESM4     |CNRM-ESM2-1   |GFDL-ESM4     |CNRM-ESM2-1   |



