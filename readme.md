---
title: "Biomass estimation for FIRE-RES WP5.2??"
author: "Francesco Pirotti, Erico Kutchartt"
date: "26/7/2022"
output: html_document
---
 
## Data

The following data sources are used:


####  Dependent variable: 

 - Above Ground Biomass. We used as proxy the biomass from CEDA Santoro et al. 
    - Year: 2018
    - Resolution:100 m
    - Layers: 
      - **Above ground biomass**
      - **Above ground biomass standard error**

####  Independent variables (aka descriptors): 

 - [COPERNICUS_Landcover_100m_Proba-V-C3_Global](https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_Landcover_100m_Proba-V-C3_Global){target=_blank}
    - Year: 2019
    - Resolution:100 m
    - Layers: 
      - **land cover category**
      - **forest type**
      - **forest canopy cover %**
  
 - Canopy height from ETH ....
    - Year: 2019??
    - Resolution: 10 m
    - Layers: **canopy height**
 - [Bioclimatic variables from WorldClim (Berkeley University)](developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_BIO){target=_blank}
    - Year: 1960-1991
    - Resolution: 1000 m
    - Layers: 
		- **bio01**	Annual mean temperature	-290	320	°C	0.1
		- **bio02**	Mean diurnal range (mean of monthly (max temp - min temp))	9	214	°C	0.1
		- **bio03**	Isothermality (bio02/bio07)	7	96	%	0
		- **bio04**	Temperature seasonality (Standard deviation * 100)	62	22721	°C	0.01
		- **bio05**	Max temperature of warmest month	-96	490	°C	0.1
		- **bio06**	Min temperature of coldest month	-573	258	°C	0.1
		- **bio07**	Temperature annual range (bio05-bio06)	53	725	°C	0.1
		- **bio08**	Mean temperature of wettest quarter	-285	378	°C	0.1
		- **bio09**	Mean temperature of driest quarter	-521	366	°C	0.1
		- **bio10**	Mean temperature of warmest quarter	-143	383	°C	0.1
		- **bio11**	Mean temperature of coldest quarter	-521	289	°C	0.1
		- **bio12**	Annual precipitation	0	11401	mm	0
		- **bio13**	Precipitation of wettest month	0	2949	mm	0
		- **bio14**	Precipitation of driest month	0	752	mm	0
		- **bio15**	Precipitation seasonality	0	265	Coefficient of Variation	0
		- **bio16**	Precipitation of wettest quarter	0	8019	mm	0
		- **bio17**	Precipitation of driest quarter	0	2495	mm	0
		- **bio18**	Precipitation of warmest quarter	0	6090	mm	0
		- **bio19**	Precipitation of coldest quarter	0	5162	mm	0

```{r cars}
#  download.file("")
```

## The code in Google Earth Engine

### Masking non-burnable areas

First we mask the LULC map removing non-burnable areas as per Scott and Burgan NB(1,2,3) etc...

```{javascript}

var lulc_mask = LULC.neq(0)
                    .multiply(LULC.neq(40)) // agriculture (NB3)
                    .multiply(LULC.neq(50))  // urban (NB1)
                    .multiply(LULC.neq(70))  // snow and ice (NB2)
                    .multiply(LULC.neq(80))  // Permanent water lakes (NB8)
                    .multiply(LULC.neq(100)) // MOSS AND LICKEN (NB9??)
                    .multiply(LULC.neq(200)) // OCEAN  (NB8)
``` 
 

### Collecting training/testing/validation data

Biomass ground truth data are not easily reached due to the very different 
sampling protocols and to many countries not providing the information 
as open data. 
Some dedicated EU projects, such as GLOBIOMASS, were funded for the main
goal to create biomass maps. We therefore decided to use these maps
for training an ensemble of machine learning methods using as descriptors the 
data-rich environment created by collecting the above-listed data.

The rationale behind this decision is that the dependent variable, i.e. the
AGB, is mapped with a well-documented uncertainty via the 2018 CEDA biomass map. 
By collecting a large number of samples from the area with stratified sampling
that accounts for land-cover type and biomass class, we will augment available
training data from ground samples. 

The continous biomass information was grouped in 10 categories:

??2000 samples per land cover category and biomass  for a total of 26000 samples 

This allow to have a balanced representation of the different cover classes 
and of different biomass scenarios.

The results from final testing on independent data shows ....

