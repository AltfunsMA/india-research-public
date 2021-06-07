# India public goods research

This repository currently contains materials that enable replication of the paper:

> Martínez Arranz, A., R. Thomson et al., 2021, "The Uneven Expansion of Electricity Supply in India", _Energy Research and Social Science_, Volume 78, August 2021, 102126. DOI: https://doi.org/10.1016/j.erss.2021.102126

You may use any code in this repository subject to the MIT license. Please acknowledge it in any materials with a citation to the above article.


## Obtaining relevant datasets

### Household data

The [Consumer Pyramids](https://consumerpyramidsdx.cmie.com/) survey provides all information related to households, which we then aggregate to the district-region level (as available during 2020). These data are available for purchase or after subscription. For our "Uneven expansion of electricity supply" paper, we used Waves 1 and 18, corresponding to early 2014 and late 2019.

For the replication materials to work, you must store the relevant sections as follows under the Input_data folder.

`Input_data/DS_CP_full/` <br/>
`├── Household Amenities, Assets _ Liabilities` <br/>
`│   ├── Wave 2014.zip` <br/>
`│   └── Wave 2019.zip` <br/>
`├── Household Expense - Details` <br/>
`│   ├── Wave 2014.zip` <br/>
`│   └── Wave 2019.zip` <br/>
`├── Income` <br/>
`│   ├── Household - Income` <br/>
`│   │   ├── Wave 2014.zip ` <br/>
`│   │   └── Wave 2019.zip` <br/>
`│   └── Member - Income` <br/>
`│       ├── Wave 2014.zip` <br/>
`│       └── Wave 2019.zip` <br/>
`└── People of India` <br/>
`    ├── Wave 2014.zip` <br/>
`    └── Wave 2019.zip` <br/>

![image](https://user-images.githubusercontent.com/26325512/120978904-580ba480-c7b8-11eb-80a4-f93b6118d332.png)


### Election data
 
We take the state election data from [Bhavnani RR 2014](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/26526) and combine it with data from www.indiavotes.com. 

To facilitate replication, we provide a zipped version of the cleaned up file for data post 1990 that is used in the relevant script.

In order to combine this information with district-based consumer pyramids data, We use assembly constituency maps from ["Community Created Maps of India"](http://projects.datameet.org/maps/assembly-constituencies/), which is based on data from the Electoral Commission.

Note that the "Third Front" does not really exist as a formal alliance in Indian politics. We use that label to capture many 'left-of-INC' and regional parties, many of which have traditionally been part of the "Left Front".

![election](https://user-images.githubusercontent.com/26325512/120981115-ac178880-c7ba-11eb-94f7-4a3daf88d849.png)

### Urban - rural distinction

To allocate assembly constituencies, we include datasets from NASA's socioeconomic data and applications center (requires free registration):


1. Gridded Population of the World (GPW), specifically the [population density estimates for 2010 with 2.5 minute resolution](https://beta.sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download) 
1. [Urban extent polygons](https://beta.sedac.ciesin.columbia.edu/data/set/grump-v1-urban-ext-polygons-rev02/data-download) 

We also contrast this data with open source information available from [GeoNames](https://www.geonames.org/)

The [2011 Indian census](https://censusindia.gov.in/2011-common/censusdata2011.html) also contains urban / rural population information that we use to verify and adjust our distinction.

The result of these combinations is something like this:

![image](https://user-images.githubusercontent.com/26325512/120928012-54c7d880-c726-11eb-87d4-c06364c8570e.png)




## Installing relevant software

This research was mostly carried out in R > 4.0.0 with some of the final operations carried out in Stata > 14. I provide an approximation using R functions but the weighted linear regression with clustered robust standard errors are best done in Stata.

### R packages

My package `cptools` provides some convenience functions using the tidyverse. Install it with `devtools::install_github("https://github.com/AltfunsMA/cptools")`.

Apart from this, you should install the `sf` and `raster` packages for map manipulation, and the `standardize` and `laeken` for some convenience functions (gini and normalisation). The `foreign` package will also allow you to export the data to Stata. 

I would also recommend using the RStudio IDE unless you really love the command line.

## Running the scripts

The scripts are a snapshot of a point in our research process. Some of the variables that are calculated are not used in this first paper, but removing those calculations is more trouble than it's worth. The easiest way after you have obtained the CP data is to run 01_master.R under `Scripts`.

`Scripts` contains the full numbered pipeline that eventually generates the files we did our calculations. The subfolder `create_inputs` contains the scripts that modify some raw inputs cited above but not provided to generate the obejcts in `Input_data`. 


## Erratum

We have noticed that there is a data entry error in the `Input_data/st_gov_by_year file.csv`: in Bihar in 2010, the RJD has been miscoded as JD(U). This makes our finding regarding the "clientelist logic" lose statistical significance; but we already flagged this finding as particularly weak and unreliable.

We are also aware of some minor discrepancies in the "expected hours" values (stemming from calculations on older maps) but the findings regarding the other logics remain robust.




