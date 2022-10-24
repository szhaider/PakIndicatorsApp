
# PakIndicatorsApp

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

The goal of PakIndicatorsApp is to provide a scaffolding for the
schematic and efficient management and visualization of the district
level indicators of Pakistan, for multiple rounds of the household
surveys. It covers a variety of socio-economic (SDG related) indicators
ranging from poverty incidence, labor market outcomes, health and
nutrition, stunting and wasting, food security and so on. We will be
updating the app based on the data availability and revisions/feedback.

##Data Coverage

Currently, the portal covers HIES, PLSM, MICS and NNS surveys. (various
rounds from 2004 to 2020)

##Usage

The app can specifically be used to explore spatial disparities in
development outcomes at district (adm2) level in the country.

## Installation

You can install the development version of PakIndicatorsApp like so:

``` r
# To install the application 
remotes::install_github("szhaider/PakIndicatorsApp")
```

## Example

This is a basic details about the data used in the application:

    #>  [1] "Household Welfare"           "Water and Sanitation"       
    #>  [3] "Affordable and Clean Energy" "Asset Ownership"            
    #>  [5] "Health and Wellbeing"        "Transportation"             
    #>  [7] "Household Charactersitics"   "Dwelling Charactersitics"   
    #>  [9] "Labor Market"                "Quality Education"          
    #> [11] "Demographics"                "Childhood Development"      
    #> [13] "Gender Equality"             "Food Insecurity"            
    #> [15] "Malnutrition"                "Social Protection"          
    #> [17] "Utilities"                   "Vaccination"                
    #> [19] "Maternal Newborn Care"

    #>  [1] 2004 2006 2008 2010 2012 2014 2016 2017 2018 2019 2020

    #>  [1] "2004-HIES/PSLM"                   "2004-PSLM"                       
    #>  [3] "2004-MICS"                        "2004-Census"                     
    #>  [5] "2006-HIES/PSLM"                   "2006-PSLM"                       
    #>  [7] "2006-MICS"                        "2006-Census"                     
    #>  [9] "2008-HIES/PSLM"                   "2008-PSLM"                       
    #> [11] "2008-MICS"                        "2008-Census"                     
    #> [13] "2010-HIES/PSLM"                   "2010-PSLM"                       
    #> [15] "2010-MICS : Punjab & Balochistan" "2010-Census"                     
    #> [17] "2012-HIES/PSLM"                   "2012-PSLM"                       
    #> [19] "2012-MICS"                        "2012-Census"                     
    #> [21] "2014-HIES/PSLM"                   "2014-PSLM"                       
    #> [23] "2014-MICS : Punjab & Sindh"       "2014-Census"                     
    #> [25] "2016-HIES/PSLM"                   "2016-PSLM"                       
    #> [27] "2016-MICS : KP"                   "2016-Census"                     
    #> [29] "2017-HIES/PSLM"                   "2017-PSLM"                       
    #> [31] "2017-MICS"                        "2018-Census"                     
    #> [33] "2018-PSLM"                        "2018-MICS : Punjab"              
    #> [35] "2017-Census"                      "2018-HIES/PSLM"                  
    #> [37] "2018-NNS"                         "2019-Census"                     
    #> [39] "2019-PSLM"                        "2019-MICS : KP & Sindh"          
    #> [41] "2020-MICS : Balochistan"
