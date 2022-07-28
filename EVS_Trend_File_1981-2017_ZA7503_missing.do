

************************** MISSING DEFINITION **********************************

****  Data set: "ZA7503_v2-0-0.dta" - EVS_Trend_File_1981-2017.
****   Version: 2.0.0 (2021-07-07) doi:10.4232/1.13736.
****   Sources: EVS data can be downloaded from:
*****  GESIS: https://search.gesis.org/research_data/ZA7503?lang=en



/* The current syntax converts missing values from negative numbers to 
extended missing values in STATA format and saves a new dataset with 
the suffix _miss  */

* Fill in your path below
cd "/Users/silviapianta/Dropbox (CMCC)/politics_and_governance/IVS_1981-2020"

* Load EVS Trend file
use "EVS_Trend_File_1981-2017_ZA7503_v2-0-0" , clear


* Define missing values for numeric variables
qui ds , has(type numeric)
foreach var in `r(varlist)' {
recode `var' (-1 = .a ) ///
(-2 = .b ) ///
(-3 = .c ) ///
(-4 = .d ) ///
(-5 = .e )
}

*  Define missing values for string variables
ds , has(type string)
foreach var in `r(varlist)' {
replace `var' = ".a" if `var' == "-1"
replace `var' = ".b" if `var' == "-2"
replace `var' = ".c" if `var' == "-3" 
replace `var' = ".d" if `var' == "-4" 
replace `var' = ".e" if `var' == "-5" 
}


* Attach missing value labels
qui ds , has(type numeric)
foreach var in `r(varlist)'  {
lab def `var' .a "Don't know" , modify
lab def `var' .b "No answer" , modify
lab def `var' .c "Not applicable" , modify
lab def `var' .d "Not asked in survey" , modify
lab def `var' .e "Missing: other" , modify
}


save "EVS_Trend_File_1981-2017_ZA7503_v2-0-0_miss.dta" , replace
