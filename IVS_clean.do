clear all


* Define your working directory, where you saved the  dataset "Integrated_values_surveys_1981-2021.dta"


cd "/Users/silviapianta/Dropbox (CMCC)/politics_and_governance/IVS_1981-2020"

use "Integrated_values_surveys_1981-2021.dta"



rename S001 whichvs
* 1 evs 2 wvs

rename S002 wvswave
*1 1981-1984 
*2 1989-1993 
*3 1994-1998 
*4 1999-2004 
*5 2005-2009 
*6 2010-2014 
*7 2017-2019 

rename S002EVS evswave
*1 1981-1984 = 1 in wvs 
*2 1990-1993 = 2 in wvs (+-)
*3 1999-2001 = *4* in wvs (+-)
*4 2008-2010 = *5* in wvs (+-)
*5 2017-2020 = *7* in wvs (+-)


* gen a single wave varibale unique for both EVS & WVS

gen wave = S002VS

* copy S002VS, which is present only in wvs data
* fill wave with EVS waves

replace wave = 1 if whichvs==1 & evswave ==1
replace wave = 2 if whichvs==1 & evswave ==2
replace wave = 4 if whichvs==1 & evswave ==3
replace wave = 5 if whichvs==1 & evswave ==4
replace wave = 7 if whichvs==1 & evswave ==5



*gen infowave string var with specific year interval to keep info on in which years the WVS /EVS data was collected
* e.g. EVS 1981-1984 or WVS 1981-1984

decode wvswave, gen(infowave)
decode evswave, gen(evswave_chr)

replace infowave = evswave_chr if whichvs ==1

replace infowave = "EVS " + infowave if whichvs ==1
replace infowave = "WVS " + infowave if whichvs ==2

*drop wvswave evswave evswave_chr

decode S003, gen(country)

gen countrywave = country + " " + infowave

label variable infowave "Wave Info"


rename S020 year


* KEEP ONLY LAST WAVE

keep if evswave == 5 | wvswave == 7


* ENV VARIABLES



* 1

* A071
* Member: Belong to conservation, the environment, ecology, animal rights

replace A071 =. if A071 ==.a
replace A071 =. if A071 ==.b
replace A071 =. if A071 ==.e

rename A071 engo



label define engol 0 "No"  1 "Yes"
label values engo engol

* CODING: 0 no 1 yes




* 2

* B008
* Protecting environment vs.  Economic growth

replace B008 =. if B008 ==.a
replace B008 =. if B008 ==.b
replace B008 =. if B008 ==.e
replace B008 = . if B008 == 3 
* 3 was other answer

replace B008 = 0 if B008 == 2

rename B008 envgrowth


label define envgrowthl 0 "Growth"  1 "Env"
label values envgrowth envgrowthl


* CODING: 0 economy 1 environment




/*
1 Protecting the environment should be given priority, even if it causes slower economic growth and some loss of jobs.
2 Economic growth and creating jobs should be the top priority, even if the environment suffers to some extent.
*/



* 3

* Confidence in  The Environmental Protection Movement

*E069_14

replace E069_14 =. if E069_14 ==.a
replace E069_14 =. if E069_14 ==.b
replace E069_14 =. if E069_14 ==.e


*reverse so that higher values higher trust 
revrs E069_14
drop E069_14
rename revE069_14 trustengo




* CODING: 1 not at all to 4 a great deal




* LRSCALE

rename E033 lrscale

replace lrscale =. if lrscale ==.a
replace lrscale =. if lrscale ==.b
replace lrscale =. if lrscale ==.d
replace lrscale =. if lrscale ==.e





* CODING : 1 left to 10 right




* POSTMATERIALIST INDEX

rename Y002 postmat

replace postmat =. if postmat ==.e

* CODING: 1 Materialist 2 Mixed 3 Post-materialist



rename COW_ALPHA country_cow
rename S009 iso2c



rename S017 weight


keep whichvs wvswave evswave year wave infowave evswave_chr country countrywave country_cow iso2c ///
engo trustengo envgrowth lrscale postmat ///
weight



*Create dataset containing the mean and median of engo trustengo envgrowth  for each 
*country and wave, and store median of engo trustengo envgrowth in engomedian trustengomedian envgrowthmedian,



			
collapse (mean) engo trustengo envgrowth lrscale postmat (median) lrscalemedian=lrscale [pw=weight], by(country  iso2c wave)

*[pw=weight] to get weighted values		
* add before coma  engomedian=engo trustengomedian=trustengo envgrowthmedian = envgrowth postmatmedian=postmat if you also want median for those vars (not much sense as very small scale for many vars)

drop wave

label variable country "Country"
label variable iso2c "ISO2"

label variable engo "ENGO membership"
label variable envgrowth "Env or growth"
label variable trustengo "Trust in ENGO"
label variable lrscale "Left-right scale"
label variable lrscalemedian "Left-right scale median"
label variable postmat "Postmaterialism"



save "IVS7_env.dta", replace



 