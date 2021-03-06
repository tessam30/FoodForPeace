/*-------------------------------------------------------------------------------
# Name:		01_ETL
# Purpose:	Create series of folders Food for Peace Time Animated Maps & Analysis
# Author:	Tim Essam, Ph.D.
# Created:	07/23/2014
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	strReplace
#-------------------------------------------------------------------------------
*/
clear
capture log close

*import delimited "$pathin/FFP 60th Country Data 8.11.csv"
import delimited "$pathin/FFP 60th Country Data Pull_9.5.14.csv"

***NOTES***
* France 1955 is missing information
* DRC 1964 beht_fswr is beht_fswr $3,363,000,000
* Tanzania 1964 beht_fswr $1,268,000,000
* Togo 1964 beht_fswr 573000000
************

* Fix Kosovo and Serbia country codes
replace iso_num = 381 if countrycode == "KV"
replace iso_num = 688 if countrycode == "RB"

* Fix three countries w/out codes
replace iso_num = 344 if country == "Hong Kong"
replace iso_alpha3 = "HKG" if country== "Hong Kong"
replace iso_alpha2 = "HK" if country == "Hong Kong"

replace iso_num = 446 if country == "Macau"
replace iso_alpha3 = "MAC" if country== "Macau"
replace iso_alpha2 = "MO" if country == "Macau"

replace iso_num = 654 if country == "St. Helena"
replace iso_alpha3 = "SHN" if country== "St. Helena"
replace iso_alpha2 = "SH" if country == "St. Helena"

replace region="ASIA" if regexm(country, "(Macau|Hong Kong)")==1
replace region="AFRICA" if regexm(country, "(St. Helena)")==1

* Fix Ireland
replace objectid = 9902 if country == "Ireland"
replace countrycode = "IE" if country == "Ireland"
replace iso_alpha2 = "IE" if country == "Ireland"
replace iso_alpha3 = "IRL" if country == "Ireland"
replace iso_num = 372 if country == "Ireland"
replace regioncap ="Europe-Sw Asia" if country == "Ireland"
replace country = "IRELAND" if country == "Ireland"
replace countryname = "Ireland" if country=="IRELAND"

* Call strReplace_FFP
strReplace_FFP title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole beht_fswr ffp total
format title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole beht_fswr ffp total %16.0gc

* Fix Costa Rica and Greece
*replace title_i = 15000000 if countryname=="Costa Rica" & year == 1992
*replace title_iii = 3562000 if countryname=="Greece" & year ==1964

* Create a Yugoslavia entry for pre-1992 data; Post 1992 this will be null
expand 2 if country=="SERBIA", generate(Yugoslavia)

* Replace object id, country, countrycode, iso_alpha2, iso_alph3, iso_num
* Guidance from: http://en.wikipedia.org/wiki/ISO_3166-3

replace iso_num = 890 if Yugoslavia == 1 
replace objectid = 9901 if Yugoslavia == 1
replace country="YUGOSLAVIA" if Yugoslavia == 1
replace countryname = "Yugoslavia" if Yugoslavia == 1
replace iso_alpha2 = "YU"  if Yugoslavia == 1
replace iso_alpha3 = "YUG" if Yugoslavia == 1
replace countrycode = "YU" if Yugoslavia == 1 

g byte notYugo = regexm(country, "(SERBIA|CROATIA|MACEDONIA|BOSNIA AND HERZEGOVINA|KOSOVO|MONTENEGRO|SLOVENIA)") ==1 & year<1992
g byte oldYugo = regexm(country, "(SERBIA|CROATIA|MACEDONIA|BOSNIA AND HERZEGOVINA|KOSOVO|MONTENEGRO|SLOVENIA)") == 1 

* Reset all values to zero if 
local reset	title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole beht_fswr ffp total
foreach x of local reset {
	replace `x' = 0 if notYugo==1 
	replace `x' = 0 if country=="YUGOSLAVIA" & year>1991
	}
* //  

* Fix values for Brazil 1980, Saint Lucia 1985, Republic of the Congo 1983, Uganda 1965 and Comoros 1983
replace title_ii_em = 876000 if countryname == "Brazil" & year == 1980
replace title_ii_em = 927000 if countryname == "Saint Lucia" & year == 1985
replace title_ii_em = 155000 if countryname == "Uganda" & year == 1965
replace title_ii_em = 394000 if countryname == "Comoros" & year == 1983
replace title_ii_em = 25000 if countryname == "Republic Of The Congo" & year == 1983

* Create variable labels for different titles
la var title_i "Title I: Economic Assistance and Food Security"
la var title_ii_em "Title II: Emergency Assistance Programs"
la var title_ii_dev "Title II: Delopment Food Assistance Programs"
la var title_iii "Title III: Food for Development"
la var title_iv "Title IV: General Authorities and Requirements"
la var b "Section 416(b)"
la var lrp_pilot "Local and Regional Procurement Pilot"
la var mcg_dole "McGovern-Dole International Food for Education and Child Nutrition Program" 
la var beht_fswr "Title V: John Ogonowski and Doug Bereuter Farmer-to-Farmer (F2F) Program"
la var ffp "Food for Peace"

* Check for duplicates in ISO & year variable
bysort iso_num year: gen id = _n

/* Countries with duplicate information
Iraq 368
Seychelles  690
TRINIDAD AND TOBAGO 780
*/

* Write a loop to fix three problem countries. Check totals before/after to ensure valid removal filters
foreach x of numlist 368 690 780 {
	*tab year if iso_num==`x'
	egen tmpCheck`x' = total(total) if iso_num==`x'
	drop if iso_num == `x' & total==0 & id == 2
	egen tmp = total(id) if iso_num == `x', by(year) 
	drop if iso_num == `x' & total ==0 & tmp ==3
	drop tmp
	
	*tab year if iso_num==`x'
	egen tmpCheck2`x' = total(total) if iso_num==`x'
	g byte check`x' = (tmpCheck2`x' == tmpCheck`x')
	drop tmpCheck2`x' tmpCheck`x'
	tab check`x' 
	}
*end

isid iso_num year

* fill in missing country names
replace countryname = country if countryname==""

* Merge in additional data collected by BFS.
merge 1:1 iso_num year using "$pathin\extra_assistance.dta"
drop _merge

* Get rid of all the zeros to ensure that any averages are not distored by 0
foreach x of varlist title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole beht_fswr ffp total CDF EFSP OCO {
	replace `x' = . if `x' == 0
}
*end

* Verfiy that all other figures match total
egen double total_check = rowtotal(title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole beht_fswr ffp CDF EFSP OCO), mi
format total_check %16.0gc
labgen2 byte total_valid "Total aggregates that match" = (total == total_check) if total!=. & total_check!=.
replace total = total_check if total_valid==0


* Tabulate data and check mis-validated totals
tab total_valid, mi
drop if total_valid==.


* Create ranks for all spending
local titles title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole ffp total total_check CDF EFSP OCO
foreach x of local titles  {
	egen rank_`x' = rank(`x'), by(year) f
}
*end
set more on
local titles title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole ffp total total_check CDF EFSP OCO
qui foreach x of local titles {
	*scatter `x' year if rank_`x' == 1, mlabel(countryname) title("`x'") scheme(burd9)
	*more
	}
*
merge m:m year using "$pathin\cpi.dta"
drop if _merge==2
	
*Create adjusted figures to 2013 Dollars
local titles title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole ffp total total_check 
foreach x of local titles {
		g double `x'_2013 = round((`x'/cpi_2013)*100, 100)
		g double ln`x'_2013 = round((`x'/cpi_2013)*100, 100)
		replace ln`x'_2013 = ln(ln`x'_2013)
		copydesc `x' `x'_2013 
		copydesc `x' ln`x'_2013
		*format `x'_2013 %16.0gc
	}
*end
sort iso_num year

* Generate running sum for adjusted and unadjusted totals
bysort iso_num (year): gen double culmTotal = sum(total)
bysort iso_num (year): gen double culmTotal2013 = sum(total_2013)

*Generate natural logarithm for culmulative totals
g lnculmTotal2013 = ln(culmTotal2013)

egen double grandTot = total(total_2013), by(iso_num)
egen double annualTot = total(total_2013), by(year)

g double lngrantTot = ln(grandTot)
g double lnannualTot = ln(annualTot)

* Create country's share for the year
g double countryShare = culmTotal2013/grandTot
la var countryShare "Country's annual share of its total value (within)"

* Create country's share for it's own history
g yearlyShare = total_2013/annualTot
la var yearlyShare "Country's annual share of overall value for year (between)"

* Create country's overall share of total pot
egen double overallTotal = total(total_2013)
g overallShare = grandTot/overallTotal

* Create decadal aggregates for each country and region
*1954-1959
* 1960 - 1969
* 1970 - 1979
* 1980 - 1989
* 1990 - 1999
* 2000 - 2009
* 2010 - 2013

egen double decTotal = total(total_2013) if year>1953 & year<1960, by(iso_num)
egen double decTotalReg = total(total_2013) if year>1953 & year<1960, by(region)
la var decTotal "Decadal totals by country"
la var decTotal "Decadal totals by region"

forvalues i = 1960(10)2010 {
	egen double tmp`i'T = total(total_2013) if year>=`i' & year<`i'+10, by(iso_num)
	egen double tmp`i'R = total(total_2013) if year>=`i' & year<`i'+10, by(region)
	replace decTotal = tmp`i'T if year>=`i' & year<`i'+10
	replace decTotalReg = tmp`i'R if year>=`i' & year<`i'+10
	drop tmp`i'T  tmp`i'R
	}
*end
	
* Check to make sure everything is summing to 1
assert countryShare==1 | countryShare==. if year==2013
order ln*, last

* Create ranks for all logged spending
local titles title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole ffp total total_check 
qui foreach x of local titles  {
	egen rank_ln`x' = rank(ln`x'_2013), by(year) f
}
*end

local titles title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole ffp total total_check
qui foreach x of local titles {
	scatter ln`x'_2013 year if rank_ln`x' <= 1, mlabel(countryname) mlabs(tiny) title("`x'") scheme(burd9) by(region)
	more
	}
*

*Create exploratory data analysis graphs
encode country, gen(countryid)
xtset countryid year

set more on
levelsof region, local(levels)
qui foreach x of local levels {
	xtline countryShare if region=="`x'", tlabel(#3) i(countryname) t(year)
	*more
	*xtline lnculmTotal if region=="`x'", tlabel(#3) i(countryname) t(year)
	more
	}
*end

* Collapse a cut of the data for decadal graphs in R
preserve
keep if inlist(year, 1959, 1969, 1979, 1988, 1999, 2009, 2013)==1
keep year country region countryname decTotal decTotalReg
export delimited using "$pathRin\FFPdata1003.csv", replace
collapse (mean) decTotalReg , by(year region)
drop if region==""
export delimited using "$pathRin\FFPdata1003_Regional.csv", replace
restore

/* Generate a report of the top 3 values for each year
preserve
	keep if rank_total_check <=5
	keep countryname year rank_total_check title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole beht_fswr ffp total_check total
	*keep countryname year rank_total lntitle_i_2013 lntitle_ii_em_2013 lntitle_ii_dev_2013 lntitle_iii_2013 lntitle_iv_2013 lnb_2013 lnlrp_pilot_2013 lnmcg_dole_2013 lnffp_2013 lntotal_2013
	order countryname year rank_total_check total_check total
	export excel using "$pathxls\FFP_outliers.xls", sheet("Outliers") firstrow(variables) replace
restore

* Export all observations for which reported total does not match calculated total
preserve
	keep if total_valid==0
	keep countryname year title_i title_ii_em title_ii_dev title_iii title_iv b lrp_pilot mcg_dole beht_fswr ffp total_check total
	order countryname year total_check total
	export excel using "$pathxls/FFP_outliers.xls", sheet("Totals_Different") firstrow(variables) sheetmodify
restore
*/
compress

* Export a copy of the data to R for playing in GGplot and GGVIS
save "$pathout/FTF_processed.dta", replace 
saveold "$pathout/FTF_processed.dta", replace

export delimited using "$pathout\FFPdata1003.csv", delimiter(tab) replace
export delimited using "$pathPdrive\Dataout\FFPdata1003.csv", delimiter(tab) replace

collapse (sum) decTotal if inlist(year, 1959, 1969, 1979, 1988, 1999, 2009, 2013), by(region year)
export delimited using "$pathRin\FFPdata1003_RegionTotals.csv", replace

