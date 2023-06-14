************************************************************
*** STATA Replication Code for Koopman and Wacker (2023) ***
***         and to estimate growth accelerations         ***
***** *https://doi.org/10.1016/j.worlddev.2023.106297 ******
************************************************************
******* BEFORE STARTING: *************
* 1. Set a WORKING DIRECTORY (folder) in line 16.
*    This folder should already exist on your computer.
*    The global defined in line 16 will ensure the code can access this folder on various code lines.
*    Type "help global" for details.
* 2. ssc install rangestat
* 3. ssc install asrol
**************************************

clear
global path "WORKING DIRECTORY"
cd "$path"

*Getting PWT10.0 data
use "https://www.rug.nl/ggdc/docs/pwt100.dta"
drop currency_unit rgdpe rgdpo ccon cda cgdpe cgdpo cn ck ctfp cwtfp rconna rdana irr delta xr pl_con pl_da pl_gdpo i_cig i_xm i_xr i_outlier i_irr cor_exp statcap csh_c csh_i csh_g csh_x csh_m csh_r pl_c pl_i pl_g pl_x pl_m pl_n pl_k

*Dropping countries with population smaller than 1 million in 2019
reshape wide country pop emp avh hc rgdpna rnna rkna rtfpna rwtfpna labsh, i(countrycode) j(year)
drop if pop2019<1
reshape long

*Obtaining the least squares growth rate from t to t+7
gen rgdpcap=rgdpna/pop
label var rgdpcap "real GDP per capita (rgdpna/pop)"
gen lnrgdpcap=ln(rgdpna/pop)
label var lnrgdpcap "natural logarithm of real GDP per capita (rgdpna/pop)"
sort countrycode year
rangestat(reg) lnrgdpcap year, by(countrycode) interval(year 0 7)
rename b_year gr0_7
label var gr0_7 "least squares growth rate of real GDP per capita from t to t+7"
replace gr0_7=. if reg_nobs<8
drop reg_nobs reg_r2 reg_adj_r2 b_cons se_year se_cons

*Obtaining the least squares growth rate from t-7 to t
by countrycode: gen gr_lag7= gr0_7[_n-7]
label var gr_lag7 "least squares growth rate of real GDP per capita from t-7 to t"

*Obtaining the change in the growth rate at time t
gen change_growth= gr0_7-gr_lag7
label var change_growth "change in the growth rate at time t over horizon 7"

*Obtaining the output level at the end of the acceleration 
by countrycode: gen rgdpcap_Lead7= rgdpcap[_n+7]
label var rgdpcap_Lead7 "level of real GDP per capita in year t+7"

*Obtaining the peak output level 
by countrycode: generate record = rgdpcap[1] if _n==1
by countrycode: replace record = max(rgdpcap, record[_n-1]) if missing(record)
label var record "peak level of real GDP per capita up to and including the current year (year t)"

*Obtaining the least squares growth rate from t+7 to t+17 (for sustained episodes)
rangestat(reg) lnrgdpcap year, by(countrycode) interval(year 7 17)
rename b_year gr7_17
label var gr7_17 "least squares growth rate of real GDP per capita from t+7 to t+17"
replace gr7_17=. if reg_nobs<11
drop reg_nobs reg_r2 reg_adj_r2 b_cons se_year se_cons

*Identifying years for which conditions are met
generate pot_episode=0
replace pot_episode=1 if gr0_7>=0.035 & gr0_7!=. & change_growth>=0.02 & change_growth!=. & rgdpcap_Lead7>=record & rgdpcap_Lead7!=.
label var pot_episode "dummy variable which is 1 when the 3 HPR conditions are met and 0 otherwise"

*Splines
gen key=countrycode+string(year,"%02.0f")
encode key, gen(key_id)
drop key
rename key_id key
label var key "key for every country+year"

encode country, gen(country_id)

gen fstat=.
label var fstat "fstatistic of a test for equality of two trend slopes of spline regression"

levelsof key if pot_episode==1, local(epi)
foreach i of local epi{
summ year if key == `i', meanonly
local date = r(min)
summ country_id if key == `i', meanonly
local cnt = r(min)
local start = `date' - 7
local end = `date' + 7
mkspline year1 `date' year2 = year
quietly regress lnrgdpcap year1-year2 if inrange(year,`start',`end') & country_id == `cnt'
test year1=year2
gen temp3=r(F) 
replace fstat=temp3 if key==`i'
drop temp3 
drop year1 year2
}

*Identifying the year for which the f-statistic is maximized
bysort countrycode (year) : gen pot_spell = cond(pot_episode == 0,  0, sum(pot_episode == 1 & pot_episode[_n-1] == 0)) 
egen max = max(fstat) if pot_spell, by(countrycode pot_spell) 
gen acceleration=0
replace acceleration=1 if fstat==max
replace acceleration=0 if pot_episode==0
label var acceleration "dummy equal to 1 if a growth acceleration starts in this year"

*Eliminating episodes within 5 years of other episodes 
replace acceleration=0 if acceleration==1 & acceleration[_n-1]==1 | acceleration[_n-2]==1 | acceleration[_n-3]==1 | acceleration[_n-4]==1

*Recovering episodes which are more than 5 years apart from other episodes 
replace acceleration = cond(acceleration==0 & pot_episode==1 & fstat==max & acceleration[_n-1]==0 & acceleration[_n-2]==0 & acceleration[_n-3]==0 & acceleration[_n-4]==0,1,0) if acceleration!=1

*Identifying sustained growth accelerations 
generate sustained=0 if acceleration==1
replace sustained=1 if acceleration==1 & gr7_17>=0.02 & gr7_17!=.
label var sustained "dummy equal to 1 if a sustained growth acceleration starts in this year"

*Identifying sustained growth accelerations 
generate sustained=0 if acceleration==1
replace sustained=1 if acceleration==1 & gr7_17>=0.02 & gr7_17!=.
label var sustained "dummy equal to 1 if a sustained growth acceleration starts in this year"

*Add region (dummies)
cd "$path/datasets"
merge m:1 countrycode using regions_new.dta
drop _merge
encode region, gen(region_id)
drop region
rename region_id region
generate africa=0
replace africa=1 if region==1
generate americas=0
replace americas=1 if region==2
generate asia=0
replace asia=1 if region==3
replace asia=1 if region==6
generate europe=0
replace europe=1 if region==4
generate meast=0
replace meast=1 if region==5

*Add decade dummies
gen decade1950=0
replace decade1950=1 if inrange(year,1950,1959)
gen decade1960=0
replace decade1960=1 if inrange(year,1960,1969)
gen decade1970=0
replace decade1970=1 if inrange(year,1970,1979)
gen decade1980=0
replace decade1980=1 if inrange(year,1980,1989)
gen decade1990=0
replace decade1990=1 if inrange(year,1990,1999)
gen decade2000=0
replace decade2000=1 if inrange(year,2000,2009)
gen decade2010=0
replace decade2010=1 if inrange(year,2010,2019)

*Add level of development (dummies)
cd "$path/datasets"
merge m:1 year using rgdpcapUS.dta
drop _merge
gen high_income_threshold= rgdpcapUS/2
label var high_income_threshold "income threshold for classification as a high-income country (dynamic)"
gen high_income=0
replace high_income=1 if rgdpcap>=high_income_threshold & rgdpcap!=.
label var high_income "dummy equal to 1 if classified as a high-income country at year t, 0 otherwise"
sort year
by year: egen max_nonhighincome=max(rgdpcap) if high_income==0
label var max_nonhighincome "highest level of real GDP per capita of the non-high income countries (dynamic)"
gen middle_income_threshold= max_nonhighincome/2
label var middle_income_threshold "income threshold for classification as middle-income country (dynamic)"
gen middle_income=0
replace middle_income=1 if rgdpcap>= middle_income_threshold & high_income==0 & rgdpcap!=.
label var middle_income "dummy equal to 1 if classified as a middle-income country at time t, 0 otherwise"
gen low_income=0
replace low_income=1 if high_income==0 & middle_income==0 & rgdpcap!=.
label var low_income "dummy equal to 1 if classified as a low-income country at time t, 0 otherwise"

*Creating a dummy for years during which an acceleration is going on
sort countrycode year
gen year_in_acceleration=0
replace year_in_acceleration=1 if acceleration==1 | acceleration[_n-1]==1 | acceleration[_n-2]==1 | acceleration[_n-3]==1 | acceleration[_n-4]==1 | acceleration[_n-5]==1 | acceleration[_n-6]==1 | acceleration[_n-7]==1
label var year_in_acceleration "dummy equal to 1 when a year is within an ongoing acceleration, 0 otherwise"

*Growth accounting (section 4)
*Growth accounting variables
gen rgdpna_worker= rgdpna/emp
label var rgdpna_worker "real GDP per worker (rgdpna/emp)"
gen lnrgdpna_worker = ln(rgdpna/emp)
label var lnrgdpna_worker "natural logarithm of real GDP per worker (rgdpna/emp)"
gen cap_worker= rnna/emp
label var cap_worker "physical capital stock per worker (rnna/emp)"
gen lncap_worker=ln(rnna/emp)
label var lncap_worker "natural logarithm of physical capital stock per worker (rnna/emp)"
gen lnhc=ln(hc)
label var lnhc "natural logarithm of human capital"
gen alpha=(1/3)
label var alpha "elasticity of output with respect to capital"
gen lnTFP= (lnrgdpna_worker - (alpha*lncap_worker) - ((1-alpha)*lnhc))
label var lnTFP "natural logarithm of total factor productivity"

*Growth rate using log-differences
sort countrycode year
by countrycode: gen growth_during=((lnrgdpna_worker[_n+7]-lnrgdpna_worker)/7)*100 if acceleration==1
label var growth_during "Average annual growth rate of real GDP per worker during an acceleration (in %)"
by countrycode: gen contribution_cap_during=((lncap_worker[_n+7]-lncap_worker)/7)*alpha*100 if acceleration==1
label var contribution_cap_during "Growth contribution of capital during an acceleration (in ppt)"
by countrycode: gen contribution_hc_during=((lnhc[_n+7]-lnhc)/7)*(1-alpha)*100 if acceleration==1
label var contribution_hc_during "Growth contribution of human capital during an acceleration (in ppt)"
by countrycode:  gen contribution_tfp_during=((lnTFP[_n+7]-lnTFP)/7)*100 if acceleration==1
label var contribution_tfp_during "Growth contribution of TFP during an acceleration (in ppt)"

by countrycode: gen growth_before=((lnrgdpna_worker-lnrgdpna_worker[_n-7])/7)*100 if acceleration==1
label var growth_before "Average annual growth rate of real GDP per worker before an acceleration (in %)"
by countrycode: gen contribution_cap_before=((lncap_worker-lncap_worker[_n-7])/7)*alpha*100 if acceleration==1
label var contribution_cap_before "Growth contribution of capital before an acceleration (in ppt)"
by countrycode: gen contribution_hc_before=((lnhc-lnhc[_n-7])/7)*(1-alpha)*100 if acceleration==1
label var contribution_hc_before "Growth contribution of human capital before an acceleration (in ppt)"
by countrycode: gen contribution_tfp_before=((lnTFP-lnTFP[_n-7])/7)*100 if acceleration==1
label var contribution_tfp_before "Growth contribution of TFP before an acceleration (in ppt)"

by countrycode: gen growth_after=((lnrgdpna_worker[_n+17]-lnrgdpna_worker[_n+7])/10)*100 if acceleration==1
label var growth_after "Average annual growth rate of real GDP per worker after an acceleration (in %)"
by countrycode: gen contribution_cap_after=((lncap_worker[_n+17]-lncap_worker[_n+7])/10)*alpha*100 if acceleration==1
label var contribution_cap_after "Growth contribution of capital after an acceleration (in ppt)"
by countrycode: gen contribution_hc_after=((lnhc[_n+17]-lnhc[_n+7])/10)*(1-alpha)*100 if acceleration==1
label var contribution_hc_after "Growth contribution of human capital after an acceleration (in ppt)"
by countrycode: gen contribution_tfp_after=((lnTFP[_n+17]-lnTFP[_n+7])/10)*100 if acceleration==1
label var contribution_tfp_after "Growth contribution of TFP after an acceleration (in ppt)"

*Difference between growth rates & contributions
gen difference_growth=growth_during-growth_before
label var difference_growth "difference between the growth rate during and before an acceleration"
gen difference_cap=contribution_cap_during-contribution_cap_before
gen difference_hc=contribution_hc_during-contribution_hc_before
gen difference_tfp=contribution_tfp_during-contribution_tfp_before

gen growth_change_cap=(difference_cap/difference_growth)*100
label var growth_change_cap "% of the growth rate change that can be explained by capital accumulation"
gen growth_change_hc=(difference_hc/difference_growth)*100
label var growth_change_hc "% of the growth rate change that can be explained by human capital accumulation"
gen growth_change_tfp=(difference_tfp/difference_growth)*100
label var growth_change_tfp "% of the growth rate change that can be explained by developments in TFP"

*Capital-poor dummy 
gen capoutput=rnna/rgdpna
label var capoutput "capital-output ratio (rnna/rgdpna)"
bysort year: egen capoutputmean=mean(capoutput)
gen capitalpoor=0
replace capitalpoor=1 if capoutput<capoutputmean
replace capitalpoor=. if capoutput==.
label var capitalpoor "dummy equal to 1 for capital-poor countries and 0 for capital-rich countries"

*Descriptives on K/Y ratio and income level
bys countrycode: egen mn_capoutput = mean(capoutput)
bys countrycode: egen mn_lnrgdpna_worker = mean(lnrgdpna_worker)
label var mn_capoutput "capital-output ratio (rnna/rgdpna), averaged over years"
label var mn_lnrgdpna_worker "natural logarithm of real GDP per worker (rgdpna/emp), averaged over years"
corr mn_capoutput mn_lnrgdpna_worker if year==2000

twoway (scatter  mn_capoutput mn_lnrgdpna_worker if year==2000) (scatter  mn_capoutput mn_lnrgdpna_worker if year==2000 & mn_capoutput>10, msymbol(none) mlab(countrycode) mlabcolor(dkblue) legend(off))

corr capoutput lnrgdpna_worker if year==2000

*Outliers
gen outlier=0 if acceleration==1
replace outlier=1 if key==331 | key==543 | key==1348 | key==3031 | key==3545 | key==6139 | key==8026 | key==10347

*Finding growth rates around the start of the acceleration (section 5.2)
sort countrycode year
by countrycode: gen growth=lnrgdpcap[_n+1]-lnrgdpcap
label var growth "economic growth rate from period t to period t+1"
gen growth_year_before=growth[_n-1]
label var growth_year_before "economic growth rate from period t-1 to period t"

gen negative_growth=0
replace negative_growth=1 if growth<0
label var negative_growth "dummy equal to 1 if growth from period t to period t+1 is negative"
gen late_growth1=0
replace late_growth1=1 if growth_year_before>growth
label var late_growth1 "dummy equal to 1 if growth in the year before is higher than growth in year t"
gen late_growth2=0
replace late_growth2=1 if growth_year_before>gr0_7
label var late_growth2 "dummy equal to 1 if growth in the year before is larger than gr0_7"

tabulate country if acceleration==1 & negative_growth==1
tabulate country if acceleration==1 & late_growth1==1
tabulate country if acceleration==1 & late_growth2==1
tabulate country if acceleration==1 & late_growth1==1 & late_growth2==1

*Alternative growth accounting (section 5.4)
gen lncapoutput = ln(rnna/rgdpna)
label var lncapoutput "natural logarithm of capital-output ratio (rnna/rgdpna)"

sort countrycode year
by countrycode: gen altcontribution_capoutput_d=((lncapoutput[_n+7]-lncapoutput)/7)*(alpha/(1-alpha))*100 if acceleration==1
label var altcontribution_capoutput_d "R: Growth contribution of capital-output ratio during an acceleration (in ppt)"
by countrycode: gen altcontribution_hc_d=((lnhc[_n+7]-lnhc)/7)*100 if acceleration==1
label var altcontribution_hc_d "R: Growth contribution of human capital during an acceleration (in ppt)"
by countrycode: gen altcontribution_tfp_d=((lnTFP[_n+7]-lnTFP)/7)*(1/(1-alpha))*100 if acceleration==1
label var altcontribution_tfp_d "R: Growth contribution of TFP during an acceleration (in ppt)"

by countrycode: gen altcontribution_capoutput_b=((lncapoutput-lncapoutput[_n-7])/7)*(alpha/(1-alpha))*100 if acceleration==1
label var altcontribution_capoutput_b "R: Growth contribution of capital-output ratio before an acceleration (in ppt)"
by countrycode: gen altcontribution_hc_b=((lnhc-lnhc[_n-7])/7)*100 if acceleration==1
label var altcontribution_hc_b "R: Growth contribution of human capital before an acceleration (in ppt)"
by countrycode: gen altcontribution_tfp_b=((lnTFP-lnTFP[_n-7])/7)*(1/(1-alpha))*100 if acceleration==1
label var altcontribution_tfp_b "R: Growth contribution of TFP before an acceleration (in ppt)"

by countrycode: gen altcontribution_capoutput_a=((lncapoutput[_n+17]-lncapoutput[_n+7])/10)*(alpha/(1-alpha))*100 if acceleration==1
label var altcontribution_capoutput_a "R: Growth contribution of capital-output ratio after an acceleration (in ppt)"
by countrycode: gen altcontribution_hc_a=((lnhc[_n+17]-lnhc[_n+7])/10)*100 if acceleration==1
label var altcontribution_hc_a "R: Growth contribution of human capital after an acceleration (in ppt)"
by countrycode: gen altcontribution_tfp_a=((lnTFP[_n+17]-lnTFP[_n+7])/10)*(1/(1-alpha))*100 if acceleration==1
label var altcontribution_tfp_a "R: Growth contribution of TFP after an acceleration (in ppt)"

gen altdifference_capoutput=altcontribution_capoutput_d-altcontribution_capoutput_b
gen altdifference_hc=altcontribution_hc_d-altcontribution_hc_b
gen altdifference_tfp=altcontribution_tfp_d-altcontribution_tfp_b

gen capoutput_lag7=capoutput[_n-7]
label var capoutput_lag7 "capital-output ratio at t-7"
gen capoutput_lead7=capoutput[_n+7]
label var capoutput_lead7 "capital-output ratio at t+7"
gen capoutput_lead17=capoutput[_n+17]
label var capoutput_lead17 "capital-output ratio at t+17"

sort countrycode year
by countrycode: gen growth_capoutput_during=((lncapoutput[_n+7]-lncapoutput)/7)*100 if acceleration==1
by countrycode: gen growth_capoutput_before=((lncapoutput-lncapoutput[_n-7])/7)*100 if acceleration==1
by countrycode: gen growth_capoutput_after=((lncapoutput[_n+17]-lncapoutput[_n+7])/11)*100 if acceleration==1

*****************************************************
*Growth accounting in per-capita terms (section 5.5)*
*****************************************************

gen capital_cap=rnna/pop
label var capital_cap "physical capital stock per capita (rnna/pop)"
gen lncapital_cap=ln(rnna/pop)
label var lncapital_cap "natural logarithm of physical capital stock per capita (rnna/pop)"
gen quality_workforce_cap=((hc*emp)/pop)
label var quality_workforce_cap "quality adjusted workforce per capita ((hc*emp)/pop)"
gen lnquality_workforce_cap=ln((hc*emp)/pop)
label var lnquality_workforce_cap "natural logarithm of quality adjusted workforce per capita ((hc*emp)/pop)"
gen lnTFP_cap=(lnrgdpcap - (alpha*lncapital_cap) - ((1-alpha)*lnquality_workforce_cap))

sort countrycode year
by countrycode: gen growth_during2=((lnrgdpcap[_n+7]-lnrgdpcap)/7)*100 if acceleration==1
label var growth_during2 "Average annual growth rate of real GDP per capita during an acceleration (in %)"
by countrycode: gen contribution_cap_during2=((lncapital_cap[_n+7]-lncapital_cap)/7)*alpha*100 if acceleration==1
label var contribution_cap_during2 "Contribution of physical capital per capita during an acceleration (in ppt)"
by countrycode: gen contribution_hc_during2=((lnquality_workforce_cap[_n+7]-lnquality_workforce_cap)/7)*(1-alpha)*100 if acceleration==1
label var contribution_hc_during2 "Contribution of qual. adj. workforce per capita during an acceleration (in ppt)"
by countrycode: gen contribution_tfp_during2=((lnTFP_cap[_n+7]-lnTFP_cap)/7)*100 if acceleration==1
label var contribution_tfp_during2 "Contribution of TFP during an acceleration (in ppt)"

by countrycode: gen growth_before2=((lnrgdpcap-lnrgdpcap[_n-7])/7)*100 if acceleration==1
label var growth_before2 "Average annual growth rate of real GDP per capita before an acceleration (in %)"
by countrycode: gen contribution_cap_before2=((lncapital_cap-lncapital_cap[_n-7])/7)*alpha*100 if acceleration==1
label var contribution_cap_before2 "Contribution of physical capital per capita before an acceleration (in ppt)"
by countrycode: gen contribution_hc_before2=((lnquality_workforce_cap-lnquality_workforce_cap[_n-7])/7)*(1-alpha)*100 if acceleration==1
label var contribution_hc_before2 "Contribution of qual. adj. workforce per capita before an acceleration (in ppt)"
by countrycode: gen contribution_tfp_before2=((lnTFP_cap-lnTFP_cap[_n-7])/7)*100 if acceleration==1
label var contribution_tfp_before2 "Contribution of TFP before an acceleration (in ppt)"

by countrycode: gen growth_after2=((lnrgdpcap[_n+17]-lnrgdpcap[_n+7])/10)*100 if acceleration==1
label var growth_after2 "Average annual growth rate of real GDP per capita after an acceleration (in %)"
by countrycode: gen contribution_cap_after2=((lncapital_cap[_n+17]-lncapital_cap[_n+7])/10)*alpha*100 if acceleration==1
label var contribution_cap_after2 "Contribution of physical capital per capita after an acceleration (in ppt)"
by countrycode: gen contribution_hc_after2=((lnquality_workforce_cap[_n+17]-lnquality_workforce_cap[_n+7])/10)*(1-alpha)*100 if acceleration==1
label var contribution_hc_after2 "Contribution of qual. adj. workforce per capita after an acceleration (in ppt)"
by countrycode: gen contribution_tfp_after2=((lnTFP_cap[_n+17]-lnTFP_cap[_n+7])/10)*100 if acceleration==1
label var contribution_tfp_after2 "Contribution of TFP after an acceleration (in ppt)"

gen difference_growth2=growth_during2-growth_before2
label var difference_growth2 "difference between the growth rate during and before an acceleration"
gen difference_cap2=contribution_cap_during2-contribution_cap_before2
label var difference_cap2 "difference between the contribution of physical capital to the growth rate change"
gen difference_hc2=contribution_hc_during2-contribution_hc_before2
label var difference_hc2 "difference between the contribution of human capital to the growth rate change"
gen difference_tfp2=contribution_tfp_during2-contribution_tfp_before2
label var difference_tfp2 "difference between the contribution of TFP to the growth rate change"

gen growth_change_cap2=(difference_cap2/difference_growth2)*100
label var growth_change_cap2 "% of the growth rate change that can be explained by capital accumulation"
gen growth_change_hc2=(difference_hc2/difference_growth2)*100
label var growth_change_hc2 "% of the growth rate change that can be explained by human capital accumulation"
gen growth_change_tfp2=(difference_tfp2/difference_growth2)*100
label var growth_change_tfp2 "% of the growth rate change that can be explained by developments in TFP"

*Development of population and employment during accelerations (section 5.5)
gen lnpop=ln(pop)
label var lnpop "natural logarithm of population"
gen lnemp=ln(emp)
label var lnemp "natural logarithm of employment"

sort countrycode year
by countrycode: gen growth_pop_before=((lnpop-lnpop[_n-7])/7)*100 if acceleration==1
label var growth_pop_before "average annual growth rate of population in the pre-accelerion period"
by countrycode: gen growth_pop_during=((lnpop[_n+7]-lnpop)/7)*100 if acceleration==1
label var growth_pop_during "average annual growth rate of population during an acceleration"
by countrycode: gen growth_pop_after=((lnpop[_n+17]-lnpop[_n+7])/10)*100 if acceleration==1
label var growth_pop_after "average annual growth rate of population after an acceleration"
by countrycode: gen growth_emp_before=((lnemp-lnemp[_n-7])/7)*100 if acceleration==1
label var growth_emp_before "average annual growth rate of employment in the pre-acceleration period"
by countrycode: gen growth_emp_during=((lnemp[_n+7]-lnemp)/7)*100 if acceleration==1
label var growth_emp_during "average annual growth rate of employment during an acceleration"
by countrycode: gen growth_emp_after=((lnemp[_n+17]-lnemp[_n+7])/10)*100 if acceleration==1
label var growth_emp_after "average annual growth rate of employment after an acceleration"

*Predicting growth accelerations (section 6)
*Deviation of capital accumulation from the trend in pre-acceleration period 
xtset country_id year
tsfilter hp deviation_capworker = lncap_worker, trend(trend_lncapworker)
sort countrycode year
bys countrycode: asrol deviation_capworker, stat(mean) window (year -8 0) min(8)
rename deviation_capworker_mean8 meandeviation_capworker_before

gen lncap=ln(rnna)
label var lncap "natural logarithm of physical capital stock"
tsfilter hp deviation_capital = lncap, trend(trend_lncap)
sort countrycode year
bys countrycode: asrol deviation_capital, stat(mean) window(year -8 0) min(8)
rename deviation_capital_mean8 meandeviation_capital_before

tsfilter hp deviation_hc = lnhc, trend(trend_lnhc)
sort countrycode year
bys countrycode: asrol deviation_hc, stat(mean)  window (year -8 0) min(8)
rename deviation_hc_mean8 meandeviation_hc_before

*Dependent variable binary choice model
generate window_start_acceleration=0
replace window_start_acceleration=1 if acceleration==1
by countrycode: replace window_start_acceleration=1 if acceleration[_n-1]==1
by countrycode: replace window_start_acceleration=1 if acceleration[_n+1]==1

generate window_start_sustained=0
replace window_start_sustained=1 if sustained==1
by countrycode: replace window_start_sustained=1 if sustained[_n-1]==1
by countrycode: replace window_start_sustained=1 if sustained[_n+1]==1

generate window_start_unsustained=0
replace window_start_unsustained=1 if acceleration==1 & sustained!=1
by countrycode: replace window_start_unsustained=1 if acceleration[_n-1]==1 & sustained[_n-1]!=1
by countrycode: replace window_start_unsustained=1 if acceleration[_n+1]==1 & sustained[_n+1]!=1

*Other variables
sort countrycode year
bys countrycode: asrol growth, stat(mean) window(year -3 -1) min(2)
rename mean_3_growth averagegrowth_2_0
gen postcrisis=.
replace postcrisis=0 if acceleration==1
replace postcrisis=1 if acceleration==1 & averagegrowth_2_0<-0.05
label var postcrisis "dummy equal to 1 for a post-crisis acceleration, 0 otherwise "

bysort year: egen hcmean=mean(hc)
gen hcpoor=0
replace hcpoor=1 if hc<hcmean
replace hcpoor=. if hc==.

sort countrycode year
by countrycode: gen growth_capworker_before=((lncap_worker-lncap_worker[_n-7])/7)*100 
label var growth_capworker_before "average annual growth rate of physical capital per worker for (t-7,t)"

sort countrycode year
by countrycode: gen growth_hc_before=((lnhc-lnhc[_n-7])/7)*100
label var growth_hc_before "average annual growth rate of human capital for (t-7,t)"

tabulate country if meandeviation_capworker_before<-0.1 & acceleration==1
gen outlier_devcapworker=0
replace outlier_devcapworker=1 if key==8941 | key==2618 | key==3545 | key==4646 | key==7892 | key==9266 | key==10052
by countrycode: replace outlier_devcapworker=1 if outlier_devcapworker[_n-1]==1 & outlier_devcapworker[_n-2]==0
by countrycode: replace outlier_devcapworker=1 if outlier_devcapworker[_n+1]==1
label var outlier_devcapworker "dummy for countries with average deviation of cap/worker from trend <-10%"

tabulate country if meandeviation_capworker_before>0.1 & acceleration==1
gen outlier_devcapworker2=0
replace outlier_devcapworker2=1 if key==2424 | key==8026 | key==9082 | key==9285
by countrycode: replace outlier_devcapworker2=1 if outlier_devcapworker2[_n-1]==1 & outlier_devcapworker2[_n-2]==0
by countrycode: replace outlier_devcapworker2=1 if outlier_devcapworker2[_n+1]==1
label var outlier_devcapworker2 "dummy for countries with average deviation of cap/worker from trend >10%"

gen low_gr_lag7=0
tabulate country if gr_lag7<-0.02 & acceleration==1
replace low_gr_lag7=1 if key==114 | key==251 | key==264 | key==1168 | key==4646 | key==4808 | key== 6391 | key==6739 | key==7534 | key==7960 | key==8026 | key== 7754 | key== 8861 | key==9038 | key==9785 | key==10347
by countrycode: replace low_gr_lag7=1 if low_gr_lag7[_n-1]==1 & low_gr_lag7[_n-2]==0
by countrycode: replace low_gr_lag7=1 if low_gr_lag7[_n+1]==1
label var low_gr_lag7 "dummy equal to 1 for countries with growth lower than -2% before an acceleration"

*Dropping years t+2 to t+4 of an acceleration 
drop if acceleration[_n-2]==1
drop if acceleration[_n-2]==1
drop if acceleration[_n-2]==1

*Testing for country fixed effects
clogit window_start_acceleration meandeviation_capworker_before i.year if change_growth!=., group(country_id) 
estimates store CLOGIT
logit window_start_acceleration meandeviation_capworker_before i.year if change_growth!=.
estimates store PLOGIT
hausman CLOGIT PLOGIT

*Comparison with conditional fixed effects logit model
clogit window_start_acceleration meandeviation_capworker_before i.year i.region if change_growth!=., group(country_id) robust

*Main probit regressions (table 6)
probit window_start_acceleration meandeviation_capworker_before i.region i.year if change_growth!=., vce(cluster country_id)
margins, dydx(meandeviation_capworker_before i.region) post
estimate store col1

probit window_start_acceleration meandeviation_capworker_before i.region i.year if change_growth!=. & capitalpoor==1, vce(cluster country_id)
margins, dydx(meandeviation_capworker_before i.region) post
estimate store col2 

probit window_start_acceleration meandeviation_capworker_before i.region i.year if change_growth!=. & capitalpoor==0, vce(cluster country_id)
margins, dydx(meandeviation_capworker_before i.region) post
estimate store col3

probit window_start_acceleration meandeviation_hc_before i.region i.year if change_growth!=., vce(cluster country_id)
margins, dydx(meandeviation_hc_before i.region) post
estimate store col4

probit window_start_acceleration meandeviation_hc_before i.region i.year if change_growth!=. & hcpoor==1, vce(cluster country_id)
margins, dydx(meandeviation_hc_before i.region) post
estimate store col5

probit window_start_acceleration meandeviation_hc_before africa americas asia europe meast i.year if change_growth!=. & hcpoor==0, vce(cluster country_id)
margins, dydx(meandeviation_hc_before africa americas asia europe meast) post
estimate store col6

*Deviation of physical capital from trend (appendix table 7)
probit window_start_acceleration meandeviation_capital_before i.region i.year if change_growth!=., vce(cluster country_id)
margins, dydx(meandeviation_capital_before i.region) post

probit window_start_acceleration meandeviation_capital_before i.region i.year if change_growth!=. & capitalpoor==1, vce(cluster country_id)
margins, dydx(meandeviation_capital_before i.region) post

probit window_start_acceleration meandeviation_capital_before i.region i.year if change_growth!=. & capitalpoor==0, vce(cluster country_id)
margins, dydx(meandeviation_capital_before i.region) post

*Robustness (appendix table 8)
probit window_start_acceleration meandeviation_capworker_before meandeviation_hc_before i.region i.year if change_growth!=., vce(cluster country_id)
margins, dydx(meandeviation_capworker_before meandeviation_hc_before i.region) post
est store rcol1

probit window_start_acceleration meandeviation_capworker_before meandeviation_hc_before i.region i.year if change_growth!=. &  low_gr_lag7!=1, vce(cluster country_id)
margins, dydx(meandeviation_capworker_before meandeviation_hc_before i.region) post
est store rcol2

reg window_start_acceleration meandeviation_capworker_before meandeviation_hc_before i.region i.year if change_growth!=., vce(cluster country_id)
predict phat
sum phat
est store rcol3

probit window_start_acceleration meandeviation_capworker_before meandeviation_hc_before i.region i.year if change_growth!=. & outlier_devcapworker!=1, vce(cluster country_id)
margins, dydx(meandeviation_capworker_before meandeviation_hc_before i.region) post
est store rcol6
