************************************************************
*** STATA Replication Code for [authors  blinded] (2023) ***
***         and to estimate growth accelerations         ***
************************************************************
******* BEFORE STARTING: *************
* 1. Set a WORKING DIRECTORY (folder) in line 15.
*    This folder should already exist on your computer.
*    The global defined in line 15 will ensure the code can access access this folder on various code lines.
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

*** remaining code will be added after paper publication