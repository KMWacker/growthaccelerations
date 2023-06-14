*Importing and cleaning the data
cd "$path"
use "https://www.rug.nl/ggdc/docs/pwt100.dta"
drop currency_unit rgdpe rgdpo ccon cda cgdpe cgdpo cn ck ctfp cwtfp rconna rdana irr delta xr pl_con pl_da pl_gdpo i_cig i_xm i_xr i_outlier i_irr cor_exp statcap csh_c csh_i csh_g csh_x csh_m csh_r pl_c pl_i pl_g pl_x pl_m pl_n pl_k

*Dropping countries with population smaller than 1 million in 2019
reshape wide country pop emp avh hc rgdpna rnna rkna rtfpna rwtfpna labsh, i(countrycode) j(year)
drop if pop2019<1
reshape long

*Identifying potential takeoffs of growth accelerations 
gen rgdpcap=rgdpna/pop
label var rgdpcap "real GDP per capita (rgdpna/pop)"
gen lnrgdpcap=ln(rgdpcap)
label var lnrgdpcap "natural logarithm of real GDP per capita (rgdpna/pop)"
sort countrycode year
by countrycode: gen growth=lnrgdpcap-lnrgdpcap[_n-1]
label var growth "economic growth rate from period t-1 to period t"
gen takeoff=0
by countrycode: replace takeoff=1 if growth[_n+1]>growth
label var takeoff "dummy equal to 1 if growth for year t+1 is > than in the year before, 0 otherwise"

*Obtaining the least squares growth rate from t to t+7
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
replace pot_episode=1 if takeoff==1 & gr0_7>=0.035 & gr0_7!=. & change_growth>=0.02 & change_growth!=. & rgdpcap_Lead7>=record & rgdpcap_Lead7!=.
label var pot_episode "dummy variable which is 1 when all conditions are met and 0 otherwise"

*Identifying accelerations
bysort countrycode (year) : gen pot_spell = cond(pot_episode == 0,  0, sum(pot_episode == 1 & pot_episode[_n-1] == 0)) 
egen min = min(rgdpcap) if pot_spell, by(countrycode pot_spell) 
label var min "minimum level of real GDP per capita for every pot_spell"
gen acceleration=0
replace acceleration=1 if rgdpcap==min
replace acceleration=0 if pot_episode==0
label var acceleration "growth acceleration according to the methodology by Jong-A-Pin & De Haan"

*Eliminating episodes within 5 years of other episodes
replace acceleration=0 if acceleration==1 & acceleration[_n-1]==1 | acceleration[_n-2]==1 | acceleration[_n-3]==1 | acceleration[_n-4]==1

*Recovering episodes which are more than 5 years apart from other episodes
replace acceleration = cond(acceleration==0 & pot_episode==1 & rgdpcap==min & acceleration[_n-1]==0 & acceleration[_n-2]==0 & acceleration[_n-3]==0 & acceleration[_n-4]==0,1,0) if acceleration!=1

*Identifying sustained growth accelerations 
generate sustained=0
replace sustained=1 if acceleration==1 & gr7_17>=0.02 & gr7_17!=.
label var sustained "dummy equal to 1 if a sustained growth acceleration starts in this year"

*Add region (dummies)
cd "/Users/elinekoopman/ownCloud/Shared/Growthaccelerations/datasets"
merge m:1 countrycode using regions.dta
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

*Growth accounting
*Variables
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
