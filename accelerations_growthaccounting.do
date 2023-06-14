* Growth accounting variables and growth rates (1.1-1.3)

*****

* 1 Growth accounting variables and growth rates 
* 1.1 Growth accounting variables
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

* 1.2 Growth rates using log-differences
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

* 1.3 Differences between the growth rates & contributions to the growth rate change 
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

* 2 Sub-sample analysis 
* 2.1 Capital-poor dummy 
gen capoutput=rnna/rgdpna
label var capoutput "capital-output ratio (rnna/rgdpna)"
bysort year: egen capoutputmean=mean(capoutput)
gen capitalpoor=0
replace capitalpoor=1 if capoutput<capoutputmean
replace capitalpoor=. if capoutput==.
label var capitalpoor "dummy equal to 1 for capital-poor countries and 0 for capital-rich countries"

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

*Growth accounting in per-capita terms (section 5.5)
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
