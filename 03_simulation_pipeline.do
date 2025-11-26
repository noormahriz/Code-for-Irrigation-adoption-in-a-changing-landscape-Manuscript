* Irrigation Adoption Simulation Script
* Author: Noormah Rizwan
* Purpose: Predict new adopters under future climate 2060 Dry scenarios


* ---------- Load Baseline Data ----------
clear
use "Baseline/irrigation_adoption_baseline.dta", clear

winsor2 field_area, cuts(5 90)
encode opstina, gen(new_opstina)
encode ko, gen(new_ko)
rename mean elevation

* Create groundwater cost categories
encode aquif_name, gen(new_aquif_name)
gen gw_costs = 1 if inlist(new_aquif_name, 1, 2, 3)
replace gw_costs = 2 if inlist(new_aquif_name, 4, 5)
label define gw_costs 1 "Low Pumping Costs" 2 "High Pumping Costs", modify
label values gw_costs gw_costs
drop new_aquif_name

* ---------- Estimate Adoption Model ----------
set seed 12345
logit irrigation_adoption mean_temperature_lag above_35_lag soil_moisture_lag ///
      historical_mean_temperature historical_soil_moisture awc elevation ///
      field_area_w distance_to_nearest_canal i.gw_costs i.new_opstina, vce(cluster ko)

predict choice_hat if e(sample)

* ---------- Mark Currently Irrigated ----------
append using "irrigation_2020.dta"
bysort id: egen irrigated = total(irrigation_adoption)
replace irrigated = 1 if irrigated > 0
replace irrigated = 0 if irrigated == 0



**Bring in 2060 data
merge 1:1 id using "/2060/Dry/irrigation_adoption_2060_dry.dta"
keep if _merge ==3
drop _merge

* ---------- Replace Current Conditions with Future Data ----------
foreach var in mean_temperature_lag above_35_lag soil_moisture_lag ///
               historical_mean_temperature historical_soil_moisture {
    drop `var'
    gen `var' = future_`var'
}


summ choice_hat choice_hat1
* ---------- Calculate Change in Predicted Probabilities ----------
* Set number of adopters in base year
local base_adopters = 15416  

quietly summarize choice_hat
local base_prob = r(mean)
quietly summarize choice_hat1
local new_prob = r(mean)

local pct_increase = `new_prob' - `base_prob'
di "Percentage point increase: `pct_increase'"
**9.2989  increase and original so total fields increase 15416 * 9.298865 is 143351 fields. 64141 fields come from 2050, so 79210 fields should be from 2060

keep id irrigated choice_hat choice_hat1

rename choice_hat choice_hat_2060
rename choice_hat1 choice_hat1_2060

**Drop Baseline fields adoptiong irrigation
drop if irrigated==1
drop irrigated


**Bring in new adopters from 2050
merge 1:1 id using "/2050/Dry/irrigation_additional_fields_2050.dta"
drop  _merge

**Drop new adopters from 2050
drop if irrigation_adoption==1
drop irrigation_adoption


gsort  -choice_hat1_2060
gen irrigation_adoption_2060 = 0
replace irrigation_adoption_2060 = 1 if (_n <= 79210)

**Bring in new adopters from 2050
merge 1:1 id using "/2050/Dry/irrigation_additional_fields_2050.dta"
drop _merge

rename irrigation_adoption irrigation_adoption_2050

gen irrigation_adoption = 0
replace irrigation_adoption = 1 if irrigation_adoption_2050 ==1|irrigation_adoption_2060==1


keep id irrigation_adoption

export delimited using "/2060/Dry/predicted_irrigated.csv", replace

save "/2060/Dry/irrigation_additional_fields_2060.dta", replace


