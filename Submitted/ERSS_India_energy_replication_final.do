/// Martinez-Arranz, Thomson et al. 
/// The uneven expansion of electricity supply in India: the logics of clientelism, incrementalism and maximin
/// Energy Research and Social Science
/// Replication file, 26th April 2021

use ERSS_India_energy_replication.dta

/// Descriptives for Table 1

/// Locations with 24 hr access and < 24 hr access in 2014 by state and party grouping controlling state
tab2 state med14is24 if stgov_alliance==1
tab2 state med14is24 if stgov_alliance==2
tab2 state med14is24 if stgov_alliance==3

/// For locations with <24 hr access in 2014, median hrs power supply in 2014, improvement 2014-2019, and median power supply in 2019
tabulate state if median_power_hrs<24, summarize(median_power_hrs)
tabulate state if median_power_hrs<24, summarize(improvemed1419)
tabulate state if median_power_hrs<24, summarize(median_power_hrs2019)

/// Households surveyed by state and total
table state, contents(sum hh14plus19)
table one, contents(sum hh14plus19)

/// Models in Table 2
/// Model 1 All states
regress improvemed1419 i.alliancematch2##c.perc_marg i.stgov_alliance mean_expec_hrs ///
median_power_hrs  prop_schedback total_income  i.region_type i.new_plants_within_300k if median_power_hrs<24 [pweight=hh_min], vce (cluster state)
estimates store m1
/// BJP states
regress improvemed1419 i.alliancematch2##c.perc_marg i.stgov_alliance mean_expec_hrs ///
median_power_hrs  prop_schedback total_income  i.region_type i.new_plants_within_300k if median_power_hrs<24 & stgov_alliance==1 [pweight=hh_min], vce (cluster state)
estimates store m2
regress improvemed1419 i.alliancematch2##c.perc_marg i.stgov_alliance mean_expec_hrs ///
median_power_hrs  prop_schedback total_income  i.region_type i.new_plants_within_300k if median_power_hrs<24 & stgov_alliance==1 [pweight=hh_min]
/// INC states
regress improvemed1419 i.alliancematch2##c.perc_marg i.stgov_alliance mean_expec_hrs ///
median_power_hrs  prop_schedback total_income  i.region_type i.new_plants_within_300k if median_power_hrs<24 & stgov_alliance==2 [pweight=hh_min], vce (cluster state)
estimates store m3
regress improvemed1419 i.alliancematch2##c.perc_marg i.stgov_alliance mean_expec_hrs ///
median_power_hrs  prop_schedback total_income  i.region_type i.new_plants_within_300k if median_power_hrs<24 & stgov_alliance==2 [pweight=hh_min]
/// Third Front states
regress improvemed1419 i.alliancematch2##c.perc_marg i.stgov_alliance mean_expec_hrs ///
median_power_hrs  prop_schedback total_income  i.region_type i.new_plants_within_300k if median_power_hrs<24 & stgov_alliance==3 [pweight=hh_min], robust
estimates store m4
esttab m1 m2 m3 m4 using full_df.txt, replace modelwidth(10) addnote("Standard errors in parentheses") starlevels(* 0.10 ** 0.05 *** 0.01) mlabel("Model 1 All" "Model 2 BJP" "Model 3 INC" "Model 4 Third Front" ) legend cells(b(star fmt(3)) se(par)) stats( N, star ) label varwidth(30)  collabels(none)

/// Figure 2 The effect of partisan match between state and local levels on improvements in electricity supply by electoral margin
/// Model 1 All states
regress improvemed1419 i.alliancematch2##c.perc_marg i.stgov_alliance mean_expec_hrs ///
median_power_hrs  prop_schedback total_income  i.region_type i.new_plants_within_300k if median_power_hrs<24 [pweight=hh_min], vce (cluster state)

margins r.alliancematch2, at(perc_marg=(0(1)20))
marginsplot, yline(0) scheme(s1mono)

/// Figure 3 The effects of the quality of neighboring supply and hours of supply in 2014 on improvements in electricity supply
margins, at(mean_expec_hrs=(0(1)24))
marginsplot, name(a, replace) scheme(s1mono)

margins, at(median_power_hrs=(0(1)23))
marginsplot, name(a, replace) scheme(s1mono)

/// END
