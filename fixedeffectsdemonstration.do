clear all

set seed 123

/* Number of observations */
set obs 1000

/* Coarse geographic fixed effects (provinces) */
gen provinces = cond(uniform() < 0.33, "A", cond(uniform() < 0.66, "B", "C"))

/* Fine geographic fixed effects (cities) nested within provinces */
gen cities = provinces + cond(uniform() < 0.33, "_1", cond(uniform() < 0.6, "_2", "_3"))

/* Independent variable */
gen x = rnormal()

/* Outcome variable now dependent on the coarse geographic fixed effect */
gen provinces_values = cond(provinces == "A", 0.1, cond(provinces == "B", 0.2, 0.3))
gen y = 2*x + provinces_values + rnormal()

/* Run hdfe regressions for each of these cases using reghdfe */

/* Control for cities */
reghdfe y x, absorb(cities)

/* Control for cities and provinces */
reghdfe y x, absorb(cities provinces)

/* Control only provinces */
reghdfe y x, absorb(provinces)

encode provinces, generate(provinces2)
encode cities, generate(cities2)

/* Control for cities */
reg y x i.cities2

/* Control for cities and provinces */
reg y x i.cities2 i.provinces2

/* Control only provinces */
reg y x i.provinces2

