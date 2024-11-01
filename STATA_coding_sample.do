			
							/********************
		
								CODE OVERVIEW
		
		This dofile merges, cleans, and analyzes car model sales in five
		different European car markets from 1970-1990. The main objective is
		to estimate the causal impact of fuel efficiency on car sales.
		
							********************
		
									DATA
		
		- car data (one csv for each year, contains all car models)
		- market data (one csv for each country, contains all years)
		
							********************
		
								CODE SECTIONS
		
		- Data cleaning (merge car data and market data into a panel with 3
		dimensions: car model, market, and year. Each row should have the
		year, car model characteristics, and market data of that year)
		
		- Exploratory analysis (preliminary analysis of fuel consumption and
		car model characteristics) (Figures and tables generated)
		
		- Main regressions (fit two way fixed effects model to estimate impact of
		a fuel efficiency indicator on model sales) (Tables generated)
		
							 ********************/
		
		clear all
		
		/* Set globals to specify where to find the data, also where to output
		graphs and tables */
		global dir "/Users/jimmytyung/Desktop/untitled_folder/koichiro_ito"
		global car_data "$dir/car_data"
		global market_data "$dir/market_data"
		global output "$dir/output"
		
		******************** DATA CLEANING ********************
		
		
		/* To stack each year's car model sales data, first
		need to convert format from csv to dta */
		forvalues i = 70/90 {
			import delimited "$car_data/car_data_`i'.csv", clear
			
			// the csv for 1977 marks missing values in "sp" variable as "NA"
			// which interferes with conversion, so replace it with empty string
			if `i' == 77 { 
				replace sp = "" if sp=="NA"
				destring sp, replace
			}
			
			save "$car_data/car_data_`i'.dta", replace
		}
		
		/* Stack data each year's car model sales data */
		use "$car_data/car_data_70.dta", clear
		forvalues i = 71/90{
			append using "$car_data/car_data_`i'.dta"
		}
		
		/* Minor cleaning to ensure compatibility with market data which will
		be merged with later */
		replace ye = ye+1900
		replace ma = "United Kingdom" if ma == "UK"
		
		/* Save stacked dataset */
		save "$car_data/car_data_stacked.dta", replace
		
		/* Create a list of countries to loop over their market data csv. In
		the loop, we conduct necessary cleaning and convert data to .dta 
		to be merged later with stacked cars dataset */
		global countries B F G I U
		foreach c in $countries {
			import delimited "$market_data/market_data_`c'.csv", clear
			
			// indicate the specific market by generating variable "ma"
			if "`c'" == "B" {
				gen ma = "Belgium"
			}
			if "`c'" == "F" {
				gen ma = "France"
			}
			if "`c'" == "G" {
				gen ma = "Germany"
			}
			if "`c'" == "I" {
				gen ma = "Italy"
			}
			if "`c'" == "U" {
				gen ma = "United Kingdom"
			}
			
			// Rename variable names in each market csv to address probelm that
			// column names contain suffix that indicate the specific market,
			// e.g. GDP is named gdp_U to indicate the UK.
			// Since we have created the "ma" variable above, this distinction
			// is no longer necessary to indicate the market
			foreach v of varlist *{
				if "`v'" != "ye" & "`v'" != "ma" {
					local n = substr("`v'", 1, length("`v'") - 2)
					rename `v' `n'	
				}
			}
			// Convert to .dta
			save "$market_data/market_data_`c'.dta", replace
		}
		
		/* Open previously created stacked car model dataset and 
		merge with market data on variables ma and ye, which correspond to
		market and year respectively, to finish constructing panel data 
		containing car model, year, and market info in each row */
		use "$car_data/car_data_stacked.dta", clear
		foreach c in $countries {
			merge m:1 ye ma using "$market_data/market_data_`c'.dta", nogen update
		}
		
		/* Sort in ascending order by car model, market, and year */
		sort model ma ye		
		
		/* The fuel consumption variables: fuel consumption at speed 1 (li1), 
		at speed 2 (li2), at speed (3), and average of all speeds (li) contain 
		missing values which can be reverse engineered using non-missing 
		related variables, i.e. li1 can be solved if we have li2, li3, and li. 
		Below, we fill in these missing values. No more than 1 of these variables
		are missing at the same time. */
		
		/* Fuel consumption should be numeric, but is currently read as a string. 
		Below, we convert "NA" values which cannot be read as numeric to 
		empty strings to aid conversion process later */
		replace li = "" if li=="NA"
		replace li1 = "" if li1=="NA"
		replace li2 = "" if li2=="NA"
		replace li3 = "" if li3=="NA"
		
		/* Convert fuel consumption variables from string to numeric */
		destring li1 li2 li3 li, replace
		
		/* Previous empty strings are converted to . , which indicates a missing
		value and the values we aim to reverse engineer. First we completely fill 
		in the average fuel consumption variable, "li" */
		replace li = (li1+li2+li3)/3 if li==.
		
		/* For missing speed-specific fuel consumption variables, we fill them in
		using the average fuel consumption calculated above */
		foreach v in li1 li2 li3 {
			replace `v' = 0 if 	`v'==. 
			replace `v' = li*3 - (li1 + li2 + li3) if `v' == 0
		}
		
		/* Save completed panel dataset */
		save "$dir/panel.dta", replace
		
		
		
		
		******************** EXPLORATORY ANALYSIS ********************
		
		/********** 
		SECTION OVERVIEW
		
		In this section, we create 3 outputs: 
		1. Time series plot of sales-weighted average of fuel consumption by vehicle classes
		
		2. Summary statistics table with sales-weighted average fuel consumption, 
		its standard deviation, 25th percentile, median, and 75th percentile
		
		3. Scatterplot of the sales-weighted average of fuel consumption versus the
		(unweighted) midpoint of each power-to-weight decile to analyze the relationship 
		between fuel consumption and power-to-weight ratio. Do this only for
		1970 and 1990. 
		**********/
		
		
		/* OUTPUT 1:
		Create a time series plot of sales-weighted average of fuel consumption
		by vehicle classes. First we do some necessary processing. */
		
		/* Currently, vehicle classes are read as strings which interferes with
		aggregating the data by class and year, hence, we convert the class variable
		to a factor variable, which changes its format by assigning an integar, called
		factor levels, to each distinct string value */
		encode cla, gen(cla_temp)
		drop cla
		rename cla_temp cla
				
		/* Save changes to dataset */
		save "$dir/panel.dta", replace
		
		/* Since we will be altering the dataset, we create a  new frame 
		called plots, and keep the unaltered dataset in the default frame
		to be used later */ 
		frame create plots
		frame change plots
		use "$dir/panel.dta", clear		
		
		/* Collapse data to generate sales-weighted average fuel consumption bysort
		vehicle class and year */
		collapse (mean) li [aweight=qu], by(cla ye)
		
		/* Generate graph */
		graph twoway ///
		(line li ye if cla==1, color(green*0.5)) ///
		(line li ye if cla==2, color(green*0.2)) ///
		(line li ye if cla==3, color(red)) ///
		(line li ye if cla==4, color(red*0.4)) ///
		(line li ye if cla==5, color(green)), ///
		ytitle("Average Fuel Consumption (L/km)") ///
		xtitle("Year") ///
		legend(order(3 4 2 1 5) ///
				label(1 "Compact") ///
				label(2 "Intermediate") ///
				label(3 "Luxury") ///
				label(4 "Standard") ///
				label(5 "Subcompact"))
				
		/* Export time series graph of each vehicle class */
		graph export "$output/fuel_by_class_year.png", replace
		
		/* Return to default frame which has the uncollapsed dataset in memory */
		frame change default
		
		/* OUTPUT 2:
		Generate summary statistics of fuel consumption in 1990 by class.
		First, we create labels for the fuel consumption variable and each 
		factor level that will be displayed in the table later */
		label variable cla "Class"
		label define cla_labels 1 "Compact" 2 "Intermediate" 3 "Luxury" 4 "Standard" 5 "Subcompact"
		label values cla cla_labels
		
		/* Generate and export summary statistics table with sales-weighted average
		fuel consumption, its standard deviation, 25th percentile, median, and 75th
		percentile, rounded to 2 decimal places */
		table cla if ye==1990 [aweight=qu], statistic(mean li) statistic(sd li)  statistic(q1 li) statistic(median li) statistic(q3 li) nototal nformat(%3.2f)
		collect export "$output/fuel_cons_1990.tex", tableonly replace
		
		
		
		/* OUTPUT 3:
		Scatterplot of the sales-weighted average of fuel consumption versus the
		(unweighted) midpoint of each power-to-weight decile. Do this only for
		1970 and 1990. First, we generate a new variable measuring fuel efficiency, 
		the power-to-weight ratio */
		gen pwr = hp/we 
		
		/* Save new changes to dataset before we drop observations not in 
		target years */
		save "$dir/panel.dta", replace
		
		/* Keep observations if they are of years 1970 or 1990 */
		keep if ye == 1970 | ye == 1990
		
		/* Generate variable indicating which year-specific power-to-weight decile each
		car model is in */
		egen decile = xtile(pwr), nq(10) by(ye)

		/* Generate sales-weighted average of fuel consumption of each decile */
		bysort ye decile: asgen w_avg = li, w(qu)
		
		/* Generate median of power-to-weight ratio of each year-specific decile */
		bysort ye decile: egen med = median(pwr)
		
		/* Create scatter plot weighted average and median of pwr
		and regress fuel cons on constant and pwr, weighted to visualize 
		relationship between fuel consumption and power-to-weight ratio */
		twoway (scatter w_avg med if ye==1970, color(blue)) ///
		(lfit w_avg med [aweight=qu] if ye==1970, color(black) lpattern(dot)) ///
		(scatter w_avg med if ye==1990, color(red)) ///
		(lfit w_avg med [aweight=qu] if ye==1990, color(black) lpattern(dash_dot)) ///
		, xlabel(0(0.01)0.1) ///
		ylabel(5(1)13) ///
		ytitle("Weighted Average Fuel Consumption (L/km)") ///
		xtitle("Power to Weight Ratio (watts/gram)") ///
		legend(label(1 "1970")label(2 "Fitted 1970")label(3 "1990")label(4 "Fitted 1990"))
		
		/* Export scatter plot */
		graph export "$output/avg_med.png", replace
		
		
		******************** MAIN REGRESSIONS ********************
		
		
		/********** 
		SECTION OVERVIEW
		
		In this section, analyze the causal impact of an increase in fuel efficiency
		of a car model on its sales performance by fitting a two way fixed effects model.
		
		We create 1 output: 
		1. Table of regression results.
		**********/
		
		/* Reload full dataset with more than just data of 1970 and 1990 */
		use "$dir/panel.dta", clear
		
		/* Construct outcome variable, the ratio of the a car model's log market 
		share in a market, in a specific year, to the log share of consumers who 
		buy no cars in that market in that year. We infer the latter by assuming
		an average household size of 4, and each household requiring 1 car.
		We calculate this using the population variable in our dataset. */
		
		// First, generate the total car sales in each market
		bysort ye ma: egen qu_tot = sum(qu)
		
		// Second, generate number of theoretical maximum sales in a market
		gen N = pop/4
		
		// Third, generate share of the market that purchased cars by dividing
		// total car sales by theoretical maximum sales.
		gen s_i = qu / N
		
		// Fourth, calculate the opposite - "untapped market" by finding the
		// complement of share of the market that purchased cars
		gen s_0 = 1 - (qu_tot/N)
		
		// Last, apply log to each component to help interpret coefficients as
		// percentage changes later.
		gen sales_ratio = ln(s_i) - ln(s_0)
		
		
		/* Add labels to variables to be displayed in regression results table later */
		label variable sales_ratio "Model Sales to Untapped Sales Ratio"
		label variable pwr "Power to Weight Ratio (watts/gram)"
		label variable home "Home Market"
		label variable eurpr "Price (in common currency)"
		
		/* Fit model 1, regressing outcome on power-to-weight ratio, dummy for if
		the model is sold in the country where it is created, and their interaction */
		eststo model1: reg sales_ratio c.pwr##ib0.home eurpr
		
		/* Fit model 2, same as model 1 with added car model, market, and year 
		fixed effects. First, we convert "model" and "market" variables from 
		string to factor variables so they can be entered into the regression
		command for Model 2. */
		encode model, gen(model_t)
		drop model
		rename model_t model
		
		encode ma, gen(ma_t)
		drop ma
		rename ma_t ma
		
		/* Fit model 2 */
		eststo model2: reghdfe sales_ratio c.pwr##ib0.home eurpr, absorb(i.model i.ma i.ye)
		estadd local modelfe "Yes"
		estadd local market "Yes"
		estadd local year "Yes"
		
		/* Export table of results */
		esttab model1 model2 using "$output/model1&2.tex", nobaselevels label b(%7.4f) se(%7.4f) star ar2 scalars("modelfe Model FE" "market Market FE" "year Year FE") replace 
		
		exit
		