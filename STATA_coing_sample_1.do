		/********************
		
		CODE OVERVIEW
		
		This do.file prepares and analyzes the student test and teacher experience
		data from the 2005-2007 Western Kenya School Tracking RCT to estimate the
		impact of tracking on student performance. 
		
		Code is run on STATA 18
		Code uses the following packages:
		> estout (run 'ssc install estout' if needed)
		
		********************
		
		DATA
		> student_test_data.csv :: contains student characteristics and test results
		> teacher_data.dta :: contains teacher's teaching experience
		
		********************
			
		CODE SECTIONS
		
		1. Directory, Paths, and Seed Set Up (user-specific, needs to be changed)
		2. Data Preparation
		3. Analysis
		********************/
							 
							 
							 
					
		************************************************************************
		****************** DIRECTORY, PATHS, AND SEED SET UP *******************
		************************************************************************
		
		// Clear any stored memory, ensures a clean slate before beginning
		clear all
		
		// Specify working directory and where to locate needed datasets
		global dir "/Users/cid"
		global student_data "$dir/student_test_data.csv"
		global teacher_data "$dir/teacher_data.dta"
		
		// Specify desired filename of cleaned student and teacher datasets
		global cleaned_student_test_data "$dir/cleaned_student_test_data.dta"
		global rand_sample_teachers_data "$dir/rand_sample_teachers.csv"
		global student_teacher_merged_data "$dir/student_teacher_merged.dta"
		
		// Specify desired filename of regression table and bar graph
		global reg_results_dest "$dir/regression_results.tex"
		global bar_graph_dest "$dir/ascore_bar_graph.png"
		
		// Specify seed number for randomization
		set seed 41416141
		
		************************************************************************
		*************************** DATA PREPARATION ***************************
		************************************************************************
		
		
		* CLEAN STUDENT DATA ***************************************************
		
		/* -- Load Student Data ----------- */
		import delimited $student_data, clear
		
		
		/* -- Drop PII -----------
		Create a list of variables that contain personally identifiable information.
		Date of birth when combined with other variables like girl can uniquely
		identify someone. */ 
		global pii_vars dob
		drop $pii_vars // drop PII var(s)
		
		
		
		/* --  Check for Duplicates in pupilid -----------
		
		Print warning message if duplicates are found.*/
		quietly duplicates report pupilid
		if (r(unique_value)!=r(N)) {
			dis "Mismatch in Number of Observations and Number of Unique pupilID"
		} 
		
		
		/* -- Recode Arithmetic Score Variables to Numberic -----------
		
		Looping through the 8 arithmetic score variables, replacing NONE with 0
		and converting to numeric from string */
		forval x = 1/8 {
			replace a`x'_correct = "0" if a`x'_correct == "NONE"
			destring a`x'_correct, replace
		}
		
		/* -- Drop Missing Values -----------
		
		Drop observations with -99 in Word Reading and/or Sentence Reading variables
		by looping through them and checking for -99 values */
		foreach v in w s {
			drop if `v'_correct == -99
			drop if `v'_incorrect == -99
			drop if `v'_missing == -99
		} // dropped 34 obs with w_correct = -99 and 29 obs with s_correct = -99
		
		
		// 1 obs with Word Reading variables = -98. 
		// Dropped for now by assuming input error.
		drop if w_correct == -98
		
		
		/* -- Check for Outliers ----------- 
		To check for outliers, first, I would calculate the percentage of questions
		answered correctly in each section, as well as for the entire exam. 
		Then, I would examine its distribution by plotting a histogram and 
		look for values far off from the bulk of the observations */ 
		
		
		/* -- Generate Total Section Score Variables ----------- 
		
		For each total score variable, sum the number of questions answered
		correctly. 
		
		New variables: 
		> tot_ws_score (Total word and sentence reading score)
		> tot_a_score (Total arithmetic score)
		> tot_sp_score (Total spelling score)
		> tot_score (Total score (sum of all reading, spelling, and arithmetic scores))*/
		gen tot_r_score = w_correct + s_correct
		gen tot_a_score = 0 
		forval x = 1/8 { // sum all the arithmetic sccore variables by looping
			replace tot_a_score = tot_a_score + a`x'_correct
		}
		rename spelling_correct tot_s_score // for uniformity
		gen tot_t_score = tot_r_score + tot_a_score + tot_s_score
		
		// Label variables for clarity since they are newly generated
		label variable tot_r_score "Total word and sentence reading score"
		label variable tot_a_score "Total arithmetic score"
		label variable tot_s_score "Total spelling score"
		label variable tot_t_score "Total score (sum of all reading, spelling, and arithmetic scores)"
		
		/* -- Generate Letter Score Variable ----------- 
		
		Generate a string variable based on total score with the following rule:
		A: 80–98; B: 60–79; C: 40–59; D: 20–39; F: 0–19. Convert string variable
		into a categorical variable that takes a value of 4 if the pupil received 
		an A, 3 if the pupil received a B, and so on. Label the values with the 
		letter grade.
		
		New variable:
		> grade (Letter grade based on total score expressed in values 0-4) */ 
		 gen grade = .
		 replace grade = 4 if inrange(tot_t_score, 80, 98)
		 replace grade = 3 if inrange(tot_t_score, 60, 79)
		 replace grade = 2 if inrange(tot_t_score, 40, 59)
		 replace grade = 1 if inrange(tot_t_score, 20, 39)
		 replace grade = 0 if inrange(tot_t_score, 0, 19)
		
		// Create and apply letter grade labels for readability
		label define grade_labs 4 "A" 3 "B" 2 "C" 1 "D" 0 "F"
		label values grade grade_labs
		
		// Label newly generated grade variable
		label variable grade "Letter grade based on total score expressed in values 0-4"
		
		
		/* -- Standardize Pupil Scores ----------- 
		
		Standardize pupil scores as follows:
		> z-score of total score relative to the control group
		> z-score of score from each subsection relative to control group 
		
		New variables:
		> t_score_z (z-score of total score relative to the control group)
		> r_score_z (z-score of reading score relative to the control group)
		> a_score_z (z-score of arithmetic score relative to the control group)
		> s_score_z (z-score of spelling score relative to the control group)
		*/
		
		// Loop over all the subsection and total score variables, use 'summarize'
		// to obtain mean and sd of control group, then construct z-score
		foreach v in a r s t {
			quietly sum tot_`v'_score if tracking==0
			gen `v'_score_z = (tot_`v'_score-r(mean))/r(sd)
		}
		
		// Label newly generated variables
		label variable t_score_z "z-score of total score relative to the control group"
		label variable r_score_z "z-score of reading score relative to the control group"
		label variable a_score_z "z-score of arithmetic score relative to the control group"
		label variable s_score_z "z-score of spelling score relative to the control group"
		
		
		/* -- Save Cleaned Student Test Data ----------- 
		Save according to filename assigned at the top of this file */ 
		save $cleaned_student_test_data, replace 
		
		
		* PREPARE TEACHER DATA *************************************************
		
		/* -- Load Teacher Data ----------- */
		use $teacher_data, clear
		
		
		/* -- Draw Random Sample of Teachers ----------- 
		
		Draw an unstratified random sample of 40% of teachers and save as a
		separate csv file. Seed used is specified at top of this do.file */
		sample 40
		sort schoolid teacherid // sort by schoolid and teacherid for tidiness
		
		
		/* -- Save Randomly Sampled Teacher Data ----------- 
		Save according to filename assigned at the top of this file */ 
		export delimited $rand_sample_teachers_data, replace
		
		
		/* -- Reload Teacher Data ----------- */
		use $teacher_data, clear
		
		
		/* -- Compute Average Years of Experience by School ----------- 
		
		Get average years of teacher experience by school and code as missing
		for the entire school if any of their teacher's years of teaching is 
		unknown 
		
		New Variables:
		> yrstaught_avg (Average Years of Teacher Experience) 
		> missing_any (1 or more yrstaught values are missing for this school */
		
		// Generate average years of experience by schoolid
		egen yrstaught_avg = mean(yrstaught), by(schoolid) 
		
		/* Create indicator for if any yrstaught values are missing, 1 for missing
		and 0 otherwise. Apply the largest value to each school such that schools
		with 1 or more missing values will be assigned missing_any = 1 and 
		missing_any = 0 otherwise. Replace average years of experience with missing 
		values if missing_any == 1 */ 
		bysort schoolid: egen missing_any = max(yrstaught==.)
		replace yrstaught_avg = . if missing_any==1
		
		
		/* -- Collapse Data by School ----------- 
		
		Collapse data by school to create dataset where schools are uniquely
		identified, with average years of experience of its teachers attached 
		
		New Variable:
		> yrstaught_avg (Average Years of Teacher Experience) */ 
		
		// For each school, take only the first 'yrstaught_avg' value, since 
		// yrstaught_avg is the same within each school, one can also use take
		// the last value, or the mean, or even the median
		collapse (first) yrstaught_avg, by(schoolid) 
		
		// Label newly generated variable
		label variable yrstaught_avg "Average Years of Teacher Experience (. if any is missing)"
		
		
		/* -- Merge with Cleaned Student Test Data ----------- 
		
		Merge with cleaned student data using schoolid as join variable without
		creating a merge status variable since all student obs are matched.
		Employ one-to-many merge since school is unique and each school is 
		matched to multiple students. */
		merge 1:m schoolid using $cleaned_student_test_data, nogen
		
		// Reorder to put at end of varlist to keep first vars as 
		// identifying columns
		order yrstaught_avg, after(s_score_z) 
		
		/* -- Label Variables For Clarity and Results Output Later ----------- 
		
		Label variables that will be used in output tables or graphs later for
		clarity and tidiness sake */
		label variable tracking "Tracking"
		label variable girl "Girl"
		label variable yrstaught_avg "Years of Teacher Experience" //relabel for brevity
		
		
		/* -- Save Cleaned and Merged Student Teacher Data ----------- 
		Save according to filename assigned at the top of this file */ 
		save $student_teacher_merged_data, replace
		
		
		************************************************************************
		******************************* ANALYSIS *******************************
		************************************************************************
		
		
		/* -- Regression 1  ----------- 
		
		Regressing Standardized Total Score on Tracking Dummy
		+ Zone Fixed Effects
		+ School Clustered Standard Errors 
		
		Choice of Standard Errors Justification: 
		
		Use standard errors clustered by zone because errors are most likely 
		correlated with unobserved determinants on the zone level, such as 
		zone population, general wealth and level of education. 
		Shouldn't cluster by schoolid because treatment is assigned on the 
		school level and we want errors to be calculated using both treated 
		and control observations. 
		
		Save results to 'r1' for output later using esttab. */
		eststo r1: reghdfe t_score_z tracking, absorb(zone) cluster(zone)
		estadd local zone "Y" // Add indicator for using zone fixed effects 
		
		
		/* -- Regressions 2-4  ----------- 
		
		Regressing Standardized Subsection Score on Tracking Dummy
		+ Zone Fixed Effects
		+ School Clustered Standard Errors 
		
		Save results to 'r2', 'r3' and 'r4' for output later using esttab. */
		eststo r2: reghdfe r_score_z tracking, absorb(zone) cluster(zone)
		estadd local zone "Y"
		eststo r3: reghdfe a_score_z tracking, absorb(zone) cluster(zone)
		estadd local zone "Y"
		eststo r4: reghdfe s_score_z tracking, absorb(zone) cluster(zone)
		estadd local zone "Y"
				
				
		/* -- Regression 5  ----------- 
		
		Regressing Standardized Subsection Score on Tracking Dummy
		+ Controls for Pupil Gender and Average Years of Teacher Experience
		+ Zone Fixed Effects
		+ School Clustered Standard Errors 
		
		Save results to 'r5' for output later using esttab. */
		eststo r5: reghdfe t_score_z tracking girl yrstaught_avg, ///
		absorb(zone) cluster(zone)
		estadd local zone "Y"
		estadd local controls "Y" // Add indicator for using additional controls
		
		
		/* -- Output Regression Results ----------- 
		
		Output to latex table with the following formatting:
		> Round standard errors to 3 significant figures
		> Display variable as labels
		> Reduce horizontal spacing
		> Attach * when p-value < 0.1, ** when p < 0.05, *** when p < 0.001
		> With number-labeled columns only
		> Indicators to clarify undisplayed regressors
		> Without table notes on the bottom (appended later using threeparttables) 
		
		// Export table to path specified at top of this do.file */		
		esttab r1 r5 using $reg_results_dest, ///
		se(%4.3f) compress label ///
		star(* 0.1 ** 0.05 *** 0.001) ///
		nomtitles ///
		scalars("zone Zone FE" "controls Controls") ///
		nonotes ///
		replace
		
		// Output additional table with all results for completness
		esttab r1 r5 r2 r3 r4 using "$dir/all_regression_results.tex", ///
		se(%4.3f) compress label ///
		star(* 0.1 ** 0.05 *** 0.001) ///
		mtitles("Total" "Total" "Reading" "Arithmetic" "Spelling") ///
		scalars("zone Zone FE" "controls Controls") ///
		nonotes ///
		replace
		
		
		/* -- Plot Bar Graph of Arithmetic Scores by District  ----------- 
		
		Calculate average arithmetic section score for each treatment group, 
		by district, then plot bar graph comparing the four values, slightly
		overlaid for aesthetics */
		egen tot_a_score_treated = mean(tot_a_score) if tracking==1, by(district)
		egen tot_a_score_control = mean(tot_a_score) if tracking==0, by(district)
		
		// Plot the bar graph with formatting specified below
		graph bar (mean) tot_a_score_control tot_a_score_treated, over(district) ///
		bargap(-30) /// Customize gap between bars of the same district to create slight overlaying
		legend(label(1 "Control") label(2 "Treated")) ///
		title("Average Arithmetic Test Scores") ///
		subtitle("by district") ///
		ytitle("Unstandardized Scores") ///
		bar(1, fcolor(gs10) lcolor(gs1) lwidth(medium)) /// Customize bar color of control group
		bar(2, fcolor(navy) lcolor(gs1) lwidth(medium)) // Customize bar color of treated group
		
		// Export graph to path specified at top of this do.file
		graph export $bar_graph_dest, replace
		
		
		/* -- Regression 5-8  ----------- 
		
		Runs regressions to examine correlations among test scores, treatment, 
		girl, and years of teacher experience. Finds that girl but not years
		of teacher experience is correlated with test scores. */
		
		eststo r6: reghdfe t_score_z girl, ///
		absorb(zone) cluster(zone)
		estadd local zone "Y"
		
		eststo r7: reghdfe t_score_z tracking girl, ///
		absorb(zone) cluster(zone)
		estadd local zone "Y"
		
		eststo r8: reghdfe t_score_z yrstaught_avg, ///
		absorb(zone) cluster(zone)
		estadd local zone "Y"
		
		// Output additional table with all results for completness
		esttab r1 r6 r7 r8 r5 using "$dir/additional_regression_results.tex", ///
		se(%4.3f) compress label ///
		nomtitles ///
		star(* 0.1 ** 0.05 *** 0.001) ///
		scalars("zone Zone FE") ///
		nonotes ///
		replace
		
		exit
