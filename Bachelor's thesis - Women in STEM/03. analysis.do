
*** EMPIRICAL ANALYSIS ***

clear 
set more off

use dataset.dta

** STEP 1

* Dependent: occupational situation
probit occup_status gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6, r

dprobit occup_status gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6, r
outreg2 using "outcomes1.xls", excel replace ctitle(occupational status)

* Dependent: log monthly income
reg log_income gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 if occup_status==1, r
outreg2 using "outcomes1.xls", excel append ctitle(income)

reg income gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 if occup_status==1, r

*Dependent: months between graduation and first job
reg stw_months gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 if occup_status==1, r
outreg2 using "outcomes1.xls", excel append ctitle(months)

* Dependent: overall satisfaction
reg l2_55 gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 if occup_status==1, r
outreg2 using "outcomes1.xls", excel append ctitle(satisfaction)

** STEP 2

* Dependent: re-enrollment
probit l1_25 gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months, r

dprobit l1_25 gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months, r 
outreg2 using "outcomes2.xls", excel replace ctitle(re-enrollment)

* Dependent: reason 1: occupational situation
probit motivo_lavoro gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months if l1_25==0, r

dprobit motivo_lavoro gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months if l1_25==0, r
outreg2 using "outcomes2.xls", excel append ctitle(reason 1: work)

* Dependent: reason 2: uni course
probit motivo_corso gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months if l1_25==0, r

dprobit motivo_corso gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months if l1_25==0, r
outreg2 using "outcomes2.xls", excel append ctitle(reason 2: course)

* Dependent: reason 3: interests
probit motivo_interessi gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months if l1_25==0, r

dprobit motivo_interessi gender_stem gender stem reg1-reg20 citizenship hs_grade1-hs_grade3 int_mobility uni_work1-uni_work3 in_time passing_grade occup_status other_courses training_activities marital_status1-marital_status3 educ_f1-educ_f6 educ_m1-educ_m6 income stw_months if l1_25==0, r
outreg2 using "outcomes2.xls", excel append ctitle(reason 3: interests)
