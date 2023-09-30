
*** DESCRIPTIVE STATISTICS ***

clear
set more off

use dataset.dta

** Graphs

* # STEM vs non STEM, female vs male graduates
graph pie, over(gender) pie(1, color(emidblue)) pie(2, color(navy8)) plabel(_all percent, color(white) size(medlarge)) by(stem)
graph save ngraduates, replace

* STEM vs non STEM, female vs male, avg passing grade
graph hbar (mean) passing_grade, over(gender) over(stem) exclude0 ytitle(Average passing grade)
graph save grade, replace

mean passing_grade, over(stem gender)
display "d_passing_nonSTEM=" 103.8518-101.7013 
display "d_passing_STEM=" 104.6095-102.2302

* STEM vs non STEM, female vs male, re-enrollment
 graph pie, over(l1_25) pie(1, color(emidblue)) pie(2, color(navy8)) plabel(_all percent, color(white) size(medsmall)) by(, title(Would you re-enroll in the same degree program?)) by(gender stem)
graph save re-enroll, replace

* STEM vs non STEM, female vs male, occupational situation
 graph pie, over(occup_status) pie(1, color(emidblue)) pie(2, color(navy8)) plabel(_all percent, color(white) size(medsmall)) by(gender stem)
graph save occupation, replace

* STEM vs non STEM, female vs male, overall satisfaction
graph hbar (mean) l2_55, over(gender) over(stem) exclude0 ytitle(Average overall satisfaction) ylabel(7(0.1)7.3)
graph save satisfaction, replace

mean l2_55, over(stem gender)
mean l2_55, over(stem)
display 7.174192-7.118413 
display 7.191509-7.126629


* STEM vs non STEM, female vs male, monthly income
graph bar (mean) income, over(gender) over(stem) blabel(bar) ytitle(Average monthly income)
graph save income, replace

* STEM vs non STEM, female vs male, months between graduation and first job
graph hbar (mean) stw_months, over(gender) over(stem) exclude0 ytitle(Months between graduation and first job) ylabel(7(1)12)
graph save months, replace

mean stw_months, over(stem gender)
mean stw_months, over(stem)
display 8.158516-7.66555
display 11.18656-10.12178




