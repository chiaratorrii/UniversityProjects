**********************************************************************
************************* EMPRIRICAL PROJECT *************************
**********************************************************************

clear
set more off

use "ewcs6_2015_ukda_1904.dta"




*************************** Data prep ********************************

* Keep only variables of interest (note: they are labelled)
keep Q88 Q82 Q80a Q80b Q80c Q80d Q81a Q81b Q81c Q2a Q2b Country Q71b Q72a Q72b Q72c Q72d Q72e Q72f Q72g Q73 Q104 Q76



*** Variables generation ***

* Generate a summary variable for "soft" violence
gen vio1=0
replace vio1=1 if Q80a==1 | Q80b==1 | Q80c==1 | Q80d==1
replace vio1=. if Q80a==8 | Q80b==8 | Q80c==8 | Q80d==8
replace vio1=. if Q80a==9 | Q80b==9 | Q80c==9 | Q80d==9

* Generate a summary variable for "hard" violence
gen vio2=0
replace vio2=1 if Q81a==1 | Q81b==1 | Q81c==1
replace vio2=. if Q81a==8 | Q81b==8 | Q81c==8
replace vio2=. if Q81a==9 | Q81b==9 | Q81c==9

* Generate a summary variable for discrimination
gen discrimination=0
replace discrimination=1 if Q72a==1 | Q72b==1 | Q72c==1 | Q72d==1 | Q72e==1 | Q72f==1 | Q72g==1
replace discrimination=. if Q72a==8 | Q72b==8 | Q72c==8 | Q72d==8 | Q72e==8 | Q72f==8 | Q72g==8
replace discrimination=. if Q72a==9 | Q72b==9 | Q72c==9 | Q72d==9 | Q72e==9 | Q72f==9 | Q72g==9



*** Missing values in violence variables ***
* Soft violence
codebook vio1
drop if vio1==.

* Hard violence
codebook vio2
drop if vio2==.


*** Numerations and missing values of controls ***

* Gender
codebook Q2a
tab Q2a, nolab
replace Q2a=0 if Q2a==1
replace Q2a=1 if Q2a==2
drop if Q2a==9
codebook Q2a

* Age
tab Q2b, nolab
drop if Q2b==888 | Q2b==999
tab Q2b, nolab
codebook Q2b

* Helth & safety committee
codebook Q71b
tab Q71b, nolab
replace Q71b=0 if Q71b==2
drop if Q71b==8 | Q71b==9 | Q71b==.
tab Q71b, nolab

* Discrimination
codebook discrimination
drop if discrimination==.

* Health/safety risk perception
codebook Q73
replace Q73=0 if Q73==2
drop if Q73==. | Q73==9 | Q73==8

* Income
codebook Q104
tab Q104, nolab
drop if Q104==88888888 | Q104==99999999

* Helth problem for more than 6 months
codebook Q76
tab Q76, nolab
replace Q76=0 if Q76==2
drop if Q76==8 | Q76==9

* Contry
codebook Country
tab Country



*** Differences in observables between respondents and non respindents to dependent variables' questions ***
codebook Q88
tab Q88
codebook Q82
tab Q82, nolab

gen dn_r_sat=0
replace dn_r_sat=1 if Q88==9 | Q88==8 


table () result, ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest vio1, by(dn_r_sat) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest vio2, by(dn_r_sat) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q2a, by(dn_r_sat) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q2b, by(dn_r_sat)unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q71b, by(dn_r_sat) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest discrimination, by(dn_r_sat) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q73, by(dn_r_sat) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q76, by(dn_r_sat) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q104, by(dn_r_sat) unequal) ///
stars(pv 0.01 " ***" 0.05 " **" 0.1 " *", shownote)


gen dn_r_abs=0
replace dn_r_abs=1 if Q82==999 | Q82==888

table () result, ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest vio1, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest vio2, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q2a, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q2b, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q71b, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest discrimination, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q73, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q76, by(dn_r_abs) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q104, by(dn_r_abs) unequal) ///
stars(pv 0.01 " ***" 0.05 " **" 0.1 " *", shownote)



*** Missing values of dependent variables ***
codebook Q88 Q82

* Evaluate and drop missing values Q82 - Absenteeism
codebook Q82 // to see the values of the refusal and don't know
tab Q82, nolab 
replace Q82=. if Q82==888 | Q82==999
drop if Q82==.
codebook Q82

* Evaluate and drop missing values Q88 - Satisfaction
codebook Q88 // to see the values of the refusal and don't know
tab Q88, nolab
replace Q88=. if Q88==8 | Q88==9
drop if Q88==.
codebook Q88 


*** Changes in the numeration of satisfaction ***

* The numeration of Q88 - Satisfaction was from 1 (very satisfied) to 4 (not at all satisfied). We reverse it to apply the ordered probit.
tab Q88, nolab
replace Q88=5 if Q88==4
replace Q88=6 if Q88==3
replace Q88=7 if Q88==2
replace Q88=8 if Q88==1
tab Q88, nolab
replace Q88=1 if Q88==5
replace Q88=2 if Q88==6
replace Q88=3 if Q88==7
replace Q88=4 if Q88==8
tab Q88, nolab

label drop Q88 
label define Q88 1 "Not at all satisfied" 2 "Not very satisfied" 3 "Satisfied" 4 "Very satisfied"
label values Q88 Q88

tab Q88





************************* Summary statistics *************************
* Summarize
summarize Q88 Q82 vio1 vio2

* Differences in observables between those exposed to violence and others to dependent variables' questions
gen vio=0
replace vio=1 if vio1==1 | vio2==2

table () result, ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q88, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q82, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q2a, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q2b, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q71b, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest discrimination, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q73, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q76, by(vio) unequal) ///
command(r(mu_1) r(mu_2) diff=r(mu_1)-r(mu_2) pv=r(p): ttest Q104, by(vio) unequal) ///
stars(pv 0.01 " ***" 0.05 " **" 0.1 " *", shownote)





**************************** Regressions ****************************

*** Satisfaction ***

** Soft violence (vio1) without controls
oprobit Q88 vio1, r
outreg2 using sat_out.xls, replace ctitle(Soft violence) keep(vio1) addtext(Controls, NO) addstat(Pseudo R2, e(r2_p))

* Average Adjustd Predicitons
quietly mtable, at (vio1=0) rown(not subject to violence) // w/labels
mtable, at (vio1=1) rown(subject to violence) below

* Average Marginal Effcts
mtable, dydx(vio1) 
margins, dydx(vio1) //more details



** Soft violence (vio1) with controls
oprobit Q88 vio1 Q2a Q2b Q73 Q104 discrimination i.Country, r
outreg2 using sat_out.xls, append ctitle(Soft violence) keep(vio1) addtext(Controls, YES) addstat(Pseudo R2, e(r2_p))

* Average Adjestd Predicitons
quietly mtable, at (vio1=0) rown(not subject to violence) // w/labels
mtable, at (vio1=1) rown(subject to violence) below

* Average Marginal Effcts
mtable, dydx(vio1)
margins, dydx(vio1)



** Hard violence (vio2) without controls
oprobit Q88 vio2, r
outreg2 using sat_out.xls, append ctitle(Hard violence) keep(vio2) addtext(Controls, NO) addstat(Pseudo R2, e(r2_p))

* Average Adjestd Predicitons
quietly mtable, at (vio2=0) rown(not subject to violence) // w/labels
mtable, at (vio2=1) rown(subject to violence) below

* Average Marginal Effcts
mtable, dydx(vio2)
margins, dydx(vio2)



** Hard violence (vio2) with controls
oprobit Q88 vio2 Q2a Q2b Q73 Q104 discrimination i.Country, r
outreg2 using sat_out.xls, append ctitle(Hard violence) keep(vio2) addtext(Controls, YES, ) addstat(Pseudo R2, e(r2_p))

* Average Adjestd Predicitons
quietly mtable, at (vio2=0) rown(not subject to violence) // w/labels
mtable, at (vio2=1) rown(subject to violence) below

* Average Marginal Effcts
mtable, dydx(vio2)
margins, dydx(vio2)




*** Absenteeism ***

* Soft violence (vio1) with and without controls
reg Q82 vio1, r
outreg2 using abs_out.xls, replace ctitle(Sof violence) addtext(Country dummies, NO) addstat(Adjusted R2, e(r2_a))

reg Q82 vio1 Q2a Q2b Q71b Q73 discrimination Q76 i.Country, r
outreg2 using abs_out.xls, append ctitle(Soft violence) keep(vio1 Q2a Q2b Q71b Q73 discrimination Q76) addtext(Country dummies, YES) addstat(Adjusted R2, e(r2_a))

* Hard violence (vio2) with and without controls
reg Q82 vio2, r
outreg2 using abs_out.xls, append ctitle(Hard violence) addtext(Country dummies, NO) addstat(Adjusted R2, e(r2_a))

reg Q82 vio2 Q2a Q2b Q71b Q73 discrimination Q76 i.Country, r
outreg2 using abs_out.xls, append ctitle(Hard violence) keep(vio2 Q2a Q2b Q71b Q73 discrimination Q76) addtext(Country dummies, YES) addstat(Adjusted R2, e(r2_a))

* Both soft and hard violence
reg Q82 vio1 vio2 Q2a Q2b Q71b Q73 discrimination Q76 i.Country, r
outreg2 using abs_out.xls, append ctitle(Both) keep(vio1 vio2 Q2a Q2b Q71b Q73 discrimination Q76) addtext(Country dummies, YES) addstat(Adjusted R2, e(r2_a))
