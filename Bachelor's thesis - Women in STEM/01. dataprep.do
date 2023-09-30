
*** DATA PREP ***

clear
set more off

import delimited "InsProLau_Microdati_Anno_2015.txt"

* Keep interest variables
keep reg_uni_micro tiplau_micro gruppo_micro classelau_micro sesso citt_mfr voto_diploma_all l1_17 l1_19 l1_20 l1_22 l1_23 l1_25 l1_26 l1_32a l1_32b l1_32c l1_32d l1_32e l1_36a l1_36b l1_36c l1_36d l1_36e l1_36f l1_36g l1_36h l2_1 l2_55 l5_1_mfr l5_4 l5_9 reddito_mfr mesi_i_lavoro

* Chage data type from string to numeric
destring l2_55 reddito_mfr mesi_i_lavoro, replace

* Label variables
label var reg_uni_micro "Regione ateneo"
label var tiplau_micro "Tipo laurea"
label var gruppo_micro "Gruppo laurea"
label var classelau_micro "Classe laurea"
label var sesso "Sesso"
label var citt_mfr "Cittadinanza"
label var voto_diploma_all "Voto diploma"
label var l1_17 "Mobilità internazionale"
label var l1_19 "Tipo di lavoro retribuito durante l'uni"
label var l1_20 "In corso"
label var l1_22 "Voto di laurea"
label var l1_23 "Motivo scelta corso"
label var l1_25 "Si riiscriverebbe?"
label var l1_26 "Motivo no reiscrizione"
label var l1_32a "Isc: master I livello"
label var l1_32b "Isc: master II livello"
label var l1_32c "Isc: l specialistica biennale"
label var l1_32d "Isc: l ciclo unico"
label var l1_32e "Isc: l triennale"
label var l1_36a "A.f.: dottorato"
label var l1_36b "A.f.: specializzazione"
label var l1_36c "A.f.: master"
label var l1_36d "A.f.: borsa di studio/lavoro"
label var l1_36e "A.f.: stage"
label var l1_36f "A.f.: tirocinio"
label var l1_36g "A.f.: corso di formaz"
label var l1_36h "A.f.: altro corso"
label var l2_1 "Situazione lavorativa"
label var l2_55 "Sod complessiva"
label var l5_1_mfr "Stato civile"
label var l5_4 "Titolo studio padre"
label var l5_9 "Titolo studio madre"
label var reddito_mfr "Reddito mensile lav principale"
label var mesi_i_lavoro "Mesi tra laurea e primo lavoro"

* Create dummies

*dummy sesso
replace sesso=0 if sesso==1
replace sesso=1 if sesso==2
label define sessol 0 "Male" 1 "Female"
label val sesso sessol
rename sesso gender

* rename citt_mfr
rename citt_mfr citizenship

* dummies voto diploma
tab voto_diploma_all, gen(hs_grade)

*dummy mobilità internazionale
replace l1_17=0 if l1_17==2
rename l1_17 int_mobility

* dummy lavoro durante l'uni
tab l1_19, gen(uni_work)

* dummy in corso
replace l1_20=0 if l1_20==2
rename l1_20 in_time

*rename passing grade
rename l1_22 passing_grade

* dummy si riiscriverebbe
replace l1_25=0 if l1_25==2
label define l1_25l 0 "No" 1 "Yes"
label val l1_25 l1_25l

* dummies motivi di non reiscirizone
tab l1_26, gen(motivo_non_reisc)
rename motivo_non_reisc2 motivo_lavoro
gen motivo_corso=0
replace motivo_corso=1 if l1_26=="2" | l1_26=="3" | l1_26=="4"
rename motivo_non_reisc6 motivo_interessi

* dummy iscrizione altri corsi
replace l1_32a=1 if l1_32a==1 | l1_32a==2 | l1_32a==3
replace l1_32a=0 if l1_32a==4
replace l1_32b=1 if l1_32b==1 | l1_32b==2 | l1_32b==3
replace l1_32b=0 if l1_32b==4
replace l1_32c=1 if l1_32c==1 | l1_32c==2 | l1_32c==3
replace l1_32c=0 if l1_32c==4
replace l1_32d=1 if l1_32d==1 | l1_32d==2 | l1_32d==3
replace l1_32d=0 if l1_32d==4
replace l1_32e=1 if l1_32e==1 | l1_32e==2 | l1_32e==3
replace l1_32e=0 if l1_32e==4
gen l1_32tot=0
replace l1_32tot=1 if l1_32a==1 | l1_32b==1 | l1_32c==1 | l1_32d==1 | l1_32e==1
label var l1_32tot "Isc. a altri corsi dopo la laurea"
rename l1_32tot other_courses

*dummy attività formative
replace l1_36a=1 if l1_36a==1 | l1_36a==2 | l1_36a==3
replace l1_36a=0 if l1_36a==4
replace l1_36b=1 if l1_36b==1 | l1_36b==2 | l1_36b==3
replace l1_36b=0 if l1_36b==4
replace l1_36c=1 if l1_36c==1 | l1_36c==2 | l1_36c==3
replace l1_36c=0 if l1_36c==4
replace l1_36d=1 if l1_36d==1 | l1_36d==2 | l1_36d==3
replace l1_36d=0 if l1_36d==4
replace l1_36e=1 if l1_36e==1 | l1_36e==2 | l1_36e==3
replace l1_36e=0 if l1_36e==4
replace l1_36f=1 if l1_36f==1 | l1_36f==2 | l1_36f==3
replace l1_36f=0 if l1_36f==4
replace l1_36g=1 if l1_36g==1 | l1_36g==2 | l1_36g==3
replace l1_36g=0 if l1_36g==4
replace l1_36h=1 if l1_36h==1 | l1_36h==2 | l1_36h==3
replace l1_36h=0 if l1_36h==4
gen l1_36tot=0
replace l1_36tot=1 if l1_36a==1 | l1_36b==1 | l1_36c==1 | l1_36d==1 | l1_36e==1 | l1_36f==1 | l1_36g==1 | l1_36h==1
label var l1_36tot "Attività formative dopo la laurea"
rename l1_36tot training_activities

* dummy situazione occupazionale
replace l2_1=0 if l2_1==2
label define l2_1l 0 "Unemployed" 1 "Employed"
label val l2_1 l2_1l
rename l2_1 occup_status

* dummy stato civile
tab l5_1_mfr, gen(marital_status)

* dummies titoli di studio genitori
tab l5_4, gen(educ_f)
tab l5_9, gen(educ_m)

* dummy stem
gen stem=0
replace stem=1 if classelau_micro=="L01" | classelau_micro=="L04" | classelau_micro=="L07  " | classelau_micro=="L08  " | classelau_micro==  "L09" | classelau_micro=="L10  " | classelau_micro=="L12  " | classelau_micro=="L16" | classelau_micro=="L21" | classelau_micro=="L22  " | classelau_micro=="L24  " | classelau_micro=="L25  " | classelau_micro=="L26" | classelau_micro=="L27" | classelau_micro=="L32  " | classelau_micro=="L37  " | classelau_micro=="L41  " | classelau_micro=="S01  " | classelau_micro=="S02  " | classelau_micro=="S03  " | classelau_micro=="S04  " | classelau_micro=="S05  " | classelau_micro=="S06  " | classelau_micro=="S07  " | classelau_micro=="S08  " | classelau_micro=="S09  " | classelau_micro=="S13  " | classelau_micro=="S14  " | classelau_micro=="S15  " | classelau_micro=="S16  " | classelau_micro=="S17  " | classelau_micro=="S18 " | classelau_micro=="S19" | classelau_micro=="S20" | classelau_micro=="S21  " | classelau_micro=="S22  " | classelau_micro=="S23  " | classelau_micro=="S24  " | classelau_micro=="S29 " | classelau_micro=="S56  " | classelau_micro=="U01  " | classelau_micro=="U02  " | classelau_micro=="U05 " | classelau_micro=="U08  " | classelau_micro=="U09  " | classelau_micro=="U10  " | classelau_micro=="U11  " | classelau_micro=="U15  " | classelau_micro=="U16  " | classelau_micro=="U20  " | classelau_micro=="U21  " | classelau_micro=="U23 " | classelau_micro=="U24  " | classelau_micro=="U25  " | classelau_micro=="U26  " | classelau_micro=="U27  " | classelau_micro=="U43  " | classelau_micro=="U54  " | classelau_micro=="U63  " | gruppo_micro==" 1" | gruppo_micro==" 2" | gruppo_micro==" 3 " | gruppo_micro==" 5" | gruppo_micro==" 6"
label define steml 0 "Non-STEM" 1 "STEM"
label val stem steml

gen gender_stem=stem*gender

*Log salari
gen log_income=ln(reddito_mfr)

*rename income e months
rename reddito_mfr income
rename mesi_i_lavoro stw_months

** Summary statistics of variables of interest before and after dropping missing values
sum gender stem occup_status l2_55 income stw_months l1_25

drop if reg_uni_micro=="  "
drop if gruppo_micro=="  "
drop if income==. & occup_status==1
drop if stw_months==. & occup_status==1

sum gender stem occup_status l2_55 income stw_months l1_25

*dummies regioni
tab reg_uni_micro, gen(reg)

save dataset, replace