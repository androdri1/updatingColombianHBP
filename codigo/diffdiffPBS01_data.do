*version 16
clear all
set seed 42

cd "C:\Users\paul.rodriguez\Dropbox\Universidad del Rosario\Cross\IETS\Actualizacion PBS\codigo"
glo graphsopt = " scheme(plotplainblind)  plotregion(lcolor(black) lwidth(thin) lpattern(solid)) "


import delimited "datos/BaseFinalAnual.csv" , clear

*Descripción de los datos y transformaciones



label define tipotecnologia 0 "Medication" 1 "Procedures"
label val tipotecnologia tipotecnologia
label var tipotecnologia "Technology is a procedure"

gen atc5=codigotecnologia if tipotecnologia==0

rename periodo year
drop v14 v15 v16 v17 v18 v19 v20

encode codigotecnologia, generate(idtecnologia)
drop codigotecnologia // luego se vuelve a crear, primero hay que balancera el panel

xtset idtecnologia year
xtdescribe

*Balanceo del panel
tsfill, full
xtdescribe

replace inclusionpbsyseguimiento=1 if L.inclusionpbsyseguimiento==1
replace inclusionpbsyseguimiento=1 if L.inclusionpbsyseguimiento==1
replace inclusionpbsyseguimiento=1 if L.inclusionpbsyseguimiento==1

gen pbsfull=inclusionpbsyseguimiento
recode pbsfull .=0

decode idtecnologia , gen(idtec) // aqui se volvio a crear

merge m:1 year atc5 using "datos/medicamentos.dta", gen(matchMedic) keep(master match)
replace matchMedic=. if tipotecnologia!=0


foreach varDep in inclusionpbs frecuencia personasunicas valortotal montoportecnologia montoporpersonaunica frecuenciaporpersonaunica alejadayespecial personaspercapita {
	replace `varDep'=0 if `varDep'==.	
}


foreach varDep in valortotal montoportecnologia montoporpersonaunica {
	replace `varDep'=`varDep'/100 if year==2018 | year==2019 // Un error en la importación de los datos
}

foreach varDep in tipotecnologia {
	bys idtecnologia: egen common=max(`varDep')
	replace `varDep'= common // Fixed characteristics
	drop common
}



////////////////////////////////////////////////////////////////////////////////


*se deflactan los gastos con IPC 2019=100
gen montotec=montoportecnologia
replace montotec=montoportecnologia/0.7266 if year==2012
replace montotec=montoportecnologia/0.7484 if year==2013
replace montotec=montoportecnologia/0.7704 if year==2014
replace montotec=montoportecnologia/0.8043 if year==2015
replace montotec=montoportecnologia/0.8701 if year==2016
replace montotec=montoportecnologia/0.9082 if year==2017
replace montotec=montoportecnologia/0.9680 if year==2018

gen montoper=montoporpersonaunica
replace montoper=montoporpersonaunica/0.7266 if year==2012
replace montoper=montoporpersonaunica/0.7484 if year==2013
replace montoper=montoporpersonaunica/0.7704 if year==2014
replace montoper=montoporpersonaunica/0.8043 if year==2015
replace montoper=montoporpersonaunica/0.8701 if year==2016
replace montoper=montoporpersonaunica/0.9082 if year==2017
replace montoper=montoporpersonaunica/0.9680 if year==2018


*Transformación de las variables del estudio
** Variables dependientes
***Frecuencia de uso de la tecnología per user
gen frecuenciapc = frecuenciaporpersonaunica
gen logfrecuencia=log(frecuenciaporpersonaunica)
gen asinhfrecuencia =asinh(frecuenciaporpersonaunica)
label var frecuenciapc	"Frequency per user"
label var logfrecuencia 	"log frequency per user"
label var asinhfrecuencia  	"asinh frequency per user"


sum frecuenciaporpersonaunica
*twoway (scatter frecuenciamill year) , $graphsopt
table year, statistic(mean frecuenciaporpersonaunica) statistic(sd frecuenciaporpersonaunica)

sum logfrecuencia
*twoway (scatter frecuencia year) , $graphsopt
table year, statistic(mean logfrecuencia) statistic(sd logfrecuencia)


if 1==0 {
    cap drop logfreq2
	gen logfreq2 = logfrecuencia
	replace logfreq2 = 1.6 if logfreq2>=1.6
	*replace logfreq2 = -0.1 if frecuenciaporpersonaunica==0
	*twoway (hist logfreq2) , xlabel( -0.1 "Less than 1" 0 "1" 0.18 "1.2" 0.66 "2" 1.04 "2.8" 1.1 "3 +") 
	twoway (hist logfreq2 , percent) , xlabel( 0 "1" 0.18 "1.2" 0.66 "2" 1.04 "2.8" 1.6 "5 +") ///
		xtitle("Frequency per user")	ytitle("Percentage")	
	graph export "..\results\figures\dist_frequency.pdf", as(pdf) name("Graph") replace
}


***usuarios únicos por tecnología

gen     idtecnologiaup = substr(idtec,1,3)   // Capitulo-zona-tipo
replace idtecnologiaup = substr(idtec,1,4) ///
	if (substr(idtec, 1, 1) >= "A" & substr(idtec, 1, 1) <= "Z") ///
	 | (substr(idtec, 1, 1) >= "a" & substr(idtec, 1, 1) <= "z")      // tomamos ATC3 si es un medicamento

	 
gen punicasmill=personaspercapita*1000000

* Al menos un usuario
gen nonzero = punicasmill >0 if punicasmill!=.
label var nonzero "At least one user"
bys idtecnologia: egen nonzeros = total(nonzero)
label var nonzeros "Number of years with at least one user"


cap drop punicasrefx punicasrefB punicasref
bys idtecnologiaup: egen punicasrefx = mean(punicasmill) if year==2012 & punicasmill>0
bys idtecnologiaup: egen punicasrefB  = max(punicasrefx) 
gen punicasref = punicasmill/punicasrefB


drop punicasrefx punicasrefB

gen logpunicasmill=log(punicasmill)
gen asinhpunicasmill=asinh(punicasmill)
*scatter logpunicasmill asinhpunicasmill
label var punicasmill		"Unique users per million"
label var logpunicasmill 	"log unique users per million"
label var asinhpunicasmill 	"asinh unique users per million"
label var punicasref         "Unique users per million relative to 2012 type of technology"


if 1==0 {
    cap drop logfreq2
	gen logfreq2 = logpunicasmill
	replace logfreq2 =-3 if punicasmill==0
	*replace logfreq2 = 1.6 if logfreq2>=1.6

	twoway (hist logpunicasmill , percent) , ///
		xlabel( -3 "0" -0.35 "0.7" 1.58 "4.8" 3.98 "53.6" 6.4 "601.8" 10 "26,635" 13.23 "556,821" ) ///
		xtitle("Unique users per million")	ytitle("Density")	
	graph export "..\results\figures\dist_personasunicas.pdf", as(pdf) name("Graph") replace
}

*** gasto medio por tecnología

gen gastotecmill= montotec/1000000
gen loggastotec=log(montotec)

sum gastotecmill
*twoway (scatter gastotecmill year) , $graphsopt
table year, statistic(mean gastotecmill) statistic(sd gastotecmill)

sum loggastotec
*twoway (scatter loggastotec year) , $graphsopt
table year, statistic(mean loggastotec) statistic(sd loggastotec)

*** Expenditure per user único
cap drop gastoperusd
gen gastopermill= montoper/1000000
gen gastoperusd = montoper /3281
gen loggastoper=log(gastopermill)
gen asinhgastoper=asinh(gastopermill)
*scatter loggastoper asinhgastoper

label var gastopermill "Expenditure per user (million COP)"
label var gastoperusd     "Expenditure per user (USD, 2019)"
label var loggastoper "log expenditure per user"
label var asinhgastoper "asinh expenditure per user"
label var alejadayespecial "Usage in scattered areas"

sum gastopermill
*twoway (scatter gastopermill year) , $graphsopt
table year, statistic(mean gastoperusd) statistic(sd gastoperusd)

sum loggastoper
*twoway (scatter loggastoper year) , $graphsopt
table year, statistic(mean loggastoper) statistic(sd loggastoper)

if 1==0 {
    cap drop logfreq2
	gen logfreq2 = loggastoper
	*replace logfreq2 =-8.1 if montoper==0
	twoway (hist logfreq2 , percent) , /// // -8.1 "0"
		xlabel(-7 "0.001" -4 "0.02" -.693 "0.5" 0 "1" 1.21 "3.3" 2.17 "8.8" 3.34 "28.2" 4.92 "137" 6.07 "432.7" 7.89 "2670"  10 "22,000" 12 "162,754" ) ///
		xtitle("Expenditure per user (USD, 2019)")	ytitle("Percentage")	
	graph export "..\results\figures\dist_gastopermill.pdf", as(pdf) name("Graph") replace
	
	*kdensity loggastoper , xtitle("log Expenditure per unique users (Million COP)")	
}

*collapse (mean) frecuenciamill punicasmill gastopermill , by(year idtecnologia year) // no tiene sentido
xtset idtecnologia year
**Variables dependientes
*** Dicotoma intervencion



foreach y in 2014 2016 2017 2018 2019 {
	gen G`y'= inclusionpbs*(year==`y')
	bys idtecnologia: egen pbsAlguna`y' = max(G`y')
}

gen ietsRev=0 //  IETS variable in an cumulative variable
replace ietsRev=iets if year==2012

forval y=2013(1)2019 {
	replace ietsRev=1 if iets==1 & year==`y'
	replace ietsRev=1 if (L.ietsRev==1 | L.iets==1) & year==`y'
}
bys idtecnologia: egen ietsAlguna = max(iets)
label var ietsAlguna "Ever analysed by IETS"


if 1==0 {
	graph bar (sum) inclusionpbs, over(year) ytitle("Number of technologies added")
	graph export "results\figures\histInclusions.pdf", as(pdf) name("Graph")
}

bys idtecnologia: egen pbsAlguna = max(pbsfull)
gen Gsiempre= inclusionpbsyseguimiento*(year==2012 | year==2013)
bys idtecnologia: egen pbsSiempre = max(Gsiempre)


drop if pbsSiempre==1 // Los "always takers" no pueden hacer parte de este estudio!!! Sólo una tecnologia

tab tipotecnologia if year ==2012 // 9,118 tecnologias. 1,706 medicamentos y 7,412 procedimientos
tab pbsAlguna if year==2012 // De esas, 702 fueron incluidas en algún momento:


// 4. Gráficos DiD, utilizando Inclusions de 2018 y 2019 como control
if 1==0 {

	preserve // Gráfico DiD (caso 2018) ............................................
	// Sólo las que tienen al´gun uso antes de 2018
	bys idtecnologia year: egen totY = sum(frecuenciapc)
	gen uso2017x = totY>0 if year==2017
	bys idtecnologia: egen uso2017 = max(uso2017x)

	tab pbsAlguna2018 if year==2018
	keep if uso2017==1
	tab pbsAlguna2018 if year==2018

	collapse (mean ) punicasmill if pbsAlguna2018==1 | pbsAlguna2019==1 | pbsAlguna==0 , by(year pbsAlguna2018 pbsAlguna)
	tw 	(connected punicasmill year if pbsAlguna2018==1 , msize(medium) msymbol(circle)) ///
		(connected punicasmill year if pbsAlguna==0 ) ///	
		, xline(2017.5) $graphsopt ///
		legend(order(1 "Inclusion in 2018" 2 "Not in the HBP" ) position(6) cols(3) ) xsize(2) ysize(2.5) ytitle("Unique users per million affiliates") xtitle("Year")
	restore // ......
	graph export "..\results\figures\DiD2018.pdf", as(pdf) name("Graph") replace

	preserve // Gráfico DiD (caso 2017) ............................................
	// Sólo las que tienen al´gun uso antes de 2017
	bys idtecnologia year: egen totY = sum(punicasmill)
	gen uso2016x = totY>0 if year==2016
	bys idtecnologia: egen uso2016 = max(uso2016x)

	tab pbsAlguna2017 if year==2017
	keep if uso2016==1
	tab pbsAlguna2017 if year==2017

	collapse (mean ) punicasmill (mean) gastotecmill if pbsAlguna2017==1 | pbsAlguna2018==1 | pbsAlguna2019==1 | pbsAlguna==0 , by(year pbsAlguna2017 pbsAlguna)
	tw 	(connected punicasmill year if pbsAlguna2017==1 , msize(medium) msymbol(circle)) ///
		(connected punicasmill year if pbsAlguna==0  ) ///	
		, xline(2016.5) $graphsopt ///
		legend(order(1 "Inclusion in 2017" 2 "Not in the HBP" ) position(6) cols(3) ) xsize(2) ysize(2.5) ytitle("Unique users per million affiliates") xtitle("Year")
	restore // ......
	graph export "..\results\figures\DiD2017.pdf", as(pdf) name("Graph") replace

	preserve // Gráfico DiD (caso 2016) ............................................
	// Sólo las que tienen al´gun uso antes de 2016
	bys idtecnologia year: egen totY = sum(punicasmill)
	gen uso2015x = totY>0 if year==2015
	bys idtecnologia: egen uso2015 = max(uso2015x)

	tab pbsAlguna2016 if year==2016
	keep if uso2015==1
	tab pbsAlguna2016 if year==2016

	collapse (mean ) punicasmill if pbsAlguna2016==1 | pbsAlguna2018==1 | pbsAlguna2019==1 | pbsAlguna==0 , by(year pbsAlguna2016 pbsAlguna)
	tw 	(connected punicasmill year if pbsAlguna2016==1 , msize(medium) msymbol(circle) ) ///
		(connected punicasmill year if pbsAlguna==0  ) ///		
		 , xline(2015.5) $graphsopt ///
		legend(order(1 "Inclusion in 2016" 2 "Not in the HBP" ) position(6) cols(3) ) xsize(2) ysize(2.5) ytitle("Unique users per million affiliates") xtitle("Year")
	restore // ......
	graph export "..\results\figures\DiD2016.pdf", as(pdf) name("Graph") replace

	preserve // Gráfico DiD (caso 2014) ............................................
	// Sólo las que tienen al´gun uso antes de 2014
	bys idtecnologia year: egen totY = sum(punicasmill)
	gen uso2013x = totY>0 if year==2013
	bys idtecnologia: egen uso2013 = max(uso2013x)

	tab pbsAlguna2014 if year==2014
	keep if uso2013==1
	tab pbsAlguna2014 if year==2014

	collapse (mean ) punicasmill if pbsAlguna2014==1 | pbsAlguna2018==1 |  pbsAlguna2019==1 | pbsAlguna==0 , by(year pbsAlguna2014 pbsAlguna)
	tw 	(connected punicasmill year if pbsAlguna2014==1 , msize(medium) msymbol(circle) ) ///
		(connected punicasmill year if pbsAlguna==0  ) ///		
		 , xline(2013.5) $graphsopt ///
		legend(order(1 "Inclusion in 2014" 2 "Not in the HBP" ) position(6) cols(3) ) xsize(2) ysize(2.5) ytitle("Unique users per million affiliates") xtitle("Year")
	restore // ......
	graph export "..\results\figures\DiD2014.pdf", as(pdf) name("Graph") replace

}


* Exportamos para correr el DiD CS en R, y el synth en Stata ..................
gen     g=.
replace g=2014 if G2014==1
replace g=2016 if G2016==1	
replace g=2017 if G2017==1	
replace g=2018 if G2018==1	
replace g=2019 if G2019==1		

bys idtecnologia: egen G=max(g)
replace G=0 if G==.
drop g

gen     Giets= G
replace Giets=. if ietsAlguna==0 & G!=0 // We take out of the "treatment" group 
										// those included technologies which 
										// were not assesed by IETS
										
										

bys tipotecnologia:  tab nonzeros G if year==2012 // Al apéndice									
										
gen uniq=cuantos_atc5==1
label var uniq "Only one firm per ATC5"
label var ietsRev "HTA in previous years"
label var years_in_market_Old "Years from registry oldest product"
label var inclusionpbsyseguimiento "Included in the HBP"			
label var who_esencial "WHO essential medicine "							

drop if pbsSiempre==1 // Sin los always takers
save "datos/actualizacionPBS.dta", replace
// Correr ahora "diffdiffPBS V4 DiD CS.r"

////////////////////////////////////////////////////////////////////////////////
use "datos/actualizacionPBS.dta", clear


