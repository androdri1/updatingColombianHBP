clear all
set seed 42

*cd "C:\Users\paul.rodriguez\Dropbox\Universidad del Rosario\Cross\IETS\Actualizacion PBS\codigo"
cd "D:\Paul.Rodriguez\Dropbox (Personal)\Universidad del Rosario\Cross\IETS\Actualizacion PBS\codigo"
glo graphsopt = " scheme(plotplainblind)  plotregion(lcolor(black) lwidth(thin) lpattern(solid)) "

////////////////////////////////////////////////////////////////////////////////
// Regresiones

use "datos\actualizacionPBS.dta", clear


********************************************************************************
* Table 1: Raw data comes from here, and is later mixed into a Excel file

gen c1=.
label var c1 "Year"
gen c2=.
label var c2 "Number of procedures"
gen c3=.
label var c3 "Number of medications"


* descriptive 1A ******************************************************************
gen infoGBD= percent_gbd!=. if tipotecnologia==0

cap mat drop output
foreach y in 2012 2013 2014 2015 2016 2017 2018 2019 {
	count if year==`y' & tipotecnologia==0
	loc proced=r(N)	
	count if year==`y' & tipotecnologia==1
	loc medica=r(N)	
	
	qui tabstat ///
		nonzero frecuenciapc punicasmill gastopermill gastoperusd tipotecnologia inclusionpbsyseguimiento alejadayespecial who_esencial infoGBD percent_gbd ///
		ietsRev underRegul uniq years_in_market_Old existe_generico both_IHH_atc4_bi both_IHH_atc5_bi cuantos_atc4_bi cuantos_atc5_bi share_insti_atc4_bi share_insti_atc5_bi  ///
		if year==`y', stat(mean)  save
	mat A = r(StatTotal) 
	matrix rownames A = "`y'"
	matrix output = nullmat(output) \ [`y',`proced',`medica',A]
}

matrix output = output'

outtable using "..\results\tables\desc_year", mat(output) center replace ///
caption("TITLE") nobox f(%12.3fc) label 

* descriptive 1B ******************************************************************

cap mat drop output
foreach y in 2012 2013 2014 2015 2016 2017 2018 2019 {
	count if year==`y' & inclusionpbs==1 & tipotecnologia==0
	loc proced=r(N)	
	count if year==`y' & inclusionpbs==1 & tipotecnologia==1
	loc medica=r(N)	
	if (`proced'+`medica')>0 {
	
		qui tabstat ///
			nonzero frecuenciapc punicasmill gastopermill gastoperusd tipotecnologia inclusionpbsyseguimiento alejadayespecial who_esencial infoGBD percent_gbd ///
			ietsRev underRegul uniq years_in_market_Old existe_generico both_IHH_atc4_bi both_IHH_atc5_bi cuantos_atc4_bi cuantos_atc5_bi share_insti_atc4_bi share_insti_atc5_bi  ///
			if year==`y' & inclusionpbs==1, stat(mean)  save
		mat A = r(StatTotal) 
		matrix rownames A = "`y'"
		matrix output = nullmat(output) \ [`y',`proced',`medica',A]
	
	}
}

matrix output = output'

outtable using "..\results\tables\desc_yearB", mat(output) center replace ///
caption("TITLE") nobox f(%12.3fc) label 


xxxxx

* Probabilidad de inclusión ****************************************************
gen Finclusionpbs=F.inclusionpbs // Al parecer no hay tecnología con una evaluacion
                                 // previa que haya sido incluida
tab Finclusionpbs ietsRev if inclusionpbsyseguimiento==0
tab inclusionpbs ietsRev if L.inclusionpbsyseguimiento==0 // Pero sí 99 evaluaciones al tiempo

replace both_IHH_atc4= both_IHH_atc4/1000
replace both_IHH_atc5= both_IHH_atc5/1000

replace punicasmill = punicasmill/ 1000 // x 100.000
replace gastoperusd = gastoperusd / 1000 // In thousand USD
label var punicasmill "unique users per 100K"
label var gastoperusd "expenditure per user (1000 USD)"

* F.ietsRev is used given how it was constructed

logit F.inclusionpbs c.punicasmill#i.tipotecnologia c.gastoperusd#i.tipotecnologia tipotecnologia F.iets i.year if inclusionpbsyseguimiento==0 , or cluster(idtecnologia)
eststo r1

logit F.inclusionpbs punicasmill gastoperusd F.iets i.year if inclusionpbsyseguimiento==0 & tipotecnologia==1 , or cluster(idtecnologia)
eststo r2

logit F.inclusionpbs punicasmill gastoperusd F.iets i.year  if inclusionpbsyseguimiento==0 & tipotecnologia==0 , or cluster(idtecnologia)
eststo r3

logit F.inclusionpbs punicasmill gastoperusd F.iets who_esencial underRegul both_IHH_atc4_bi cuantos_atc4_bi share_insti_atc4_bi uniq years_in_market_Old existe_generico i.year if inclusionpbsyseguimiento==0 , or cluster(idtecnologia)
eststo r4

logit F.inclusionpbs punicasmill gastoperusd F.iets who_esencial underRegul both_IHH_atc5_bi cuantos_atc5_bi share_insti_atc5_bi uniq years_in_market_Old existe_generico i.year if inclusionpbsyseguimiento==0 , or cluster(idtecnologia)
eststo r5

logit F.inclusionpbs punicasmill gastoperusd F.iets who_esencial underRegul i.year if inclusionpbsyseguimiento==0 & tipotecnologia==0 & year!=2015 , or cluster(idtecnologia)
eststo r6x // solo para mostrar que los resultados no dependen de esto

logit F.inclusionpbs punicasmill gastoperusd F.iets who_esencial underRegul percent_gbd i.year if inclusionpbsyseguimiento==0 & tipotecnologia==0 & year!=2015 , or cluster(idtecnologia)
eststo r7x // No parece que lo del GBD ayude mucho, pero también es que hay poca info; el tener la info no parece funcionar mucho ( infoGBD)

esttab r1 r2 r3 r4 r5 , eform label star(* 0.1 ** 0.05 *** 0.01 ) nogaps se stats(N N_clust r2_p) ///
	drop(F.inclusionpbs:*.year) 
esttab r1 r2 r3 r4 r5 using "..\results\tables\logit.tex", eform label tex star(* 0.1 ** 0.05 *** 0.01 ) ///
	nogaps se stats(N N_clust r2_p)  drop(F.inclusionpbs:*.year) replace

tab ietsAlguna pbsAlguna if year==2017 & pbsSiempre==0 , row
tab underRegul pbsAlguna if year==2017 & pbsSiempre==0 , row

/*
Notas para mejorar el documento a la par que se hace la revisión.
4. Hace falta hacer las regresiones con información que no sea sólo del panel balanceado; es decir, una robustez a la inclusión de tecnologías "nuevas" (hasta un efecto heterogéneo por la antigüedad de la tecnología9
5. Quizás hacer una tabla del impacto "individual" sobre cada tecnología. Esto es, correr el synth sobre cada tecnología por aparte. Esto también puede servir para entender mejor la heterogeneidad.
6. Quizás hace falta explicar por qué las inclusiones previas a 2018 sí incrementaban el uso pero no tanto el gasto, mientras que es lo opuesto en las más recientes. El "estudio de caso" de una tecnología particular sería súper valioso para aterrizar los hallazgos.
*/

tab idtecnologia year if matchMedic==1