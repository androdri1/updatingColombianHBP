clear all
set matsize 799
set seed 42

*parallel setclusters 3


if "`c(username)'"== "paul.rod" {
	cd "/dataeco01/economia/usuarios/paul.rodriguez/ActualizacionPBS/"	
}
else {
	cd "C:\Users\paul.rodriguez\Dropbox\Universidad del Rosario\Cross\IETS\Actualizacion PBS"
	*cd "D:\Paul.Rodriguez\Dropbox (Personal)\Universidad del Rosario\Cross\IETS\Actualizacion PBS"
}

glo graphsopt = " scheme(plotplainblind)  plotregion(lcolor(black) lwidth(thin) lpattern(solid) fcolor(white)) graphregion(fcolor(white)) "


////////////////////////////////////////////////////////////////////////////////
// Regresiones

use "codigo/datos/actualizacionPBS.dta", clear

cap log close
log using "results/logs/resultados.smcl" , replace

keep if nonzeros==8 // Solo datos completos

*
*Control Sintético .............................................................
preserve // Para 2019 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
	bys idtecnologia year: egen totY = sum(gastopermill)
	gen uso2012x = totY>0 if year==2012
	bys idtecnologia: egen uso2012 = max(uso2012x)
	keep if uso2012==1
	keep if year>=2012
		
	// Y sólo para las que hay información de IHH y share institucional (medicamentos)

	gen miIHH=missing(both_IHH_atc5)
	bys idtecnologia : egen nMI_IHH=total(miIHH)

	gen miShare=missing(share_insti_atc5)
	bys idtecnologia : egen nMI_share=total(miShare)

	keep if (nMI_IHH==0 & nMI_IHH==0) | tipotecnologia==1 // Medicamentos con historial O procedimientos
	xtdescribe	

	tsset idtecnologia year

	// Paso 2. Del total general de controles, hacer un pre-matching ...........
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial both_IHH_atc5 share_insti_atc5 {
	    gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
		gen L3`varo'=L3.`varo'
	}

	gen potencial=0

	// Procedimientos		
		psmatch2 G2019 	L1frecuenciapc L2frecuenciapc L3frecuenciapc ///
						L1gastopermill L2gastopermill L3gastopermill ///
						L1punicasmill  L2punicasmill L3punicasmill ///
						L1logpunicasmill L2logpunicasmill L3logpunicasmill ///
						L1logfrecuencia L2logfrecuencia L3logfrecuencia ///
						L1loggastoper L2loggastoper L3loggastoper ///		
						L1alejadayespecial L2alejadayespecial L3alejadayespecial ///		
			if year==2019 & (pbsAlguna2019==1 | pbsAlguna==0) & tipotecnologia==1 , n(5) logit
		forval i=1(1)5 {
			levelsof _n`i' , local(x`i')
		}
		foreach ido in `x1' `x2' `x3' `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
	
	// Medicamentos
		psmatch2 G2019 	L1frecuenciapc L2frecuenciapc L3frecuenciapc ///
						L1gastopermill L2gastopermill L3gastopermill ///
						L1punicasmill L2punicasmill L3punicasmill ///
						L1logpunicasmill L2logpunicasmill L3logpunicasmill ///
						L1logfrecuencia L2logfrecuencia L3logfrecuencia ///
						L1loggastoper L2loggastoper L3loggastoper ///		
						L1alejadayespecial L2alejadayespecial L3alejadayespecial ///	
						L1both_IHH_atc5 L2both_IHH_atc5 L3both_IHH_atc5 ///
						L1share_insti_atc5 L2share_insti_atc5 L3share_insti_atc5 ///
			if year==2019 & (pbsAlguna2019==1 | pbsAlguna==0) & tipotecnologia==0 , n(5) logit
		forval i=1(1)5 {
			levelsof _n`i' , local(x`i')
		}
		foreach ido in `x1' `x2' `x3' `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
	
		
		
	
	bys idtecnologia: egen potencialX = max(potencial)
		
	keep  if pbsAlguna2019==1 | potencialX==1  // Sólo las Inclusions del 2019, vs lo no incluido ever
	
	// Paso 3. Ahora sí el control sintético  ..................................
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logpunicasmill frecuenciapc(2012(1)2018) gastopermill(2012(1)2018) punicasmill(2012(1)2018) , d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2019") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1) tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logfrecuencia frecuenciapc(2012(1)2018) gastopermill(2012(1)2018) punicasmill(2012(1)2018) , d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2019") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2) tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner loggastoper frecuenciapc(2012(1)2018) gastopermill(2012(1)2018) punicasmill(2012(1)2018) , d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2019") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner alejadayespecial frecuenciapc(2012(1)2018) gastopermill(2012(1)2018) punicasmill(2012(1)2018) , d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2019") sc_name("Not in the HBP") tc_ytitle("Scattered areas") tc_gname(a4) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)	
restore
grc1leg a1 a2 a3 a4
graph export "results/figures/Synth2019.pdf", as(pdf) name("Graph") replace


preserve // Para 2018 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
	bys idtecnologia year: egen totY = sum(gastopermill)
	gen uso2012x = totY>0 if year==2012
	bys idtecnologia: egen uso2012 = max(uso2012x)
	keep if uso2012==1
	keep if year>=2012

	// Y sólo para las que hay información de IHH y share institucional (medicamentos)

	gen miIHH=missing(both_IHH_atc5)
	bys idtecnologia : egen nMI_IHH=total(miIHH)

	gen miShare=missing(share_insti_atc5)
	bys idtecnologia : egen nMI_share=total(miShare)

	keep if (nMI_IHH==0 & nMI_IHH==0) | tipotecnologia==1 // Medicamentos con historial O procedimientos
	xtdescribe	

	tsset idtecnologia year

	// Paso 2. Del total general de controles, hacer un pre-matching ...........
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial both_IHH_atc5 share_insti_atc5 {
	    gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
		gen L3`varo'=L3.`varo'
	}

	gen potencial=0

	
	// Procedimientos (no hay procedimientos)

	
	// Medicamentos
		psmatch2 G2018 	L1frecuenciapc L2frecuenciapc L3frecuenciapc ///
						L1gastopermill L2gastopermill L3gastopermill ///
						L1punicasmill L2punicasmill L3punicasmill ///
						L1logpunicasmill L2logpunicasmill L3logpunicasmill ///
						L1logfrecuencia L2logfrecuencia L3logfrecuencia ///
						L1loggastoper L2loggastoper L3loggastoper ///		
						L1alejadayespecial L2alejadayespecial L3alejadayespecial ///	
						L1both_IHH_atc5 L2both_IHH_atc5 L3both_IHH_atc5 ///
						L1share_insti_atc5 L2share_insti_atc5 L3share_insti_atc5 ///
			if year==2018 & (pbsAlguna2018==1 | pbsAlguna==0) & tipotecnologia==0 , n(5) logit
		forval i=1(1)5 {
			levelsof _n`i' , local(x`i')
		}
		foreach ido in `x1' `x2' `x3' `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
	
	
	bys idtecnologia: egen potencialX = max(potencial)
		
	keep  if pbsAlguna2018==1 | potencialX==1  // Sólo las Inclusions del 2018, vs lo no incluido ever
	
	// Paso 3. Ahora sí el control sintético  ..................................
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logpunicasmill frecuenciapc(2012(1)2017) gastopermill(2012(1)2017) punicasmill(2012(1)2017), d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2018") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1) tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logfrecuencia frecuenciapc(2012(1)2017) gastopermill(2012(1)2017) punicasmill(2012(1)2017), d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2018") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2) tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner loggastoper frecuenciapc(2012(1)2017) gastopermill(2012(1)2017) punicasmill(2012(1)2017), d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2018") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner alejadayespecial frecuenciapc(2012(1)2017) gastopermill(2012(1)2017) punicasmill(2012(1)2017), d(pbsfull) 
	effect_graphs, treated_name("Inclusions 2018") sc_name("Not in the HBP") tc_ytitle("Scattered areas") tc_gname(a4) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)	
restore
grc1leg a1 a2 a3 a4
graph export "results/figures/Synth2018.pdf", as(pdf) name("Graph") replace

preserve // Para 2017 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	drop if idtec=="B03AC02" //Generaba error con esta unidad (no entiendo bien por qué)

	// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
	bys idtecnologia year: egen totY = sum(gastopermill)
	gen uso2012x = totY>0 if year==2012
	bys idtecnologia: egen uso2012 = max(uso2012x)
	keep if uso2012==1
	keep if year>=2012

	// Y sólo para las que hay información de IHH y share institucional (medicamentos)

	gen miIHH=missing(both_IHH_atc5)
	bys idtecnologia : egen nMI_IHH=total(miIHH)

	gen miShare=missing(share_insti_atc5)
	bys idtecnologia : egen nMI_share=total(miShare)

	keep if (nMI_IHH==0 & nMI_IHH==0) | tipotecnologia==1 // Medicamentos con historial O procedimientos
	xtdescribe	

	tsset idtecnologia year

	// Paso 2. Del total general de controles, hacer un pre-matching ...........
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial both_IHH_atc5 share_insti_atc5 {
	    gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
		gen L3`varo'=L3.`varo'
	}

	gen potencial=0
	
	
	* Solo hay 3 tecnologias potenciales a incluir este año, asi que no hay que hacer el pre-matching
	replace potencial=1 if year==2017 & (pbsAlguna==0 & L1alejadayespecial==0) & ( abs(L1gastopermill - .1613874)<0.3 ) & tipotecnologia==0  // Medicamentos
	replace potencial=1 if year==2017 & pbsAlguna==0 & ///
							(L1gastopermill<4 ) & ///
							( (L1punicasmill>0.05 & L1punicasmill<0.08) | (L1punicasmill >360 &  L1punicasmill<364) ) ///
							& tipotecnologia==1  // Procedimientos
	
	bys idtecnologia: egen potencialX = max(potencial)
		
	keep  if pbsAlguna2017==1 | potencialX==1  // Sólo las Inclusions del 2016, vs lo no incluido ever
	
	// Paso 3. Ahora sí el control sintético  ..................................
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logpunicasmill frecuenciapc(2012(1)2016) gastopermill(2012(1)2016) punicasmill(2012(1)2016) tipotecnologia, d(pbsfull) gen_vars 
	effect_graphs, treated_name("Inclusions 2017") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1) tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logfrecuencia frecuenciapc(2012(1)2016) gastopermill(2012(1)2016) punicasmill(2012(1)2016) tipotecnologia, d(pbsfull)  gen_vars
	effect_graphs, treated_name("Inclusions 2017") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2) tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner loggastoper frecuenciapc(2012(1)2016) gastopermill(2012(1)2016) punicasmill(2012(1)2016) tipotecnologia, d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2017") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)
	 
	*B03AC02
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner alejadayespecial frecuenciapc(2012(1)2016) gastopermill(2012(1)2016) punicasmill(2012(1)2016) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2017") sc_name("Not in the HBP") tc_ytitle("Scattered areas") tc_gname(a4) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)	
restore
grc1leg a1 a2 a3 a4
graph export "results/figures/Synth2017.pdf", as(pdf) name("Graph") replace

preserve // Para 2016 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	drop if idtec=="922805" // Gasto pc sufre con este procedimiento
	drop if idtec=="042201"
	drop idtec

	// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
	bys idtecnologia year: egen totY = sum(gastopermill)
	gen uso2012x = totY>0 if year==2012
	bys idtecnologia: egen uso2012 = max(uso2012x)
	keep if uso2012==1
	keep if year>=2012

	// Y sólo para las que hay información de IHH y share institucional (medicamentos)

	gen miIHH=missing(both_IHH_atc5)
	bys idtecnologia : egen nMI_IHH=total(miIHH)

	gen miShare=missing(share_insti_atc5)
	bys idtecnologia : egen nMI_share=total(miShare)

	keep if (nMI_IHH==0 & nMI_IHH==0) | tipotecnologia==1 // Medicamentos con historial O procedimientos
	xtdescribe	

	tsset idtecnologia year

	// Paso 2. Del total general de controles, hacer un pre-matching ...........
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial both_IHH_atc5 share_insti_atc5 {
	    gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
		gen L3`varo'=L3.`varo'
	}

	gen potencial=0

	// Procedimientos	
		psmatch2 G2016 	L1frecuenciapc L2frecuenciapc L3frecuenciapc ///
						L1gastopermill L2gastopermill L3gastopermill ///
						L1punicasmill L2punicasmill L3punicasmill ///
						L1logpunicasmill L2logpunicasmill L3logpunicasmill ///
						L1logfrecuencia L2logfrecuencia L3logfrecuencia ///
						L1loggastoper L2loggastoper L3loggastoper ///		
						L1alejadayespecial L2alejadayespecial L3alejadayespecial ///		
			if year==2016 & (pbsAlguna2016==1 | pbsAlguna==0) & tipotecnologia==1 , n(5) logit
		forval i=1(1)5 {
			levelsof _n`i' , local(x`i')
		}
		foreach ido in `x1' `x2' `x3' `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
	
	// Medicamentos
		psmatch2 G2016 	L1frecuenciapc L2frecuenciapc L3frecuenciapc ///
						L1gastopermill L2gastopermill L3gastopermill ///
						L1punicasmill L2punicasmill L3punicasmill ///
						L1logpunicasmill L2logpunicasmill L3logpunicasmill ///
						L1logfrecuencia L2logfrecuencia L3logfrecuencia ///
						L1loggastoper L2loggastoper L3loggastoper ///		
						L1alejadayespecial L2alejadayespecial L3alejadayespecial ///	
						L1both_IHH_atc5 L2both_IHH_atc5 L3both_IHH_atc5 ///
						L1share_insti_atc5 L2share_insti_atc5 L3share_insti_atc5 ///
			if year==2016 & (pbsAlguna2016==1 | pbsAlguna==0) & tipotecnologia==0 , n(5) logit
		forval i=1(1)5 {
			levelsof _n`i' , local(x`i')
		}
		foreach ido in `x1' `x2' `x3' `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
		
	bys idtecnologia: egen potencialX = max(potencial)
		
	keep  if pbsAlguna2016==1 | potencialX==1  // Sólo las Inclusions del 2017, vs lo no incluido ever
	
	// Paso 3. Ahora sí el control sintético  ..................................
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logpunicasmill frecuenciapc(2012(1)2015) gastopermill(2012(1)2015) punicasmill(2012(1)2015) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2016") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1) tc_options($graphsopt)
		
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logfrecuencia frecuenciapc(2012(1)2015) gastopermill(2012(1)2015) punicasmill(2012(1)2015) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2016") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2) tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner loggastoper frecuenciapc(2012(1)2015) gastopermill(2012(1)2015) punicasmill(2012(1)2015) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2016") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)

	cap drop lead effect pre_rmspe post_rmspe
	synth_runner alejadayespecial frecuenciapc(2012(1)2015) gastopermill(2012(1)2015) punicasmill(2012(1)2015) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2016") sc_name("Not in the HBP") tc_ytitle("Scattered areas") tc_gname(a4) tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)	
restore
grc1leg a1 a2 a3 a4
graph export "results/figures/Synth2016.pdf", as(pdf) name("Graph") replace

preserve // Para 2014  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*.... Aquí hay que usar sólo hasta dos rezagos

	// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
	bys idtecnologia year: egen totY = sum(gastopermill)
	gen uso2012x = totY>0 if year==2012
	bys idtecnologia: egen uso2012 = max(uso2012x)
	keep if uso2012==1
	keep if year>=2012

	// Y sólo para las que hay información de IHH y share institucional (medicamentos)

	gen miIHH=missing(both_IHH_atc5)
	bys idtecnologia : egen nMI_IHH=total(miIHH)

	gen miShare=missing(share_insti_atc5)
	bys idtecnologia : egen nMI_share=total(miShare)

	keep if (nMI_IHH==0 & nMI_IHH==0) | tipotecnologia==1 // Medicamentos con historial O procedimientos
	xtdescribe	

	tsset idtecnologia year

	// Paso 2. Del total general de controles, hacer un pre-matching ...........
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial both_IHH_atc5 share_insti_atc5 {
	    gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
	}

	gen potencial=0

	// Procedimientos	
		psmatch2 G2014 	L1frecuenciapc L2frecuenciapc  ///
						L1gastopermill L2gastopermill  ///
						L1punicasmill L2punicasmill  ///
						L1logpunicasmill L2logpunicasmill  ///
						L1logfrecuencia L2logfrecuencia  ///
						L1loggastoper L2loggastoper  ///		
						L1alejadayespecial L2alejadayespecial  ///		
			if year==2014 & (pbsAlguna2014==1 | pbsAlguna==0) & tipotecnologia==1 , n(5) logit
		forval i=1(1)5 {
			levelsof _n`i' , local(x`i')
		}
		foreach ido in `x1' `x2' `x3' `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
	
	// Medicamentos
		psmatch2 G2014 	L1frecuenciapc L2frecuenciapc  ///
						L1gastopermill L2gastopermill  ///
						L1punicasmill L2punicasmill  ///
						L1logpunicasmill L2logpunicasmill  ///
						L1logfrecuencia L2logfrecuencia  ///
						L1loggastoper L2loggastoper  ///		
						L1alejadayespecial L2alejadayespecial  ///	
						L1both_IHH_atc5 L2both_IHH_atc5  ///
						L1share_insti_atc5 L2share_insti_atc5  ///
			if year==2014 & (pbsAlguna2014==1 | pbsAlguna==0) & tipotecnologia==0 , n(5) logit
		forval i=1(1)5 {
			levelsof _n`i' , local(x`i')
		}
		foreach ido in `x1' `x2' `x3' `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
		
	bys idtecnologia: egen potencialX = max(potencial)
		
	keep  if pbsAlguna2014==1 | potencialX==1  // Sólo las Inclusions del 2014, vs lo no incluido ever
	
	// Paso 3. Ahora sí el control sintético  ..................................
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logpunicasmill frecuenciapc(2012(1)2013) gastopermill(2012(1)2013) punicasmill(2012(1)2013) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2014") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1)  tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner logfrecuencia frecuenciapc(2012(1)2013) gastopermill(2012(1)2013) punicasmill(2012(1)2013) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2014") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2)  tc_options($graphsopt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner loggastoper frecuenciapc(2012(1)2013) gastopermill(2012(1)2013) punicasmill(2012(1)2013) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2014") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3)  tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)
	
	cap drop lead effect pre_rmspe post_rmspe
	synth_runner alejadayespecial frecuenciapc(2012(1)2013) gastopermill(2012(1)2013) punicasmill(2012(1)2013) , d(pbsfull) gen_vars
	effect_graphs, treated_name("Inclusions 2014") sc_name("Not in the HBP") tc_ytitle("Scattered areas") tc_gname(a4)  tc_options($graphsopt)
	pval_graphs , pvals_gname(a2pval) pvals_std_gname(a2pvalt)	
restore
grc1leg a1 a2 a3 a4
graph export "results/figures/Synth2014.pdf", as(pdf) name("Graph") replace


log close

*/
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Y la versión final, agrupada

cap log close
log using "results/logs/resultadosCS.smcl" , replace

use "codigo/datos/actualizacionPBS.dta", clear
keep if nonzeros==8 // Solo datos completos

// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
bys idtecnologia year: egen totY = sum(gastopermill)
bys idtecnologia year: egen totY2 = sum(frecuenciapc)
bys idtecnologia year: egen totY3 = sum(punicasmill)
gen uso2012x = totY>0 if year==2012
gen uso2012x2= totY2>0 if year==2012
gen uso2012x3= totY3>0 if year==2012
bys idtecnologia: egen uso2012 = max(uso2012x)
bys idtecnologia: egen uso20122= max(uso2012x2)
bys idtecnologia: egen uso20123= max(uso2012x3)
keep if uso2012==1 & uso20122==1 & uso20123==1
keep if year>=2012

drop if pbsAlguna2019==1 // Do not include this one
	// Y sólo para las que hay información de IHH y share institucional (medicamentos)

	gen miIHH=missing(both_IHH_atc5)
	bys idtecnologia : egen nMI_IHH=total(miIHH)

	gen miShare=missing(share_insti_atc5)
	bys idtecnologia : egen nMI_share=total(miShare)

	keep if (nMI_IHH==0 & nMI_IHH==0) | tipotecnologia==1 // Medicamentos con historial O procedimientos
	xtdescribe	


tsset idtecnologia year

// Paso 2. Del total general de controles, hacer un pre-matching ...........
loc conda="1==0 "
foreach cohort in 2014 2016 2017 2018 {
	disp in red "`cohort'"
	cap drop potencial L1* L2* Fu2*
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial both_IHH_atc5 share_insti_atc5 {
		gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
	}
	gen Fu2 = F.inclusionpbs==1 | F2.inclusionpbs==1
		
	gen potencial=0
	
	
	if `cohort'!=2017 & `cohort'!=2018 {
		// Procedimientos	
			psmatch2 G`cohort' 	L1frecuenciapc 		L2frecuenciapc  ///
								L1gastopermill 		L2gastopermill  ///
								L1punicasmill 		L2punicasmill  ///
								L1logpunicasmill 	L2logpunicasmill  ///
								L1logfrecuencia 	L2logfrecuencia  ///
								L1loggastoper 	L2loggastoper  ///		
								L1alejadayespecial 	L2alejadayespecial  ///		
				if year==`cohort' & (pbsAlguna`cohort'==1 | Fu2==0) & tipotecnologia==1 , n(5) logit
			forval i=1(1)5 {
				levelsof _n`i' , local(x`i')
			}
			foreach ido in `x1' `x2' `x3' `x4' `x5' {
			   qui replace potencial=1 if _id==`ido'
			}
		
		// Medicamentos
			psmatch2 G`cohort' 	L1frecuenciapc 		L2frecuenciapc  ///
								L1gastopermill 		L2gastopermill  ///
								L1punicasmill 		L2punicasmill  ///
								L1logpunicasmill 	L2logpunicasmill  ///
								L1logfrecuencia 	L2logfrecuencia  ///
								L1loggastoper 	L2loggastoper  ///		
								L1alejadayespecial 	L2alejadayespecial  ///	
								L1both_IHH_atc5 	L2both_IHH_atc5  ///
								L1share_insti_atc5 	L2share_insti_atc5  ///
				if year==`cohort' & (pbsAlguna`cohort'==1 | Fu2==0) & tipotecnologia==0 , n(5) logit
			forval i=1(1)5 {
				levelsof _n`i' , local(x`i')
			}
			foreach ido in `x1' `x2' `x3' `x4' `x5' {
			   qui replace potencial=1 if _id==`ido'
			}	
	}
	else if `cohort'==2018 { // solo hay medicamentos
		// Medicamentos	
			disp in red "`cohort'"
		
			psmatch2 G`cohort' 	L1frecuenciapc 		L2frecuenciapc  ///
								L1gastopermill 		L2gastopermill  ///
								L1punicasmill 		L2punicasmill  ///
								L1logpunicasmill 	L2logpunicasmill  ///
								L1logfrecuencia 	L2logfrecuencia  ///
								L1loggastoper 	L2loggastoper  ///		
								L1alejadayespecial 	L2alejadayespecial  ///	
								L1both_IHH_atc5 	L2both_IHH_atc5  ///
								L1share_insti_atc5 	L2share_insti_atc5  ///
				if year==`cohort' & (pbsAlguna`cohort'==1 | Fu2==0) & tipotecnologia==0 , n(5) logit
			forval i=1(1)5 {
				levelsof _n`i' , local(x`i')
			}
			foreach ido in `x1' `x2' `x3' `x4' `x5' {
			   qui replace potencial=1 if _id==`ido'
			}			
	}
	else {
	    
		replace potencial=1 if year==2017 & (Fu2==0 & L1alejadayespecial==0) & ( abs(L1gastopermill - .1613874)<0.3 ) & tipotecnologia==0  // Medicamentos
		replace potencial=1 if year==2017 & Fu2==0 & ///
								(L1gastopermill<4 ) & ///
								( (L1punicasmill>0.05 & L1punicasmill<0.08) | (L1punicasmill >360 &  L1punicasmill<364) ) ///
								& tipotecnologia==1  // Procedimientos		
	}	
	
	
	bys idtecnologia: egen potencialX`cohort' = max(potencial)
	loc conda="`conda'| pbsAlguna`cohort'==1 | potencialX`cohort'==1"
}	

keep  if `conda'  // Sólo las Inclusions y sus controles emparejados
save "results/sample_MAIN.dta", replace
cap log close


// Paso 3. Ahora sí el control sintético  ..................................
cap log close
log using "results/logs/resultadosCSx.smcl" , append
use "results/sample_MAIN.dta", clear
* -----------------------------------------------------------------------------
*decode idtecnologia , gen(idtec)
drop if idtec=="930820"
drop if idtec=="B05ZA98"

cap program drop my_pred
program my_pred, rclass
	args tyear
	return local predictors "logpunicasmill(`=`tyear'-2'(1)`=`tyear'-1') logfrecuencia(`=`tyear'-2'(1)`=`tyear'-1') loggastoper(`=`tyear'-2'(1)`=`tyear'-1')  frecuenciapc(`=`tyear'-2'(1)`=`tyear'-1') gastopermill(`=`tyear'-2'(1)`=`tyear'-1') punicasmill(`=`tyear'-2'(1)`=`tyear'-1')"
end

cap program drop my_drop_units
program my_drop_units
	args tunit
*	if `tunit'==39 qui drop if inlist(state,21,38)
end

cap program drop my_xperiod
program my_xperiod, rclass
	args tyear
	return local xperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

cap program drop my_mspeperiod
program my_mspeperiod, rclass
	args tyear
	return local mspeperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end


cap drop lead effect pre_rmspe post_rmspe
synth_runner logpunicasmill, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

cap drop lead effect pre_rmspe post_rmspe
synth_runner logfrecuencia, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2)  tc_options($graphsopt) 

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

cap drop lead effect pre_rmspe post_rmspe
synth_runner loggastoper, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.
								
cap drop lead effect pre_rmspe post_rmspe
synth_runner alejadayespecial, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Used in scattered areas") tc_gname(a4)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.



grc1leg a1 a2 a3 a4
graph save "Graph" "results/figures/SynthOverall.gph" , replace
graph export "results/figures/SynthOverall.pdf", as(pdf) name("Graph") replace

log close



////////////////////////////////////////////////////////////////////////////////
**#  Solo servicios
////////////////////////////////////////////////////////////////////////////////
use "codigo/datos/actualizacionPBS.dta", clear
keep if nonzeros==8 // Solo datos completos


cap log close
log using "results/logs/resultados_proc.smcl" , replace

keep if tipotecnologia==1 // Procedimientos solamente



// Paso 1. DSÃ³lo las que tienen algÃºn uso en 2012 ..........................
bys idtecnologia year: egen totY = sum(gastopermill)
bys idtecnologia year: egen totY2 = sum(frecuenciapc)
gen uso2012x = totY>0 if year==2012
gen uso2012x2= totY2>0 if year==2012
bys idtecnologia: egen uso2012 = max(uso2012x)
bys idtecnologia: egen uso20122= max(uso2012x2)
keep if uso2012==1 & uso20122==1
keep if year>=2012
*keep if year<2019 // Este es el grupo de control, asÃ­ que no puede "convertirse"
drop if pbsAlguna2019==1 // Do not include this one

tsset idtecnologia year

// Paso 2. Del total general de controles, hacer un pre-matching ...........
loc conda="1==0 "
foreach cohort in 2014 2016   {
    disp in red `cohort'
	cap drop potencial L1* L2* Fu2*
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial {
		gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
	}
	gen Fu2 = F.inclusionpbs==1 | F2.inclusionpbs==1
	
	if `cohort'!=2017 & `cohort'!=2018 {
	    
		
		
		psmatch2 G`cohort'  L1frecuenciapc L2frecuenciapc ///
						L1gastopermill L2gastopermill ///
						L1punicasmill L2punicasmill ///
						L1logpunicasmill L2logpunicasmill  ///
						L1logfrecuencia L2logfrecuencia  ///
						L1loggastoper L2loggastoper  ///		
						L1alejadayespecial L2alejadayespecial /// //						tipotecnologia ///
		if year==`cohort' & (pbsAlguna`cohort'==1 | Fu2==0) , n(3) logit		

		

		forval i=1(1)3 {
			levelsof _n`i' , local(x`i')
		}
		gen potencial=0
		foreach ido in `x1' `x2' `x3' { // `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
		
	}
	else {
		gen potencial=0
		if `cohort'==2017  replace potencial=1 if year==`cohort' & (Fu2==0 & L1alejadayespecial==0) 
		if `cohort'==2018  replace potencial=1 if year==`cohort' & (Fu2==0 & L1alejadayespecial==0 & ///
																			L1frecuenciapc<0.01 & L2frecuenciapc<0.01 & ///
																			L1punicasmill<0.01    & L2punicasmill<0.01    & ///
																			L1gastopermill<1.1    & L2gastopermill<1.1  & L2gastopermill>=0.8   ) 
		
	}
	
	bys idtecnologia: egen potencialX`cohort' = max(potencial)
	loc conda="`conda'| pbsAlguna`cohort'==1 | potencialX`cohort'==1"			

}	

replace pbsfull=0 if year==2017 | year==2018  | year==2019

keep  if `conda'  // SÃ³lo las Inclusions y sus controles emparejados
save "results/sample_proc.dta", replace


// Paso 3. Ahora sÃ­ el control sintÃ©tico  ..................................

* -----------------------------------------------------------------------------
cap program drop my_pred
program my_pred, rclass
	args tyear
	return local predictors "frecuenciapc(`=`tyear'-2'(1)`=`tyear'-1') gastopermill(`=`tyear'-2'(1)`=`tyear'-1') punicasmill(`=`tyear'-2'(1)`=`tyear'-1')	logpunicasmill(`=`tyear'-2'(1)`=`tyear'-1') logfrecuencia(`=`tyear'-2'(1)`=`tyear'-1') loggastoper(`=`tyear'-2'(1)`=`tyear'-1')"
end

cap program drop my_drop_units
program my_drop_units
	args tunit
*	if `tunit'==39 qui drop if inlist(state,21,38)
*	if `tunit'==3 qui drop if state==21
end

cap program drop my_xperiod
program my_xperiod, rclass
	args tyear
	return local xperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

cap program drop my_mspeperiod
program my_mspeperiod, rclass
	args tyear
	return local mspeperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

synth_runner logpunicasmill, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.


synth_runner logfrecuencia, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2)  tc_options($graphsopt) 

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

	
synth_runner loggastoper, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.
								
						
synth_runner alejadayespecial, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Used in scattered areas") tc_gname(a4)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.



grc1leg a1 a2 a3 a4
graph export "results/figures/Synth_Proc.pdf", as(pdf) name("Graph") replace

log close




////////////////////////////////////////////////////////////////////////////////
**#  Solo medicamentos (sin ajustes adicionales)
////////////////////////////////////////////////////////////////////////////////

use "codigo/datos/actualizacionPBS.dta", clear
keep if nonzeros==8 // Solo datos completos

cap log close
log using "results/logs/resultados_medi.smcl" , replace

keep if tipotecnologia==0 // Medicamentos solamente



// Paso 1. DSÃ³lo las que tienen algÃºn uso en 2012 ..........................
bys idtecnologia year: egen totY = sum(gastopermill)
bys idtecnologia year: egen totY2 = sum(frecuenciapc)
gen uso2012x = totY>0 if year==2012
gen uso2012x2= totY2>0 if year==2012
bys idtecnologia: egen uso2012 = max(uso2012x)
bys idtecnologia: egen uso20122= max(uso2012x2)
keep if uso2012==1 & uso20122==1
keep if year>=2012
*keep if year<2019 // Este es el grupo de control, asÃ­ que no puede "convertirse"
drop if pbsAlguna2019==1 // Do not include this one

tsset idtecnologia year

// Paso 2. Del total general de controles, hacer un pre-matching ...........
loc conda="1==0 "
foreach cohort in 2014 2016 2017 2018 {
	cap drop potencial L1* L2* Fu2*
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial {
		gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
	}
	gen Fu2 = F.inclusionpbs==1 | F2.inclusionpbs==1
	
	if `cohort'!=2017 {
		psmatch2 G`cohort'  L1frecuenciapc L2frecuenciapc ///
						L1gastopermill L2gastopermill ///
						L1punicasmill L2punicasmill ///
						L1logpunicasmill L2logpunicasmill  ///
						L1logfrecuencia L2logfrecuencia  ///
						L1loggastoper L2loggastoper  ///		
						L1alejadayespecial L2alejadayespecial /// //						tipotecnologia ///
		if year==`cohort' & (pbsAlguna`cohort'==1 | Fu2==0) , n(3) logit		
				

		forval i=1(1)3 {
			levelsof _n`i' , local(x`i')
		}
		gen potencial=0
		foreach ido in `x1' `x2' `x3' { // `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
		
	}
	else {
		gen potencial=0
		replace potencial=1 if year==`cohort' & (Fu2==0 & L1alejadayespecial==0) 
		
	}
	
	bys idtecnologia: egen potencialX`cohort' = max(potencial)
	loc conda="`conda'| pbsAlguna`cohort'==1 | potencialX`cohort'==1"			

}	
replace pbsfull=0 if year==2017 | year==2018  | year==2019
keep  if `conda'  // SÃ³lo las Inclusions y sus controles emparejados
save "results/sample_medi.dta", replace

// Paso 3. Ahora sÃ­ el control sintÃ©tico  ..................................

* -----------------------------------------------------------------------------
cap program drop my_pred
program my_pred, rclass
	args tyear
	return local predictors "frecuenciapc(`=`tyear'-2'(1)`=`tyear'-1') gastopermill(`=`tyear'-2'(1)`=`tyear'-1') punicasmill(`=`tyear'-2'(1)`=`tyear'-1')	logpunicasmill(`=`tyear'-2'(1)`=`tyear'-1') logfrecuencia(`=`tyear'-2'(1)`=`tyear'-1') loggastoper(`=`tyear'-2'(1)`=`tyear'-1')"
end

cap program drop my_drop_units
program my_drop_units
	args tunit
*	if `tunit'==39 qui drop if inlist(state,21,38)
*	if `tunit'==3 qui drop if state==21
end

cap program drop my_xperiod
program my_xperiod, rclass
	args tyear
	return local xperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

cap program drop my_mspeperiod
program my_mspeperiod, rclass
	args tyear
	return local mspeperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

synth_runner logpunicasmill, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.


synth_runner logfrecuencia, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2)  tc_options($graphsopt) 

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

	
synth_runner loggastoper, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.
								
						
synth_runner alejadayespecial, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Used in scattered areas") tc_gname(a4)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.



grc1leg a1 a2 a3 a4
graph export "results/figures/Synth_medi.pdf", as(pdf) name("Graph") replace

log close



////////////////////////////////////////////////////////////////////////////////
**# Con HTA solamente
////////////////////////////////////////////////////////////////////////////////
use "codigo/datos/actualizacionPBS.dta", clear
keep if nonzeros==8 // Solo datos completos

cap log close
log using "results/logs/resultados_HTA.smcl" , replace

drop if ietsAlguna==0 & pbsAlguna==1 // Take out those without a HTA which were included into the HBP
*drop if Giets // Un missing sobra por ahí

// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
bys idtecnologia year: egen totY = sum(gastopermill)
bys idtecnologia year: egen totY2 = sum(frecuenciapc)
gen uso2012x = totY>0 if year==2012
gen uso2012x2= totY2>0 if year==2012
bys idtecnologia: egen uso2012 = max(uso2012x)
bys idtecnologia: egen uso20122= max(uso2012x2)
keep if uso2012==1 & uso20122==1
keep if year>=2012
*keep if year<2019 // Este es el grupo de control, así que no puede "convertirse"
drop if pbsAlguna2019==1 // Do not include this one

tsset idtecnologia year

// Paso 2. Del total general de controles, hacer un pre-matching ...........
loc conda="1==0 "
foreach cohort in 2014 2016 2018 { // 2017 ihas not IETS revised technologies
	disp in red "`cohort'"
	cap drop potencial L1* L2* Fu2*
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial {
		gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
	}
	gen Fu2 = F.inclusionpbs==1 | F2.inclusionpbs==1
	
	
	psmatch2 G`cohort'  L1frecuenciapc L2frecuenciapc ///
						L1gastopermill L2gastopermill ///
						L1punicasmill L2punicasmill ///
						L1logpunicasmill L2logpunicasmill  ///
						L1logfrecuencia L2logfrecuencia  ///
						L1loggastoper L2loggastoper  ///		
						L1alejadayespecial L2alejadayespecial /// //						tipotecnologia ///
		if year==`cohort' & (  (pbsAlguna`cohort'==1  ) | Fu2==0) , n(3) logit  
	forval i=1(1)3 {
		levelsof _n`i' , local(x`i')
	}
	gen potencial=0
	foreach ido in `x1' `x2' `x3' { // `x4' `x5' {
	   qui replace potencial=1 if _id==`ido'
	}
	bys idtecnologia: egen potencialX`cohort' = max(potencial)
	loc conda="`conda'| pbsAlguna`cohort'==1 | potencialX`cohort'==1"
}	

keep  if `conda'  // Sólo las Inclusions y sus controles emparejados
save "results/sample_HTA.dta", replace

// Paso 3. Ahora sí el control sintético  ..................................

* -----------------------------------------------------------------------------
cap program drop my_pred
program my_pred, rclass
	args tyear
	return local predictors "frecuenciapc(`=`tyear'-2'(1)`=`tyear'-1') gastopermill(`=`tyear'-2'(1)`=`tyear'-1') punicasmill(`=`tyear'-2'(1)`=`tyear'-1')	logpunicasmill(`=`tyear'-2'(1)`=`tyear'-1') logfrecuencia(`=`tyear'-2'(1)`=`tyear'-1') loggastoper(`=`tyear'-2'(1)`=`tyear'-1')"
end

cap program drop my_drop_units
program my_drop_units
	args tunit
*	if `tunit'==39 qui drop if inlist(state,21,38)
*	if `tunit'==3 qui drop if state==21
end

cap program drop my_xperiod
program my_xperiod, rclass
	args tyear
	return local xperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

cap program drop my_mspeperiod
program my_mspeperiod, rclass
	args tyear
	return local mspeperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

synth_runner logpunicasmill, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.


synth_runner logfrecuencia, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2)  tc_options($graphsopt) 

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

	
synth_runner loggastoper, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.
								
						
synth_runner alejadayespecial, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Used in scattered areas") tc_gname(a4)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.



grc1leg a1 a2 a3 a4
graph export "results/figures/Synth_HTA.pdf", as(pdf) name("Graph") replace

log close




////////////////////////////////////////////////////////////////////////////////
// Medicamentos con share_insti_atc5 y both_IHH_atc5
////////////////////////////////////////////////////////////////////////////////
use "codigo/datos/actualizacionPBS.dta", clear
keep if nonzeros==8 // Solo datos completos

cap log close
log using "results/logs/resultados_medi2.smcl" , replace



// Paso 1. DSólo las que tienen algún uso en 2012 ..........................
bys idtecnologia year: egen totY = sum(gastopermill)
bys idtecnologia year: egen totY2 = sum(frecuenciapc)
gen uso2012x = totY>0 if year==2012
gen uso2012x2= totY2>0 if year==2012
bys idtecnologia: egen uso2012 = max(uso2012x)
bys idtecnologia: egen uso20122= max(uso2012x2)
keep if uso2012==1 & uso20122==1
keep if year>=2012
*keep if year<2019 // Este es el grupo de control, así que no puede "convertirse"
drop if pbsAlguna2019==1 // Do not include this one

// Y sólo para las que hay información de IHH y share institucional

gen miIHH=missing(both_IHH_atc5)
bys idtecnologia : egen nMI_IHH=total(miIHH)

gen miShare=missing(share_insti_atc5)
bys idtecnologia : egen nMI_share=total(miShare)

keep if nMI_IHH==0 & nMI_IHH==0 // Medicamentos solamente
xtdescribe


tsset idtecnologia year

// Paso 2. Del total general de controles, hacer un pre-matching ...........
loc conda="1==0 "
foreach cohort in 2014 2016 2017 2018 {
	cap drop potencial L1* L2* Fu2*
	foreach varo in frecuenciapc gastopermill punicasmill logpunicasmill logfrecuencia loggastoper alejadayespecial both_IHH_atc5 share_insti_atc5 {
		gen L1`varo'=L.`varo'
		gen L2`varo'=L2.`varo'
	}
	gen Fu2 = F.inclusionpbs==1 | F2.inclusionpbs==1
	
	if `cohort'!=2017 {
		psmatch2 G`cohort'  L1frecuenciapc L2frecuenciapc ///
						L1gastopermill L2gastopermill ///
						L1punicasmill L2punicasmill ///
						L1logpunicasmill L2logpunicasmill  ///
						L1logfrecuencia L2logfrecuencia  ///
						L1loggastoper L2loggastoper  ///		
						L1alejadayespecial L2alejadayespecial ///
						L1both_IHH_atc5 L2both_IHH_atc5 ///
						L1share_insti_atc5 L2share_insti_atc5 ///
		if year==`cohort' & (pbsAlguna`cohort'==1 | Fu2==0) , n(3) logit		
				

		forval i=1(1)3 {
			levelsof _n`i' , local(x`i')
		}
		gen potencial=0
		foreach ido in `x1' `x2' `x3' { // `x4' `x5' {
		   qui replace potencial=1 if _id==`ido'
		}
		
	}
	else {
		gen potencial=0
		replace potencial=1 if year==`cohort' & (Fu2==0 & L1alejadayespecial==0) 
		
	}
	
	bys idtecnologia: egen potencialX`cohort' = max(potencial)
	loc conda="`conda'| pbsAlguna`cohort'==1 | potencialX`cohort'==1"			

}	

keep  if `conda'  // Sólo las Inclusions y sus controles emparejados
save "results/sample_medi2.dta", replace


// Paso 3. Ahora sí el control sintético  ..................................

* -----------------------------------------------------------------------------
cap program drop my_pred
program my_pred, rclass
	args tyear
	return local predictors "frecuenciapc(`=`tyear'-2'(1)`=`tyear'-1') gastopermill(`=`tyear'-2'(1)`=`tyear'-1') punicasmill(`=`tyear'-2'(1)`=`tyear'-1')	logpunicasmill(`=`tyear'-2'(1)`=`tyear'-1') logfrecuencia(`=`tyear'-2'(1)`=`tyear'-1') loggastoper(`=`tyear'-2'(1)`=`tyear'-1') both_IHH_atc5(`=`tyear'-2'(1)`=`tyear'-1') share_insti_atc5(`=`tyear'-2'(1)`=`tyear'-1') "
end

cap program drop my_drop_units
program my_drop_units
	args tunit
*	if `tunit'==39 qui drop if inlist(state,21,38)
*	if `tunit'==3 qui drop if state==21
end

cap program drop my_xperiod
program my_xperiod, rclass
	args tyear
	return local xperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

cap program drop my_mspeperiod
program my_mspeperiod, rclass
	args tyear
	return local mspeperiod "`=`tyear'-2'(1)`=`tyear'-1'"
end

synth_runner logpunicasmill, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Unique users") tc_gname(a1)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.


synth_runner logfrecuencia, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Frequency") tc_gname(a2)  tc_options($graphsopt) 

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

	
synth_runner loggastoper, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Expenditure per user") tc_gname(a3)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.
								
						
synth_runner alejadayespecial, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Used in scattered areas") tc_gname(a4)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

synth_runner both_IHH_atc5, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("IHH ATC5") tc_gname(a5)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.

synth_runner share_insti_atc5, d(pbsfull) pred_prog(my_pred)  drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) mspeperiod_prog(my_mspeperiod) 
effect_graphs, treated_name("Inclusions") sc_name("Not in the HBP") tc_ytitle("Share institutional sector") tc_gname(a6)  tc_options($graphsopt)

	disp e(pval_joint_post)		// The proportion of effects from control units that have 
								// post-treatment RMSPE at least as great as the treated unit
	disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
	disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
								// large as the average of the treated units.
								
 		
grc1leg a1 a2 a3 a4, $graphsopt
graph export "results/figures/Synth_medi2_1.pdf", as(pdf) name("Graph") replace

grc1leg a5 a6, $graphsopt
graph export "results/figures/Synth_medi2_2.pdf", as(pdf) name("Graph") replace


log close




