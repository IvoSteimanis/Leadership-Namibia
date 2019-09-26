*--------------------------------------------------
* Procedural fairness and nepotism among local traditional and democratic leaders in rural Namibia
* Short title: Local leaders in Namibia
* Code to reproduce the figures and analysis in the main manuscript
* Authors: Björn Vollan, Esther Blanco, Sebastian Prediger, Fabian Petutschnig & Ivo Steimanis
*--------------------------------------------------


*--------------------------------------------------
* Program Setup
*--------------------------------------------------
*version XX             // Set Version number for backward compatibility, we used Stata 14
set more off            
clear all               
set linesize 80         
macro drop _all        
set scheme plotplain /*default is a black and white scheme, feel free to change*/
set matsize 2000
* -------------------------------------------------

*--------------------------------------------------
* Directory
*--------------------------------------------------

global workpath "D:\Google Drive\Promotion\2_Paper Drafts\WP3 - Shadow of the Future\OUTPUT\pooled"
global datapath "D:\Google Drive\Promotion\2_Paper Drafts\WP3 - Shadow of the Future\STATA\DTA-FILES"
global output "D:\Google Drive\Promotion\2_Paper Drafts\WP3 - Shadow of the Future\STATA\OUTPUT\ncc"
global xls "D:\Google Drive\Promotion\2_Paper Drafts\WP3 - Shadow of the Future\STATA\XLS-FILES"
* --------------------------------------------------

*--------------------------------------------------
* Description
*--------------------------------------------------
/*
In this study, we test the common conception that democratically elected leaders behave in the interest of their constituents to a greater extent than traditional chiefs do. Our sample consists of 64 (real-life) village leaders and 384 villagers in rural Namibia, where democratically elected leaders and traditional chiefs co-exist within the same villages. We analyze two main attributes of local political leaders: their procedural fairness preferences and their preferential treatment of relatives (nepotism). In addition, we measure personality traits, employing an incentivized social preference task and standardized surveys on local governance practices and villagers’ perceptions of their leaders’ performance. Our results contradict the theoretical priors, as they indicate that traditional chiefs are as likely to implement fair, democratic decision-making procedures (chiefs 68% and DELs 58% on average), and they are as likely not to behave nepotistic (25% of chiefs and 40% of DELs). Moreover, elected leaders and chiefs express similar fundamental social preferences and personality traits. These findings are in line with villagers’ perceptions of most leaders in our sample being popular and fair, albeit villagers’ responses reveal a substantial discrepancy between planned and de-facto implementation of democratic institutions at the local level in Namibia.

*/
*--------------------------------------------------

// Open data file for analyis
use leadership.dta





********************************
*** Procedural Fairness Task ***
********************************
preserve
drop if id > 64 /*only keep leader data for now*/
*one sided tests: Average democratic choices over all 4 decisions, DEL "outpeforms" TL
reg dem7 DEL, vce(robust)
test _b[DEL]=0
local sign_DEL = sign(_b[DEL])
display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_DEL'*sqrt(r(F)))
*p=0.87, we cannot reject the hypothesis that DEL are less democratic (its not even close)
display "Ho: coef >= 0  p-value = " 1-ttail(r(df_r),`sign_DEL'*sqrt(r(F)))
*p=0.13, we can also not reject the one-sided hypothesis that chiefs are more democratic (but its close)

tab dem2 DEL, chi2 exact
ttest dem2, by(DEL)




// Figure 1: Likelihood of procedural choices
*Panel A: democratic vs. autocratic
forvalues i=1/4 {
	reg dem`i' DEL, vce(robust)
	est store c_`i'
	}

coefplot (c_1, aseq(Decision 1, n=64) ///
		\ c_2, aseq(Decision 2, n=64) ///
		\ c_3, aseq(Decision 3, n=64) ///
		\ c_4, aseq(Decision 4, n=64)), levels(95 90) ciopts(lwidth(0.5 1.5) lcolor(*.6 *.9) recast(rcap))  xline(0, lpattern(dash) lcolor(gs3)) ///
		drop(_cons) yla(,labsize(medium)) xla(-1(0.25)1) swapnames ///
		msymbol(d) msize(large) xsize(4.488189) ///
		mlabel format(%9.2f) mlabposition(11) mlabgap(*2)  ///
		legend(order(1 "95% CI" 2 "90% CI") pos(3) ring(0) size(medium) bmargin(tiny)) ///
		title("{bf:Panel A:} democratic vs. autocratic", size(large))  xtitle("Estimated difference in means (DELs - chiefs)", size(large))
graph save fig2_a, replace
 
*Panel B: pseudo-democratic vs. dictator
forvalues i=1/4 {
	reg pseudo`i' DEL, vce(robust)
	est store ols`i'
	}

coefplot (ols1, aseq(Decision 1, n=23) ///
		\ ols2, aseq(Decision 2, n=25) ///
		\ ols3, aseq(Decision 3, n=20) ///
		\ ols4, aseq(Decision 4, n=27)), levels(95 90) ciopts(lwidth(0.5 1.5) lcolor(*.6 *.9) recast(rcap))  xline(0, lpattern(dash) lcolor(gs3)) ///
		drop(_cons) yla(,labsize(medium)) xla(-1(0.25)1) swapnames ///
		msymbol(d) msize(large) xsize(4.488189) ///
		mlabel format(%9.2f) mlabposition(11) mlabgap(*2)  ///
		legend(order(1 "95% CI" 2 "90% CI") pos(3) ring(0) size(medium) bmargin(tiny)) ///
		title("{bf:Panel B:} pseudo-democratic vs. dictator", size(large))  xtitle("Estimated difference in means (DELs - chiefs)", size(large))
graph save fig2_b, replace

graph combine fig2_a.gph fig2_b.gph, scale(*1.2) col(1)
graph export "$output\figure1.emf", replace /*file format of graphs can be changed to other popular formats, e.g. .tif, .jpg or .svg*/




*********************
*** Nepotism Task ***
*********************

*tests
tab nepotistic DEL, chi2 exact
ttest nepotistic, by(DEL)

tab nepotisNR DEL, chi2 exact
ranksum nepotisNR, by(DEL)
tab vengefulRN DEL, chi2 exact
ranksum vengefulRN, by(DEL)

*OLS for CIs
reg nepotisNR DEL, vce(robust)
reg vengefulRN DEL, vce(robust)




// Figure 2: Share of nepotistic leaders.
cibar nepotisNR,  over1(leader)  bargap(20)  barlabel(on) blsize(large) blfmt(%9.2f) blpos(11) /*
	*/ graphopts(legend(size(large) pos(12) rows(1) ring(0)) xsize(1.712598) yla(0(0.1)0.5, labsize(medium)) title("{bf:Panel A:} Leniency", size(large)) /*
	*/ ytitle("frequency",size(large))) ciopts(lpattern(dash)) 
graph save fig5_a, replace

cibar vengefulRN,  over1(leader)  bargap(20)  barlabel(on) blsize(large) blfmt(%9.2f) blpos(11) /*
	*/ graphopts(legend(size(large) pos(12) rows(1) ring(0)) xsize(1.712598) yla(0(0.1)0.5, labsize(medium)) title("{bf:Panel B:} Vengefulness", size(large)) /*
	*/ ytitle("",size(large))) ciopts(lpattern(dash)) 
graph save fig5_b, replace
graph combine fig5_a.gph fig5_b.gph, scale(*1.1)  col(2)
graph export "$output\figure2.emf", replace

restore




// Figure 3: Social preferences and personality traits of leaders and villagers
preserve
tab social_type2, gen(st_)
*Panel A: Social preferences
reshape long st_, i(id) j(st_id)
rename st_ social_types
label define st2 1 "egalitarian" 2 "generous" 3 "spiteful" 4 "ambiguous"
label values st_id st2

cibar social_types,  over1(id1) over2(st_id) barlabel(on) bargap(10) blsize(medium) blpos(11) /*
	*/ graphopts(legend(pos(12) size(large) ring(0) rows(1)) xsize(4.488189) yla(0(0.2)1,labsize(medium)) xlabel(,labsize(medium)) /*
	*/ title("{bf:Panel A:} Social preferences", size(large))  ytitle("mean",size(large))) /*
	*/ ciopts(lpattern(dash))
	graph save fig6_a, replace
restore

* Panel B: Personality traits
preserve
rename extraversion_normal bigfive1
rename agreeableness_normal bigfive2
rename conscientiousness_normal bigfive3
rename neuroticism_normal bigfive4
rename openness_normal bigfive5

reshape long bigfive, i(id) j(big5_id)
label define big5 1 "extraversion" 2 "agreeableness" 3 "conscientiousness" 4 "neuroticism" 5 "openness"
label values big5_id big5


cibar bigfive,  over1(leader) over2(big5_id) barlabel(on) bargap(10) blsize(medium) blpos(11) /*
	*/ graphopts(legend(pos(12) size(large) ring(0) rows(1)) xsize(4.488189) yla(0(0.2)1, labsize(medium)) xlabel(,labsize(medium)) /*
	*/ title("{bf:Panel B:} Personality traits", size(large))  ytitle("mean",size(large)))  /*
	*/  ciopts(lpattern(dash))
	graph save fig6_b, replace
restore

graph combine fig6_a.gph fig6_b.gph,scale(*1.1)col(1)
graph export "$output\figure3.emf",  replace




// Figure 4: Villagers' perception of leader performance
// Package MOREMATE & COEFPLOT from SSC are needed for this graph
*Relative satisfaction with leader performance
gen rel_performance = e_2 - e_1
sum rel_performance
tab e_2 e_1, chi2 exact
* villagers are slightly more satisfied with chiefs, as expressed by the negative relative performance value

*setup matrix
local vars e_1 e_2 e_3 e_4 e_5 e_6 e_7 e_8 /*villager perception items*/ 
local lblname e_ 
local levels 1 2 3 4 5
local nvars: list sizeof vars
local nlevels: list sizeof levels

matrix p = J(`nlevels', `nvars', .)
matrix colnames p = `vars'
matrix rownames p = `levels'

local i 0
foreach v of local vars {
	local ++i
	quietly proportion `v'
	matrix p[1,`i'] = e(b)'*100
	}
	
matrix r=p
mata: st_replacematrix("r", mm_colrunsum(st_matrix("p")))
mata: st_matrix("l", (J(1,`nvars',0) \ st_matrix("r")[1::`nlevels'-1,]))
matrix m = r
mata: st_replacematrix("m", (st_matrix("l") :+ st_matrix("r"))/2)

local plots
local i 0
foreach l of local levels {
	local ++i
	local lbl: lab `lblname' `l'
	local plots `plots' (matrix(m[`i']), ci((l[`i'] r[`i'])) aux(p[`i']) key(ci) label(`lbl'))
}

*plot the data
coefplot `plots', title("")nooffset ms(i) mlabel(@aux) mlabsize(small) mlabcolor(white) mlabpos(0) format(%9.0f) coeflabels(, nobreak labsize(medium))  xsize(7.007874) ciopts(recast(rbar) barwidth(0.6) color(*.8)) legend(rows(1) pos(5) size (medium) span stack order(1 "strongly disagree" 3 "disagree a little" 5 "neutral" 7 "agree a little" 9 "strongly agree"))
graph export "$output\figure4.emf",  replace




// Figure 5: Monetary consequences of leader's anti-social behavior
*tests
ranksum m_fg, by(TL)
sdtest m_fg, by(TL)

cibar m_fg, over1(leader) /*
	*/ barlabel(on) bargap(40) blpos(11) blsize(large) blfmt(%9.2f)/*
	*/ graphopts(legend(pos(12) ring(0) rows(1) size(large))  /*
	*/ title("{bf:Panel A:} Mean differences", size(vlarge)) xsize(2.244094)/*
	*/ yla(0(5)20) ytitle("Mean", size(large)) xtitle("Earnings eliminated in N$", size(large))) /*
	*/ ciopts(lpattern(dash))
graph save fig7_a, replace
	
ranksum m_fg, by(leader)
ttest m_fg, by(leader)

twoway hist m_fg if TL==0, start(0) width(2) freq bcolor(gs0) ///
	|| hist m_fg if TL==1, freq start(0) width(2) barw(1.2) bcolor(gs7) ///
	legend(order(1 "DELs" 2 "Chiefs" ) pos(12) size(large) ring(0) rows(1)) ///
	xla(0(5)30) xsize(2.244094) ytitle("Frequency", size(large)) xtitle("Earnings eliminated in N$", size(large)) title("{bf:Panel B:} Distribution", size(vlarge))
graph save fig7_b, replace

graph combine fig7_a.gph fig7_b.gph, col(2)
graph export "$output\figure5.emf",  replace




**********************
*** POWER ANALYSIS ***
**********************

* MDE given a sample size of n=64 (equally across DEL and chiefs), assuming 50% share of democratic rule choice among chiefs
power twoproportions 0.5,  alpha(0.05 0.1) n(64) diff(0(.05).4) onesided  /*
	*/ table(alpha power beta N delta:"effect size") graph(ytitle(Effect size) legend(ring(0) rows(1) pos(12) size(medium)) title("One-sided MDE - fixed sample size: ""Pr(democratic rule|chiefs)=0.5 & n=64") xdimension(power) xla(0(0.1)1) yla(0(0.05)0.4) ydimension(diff))
gr export "$output\MDE_n64.emf",replace

* Sample size gains
power twoproportions 0.58, alpha(0.05 0.1) power(0.8) n(64(32)512) /*
	*/ table(alpha delta:"min. detectable effect size" power beta N) graph(ytitle(Effect size) legend(ring(0) rows(1) pos(12) size(medium)) title("MDE - fixed power:""Pr(democratic rule|DEL)=0.58 & 80% power")ydimension(delta) xla(64(32)512) yla(0(0.05)0.4))
gr export "$output\MDE_power.emf",replace