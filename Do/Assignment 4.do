************* Assignment 4 *************

//Github repo and summary 
cd "/Users/paula/Documents/GitHub/RDD"
use https://github.com/scunning1975/causal-inference-class/raw/master/hansen_dwi, clear
br 
cd "/Users/paula/Documents/GitHub/RDD/Data"
save Hansen_dwi.dta, replace

//Replication 
/**3. Treatment variable 
bac1>=0.08 and 0 otherwise **/
gen D=1 if bac1>=0.08
replace D=0 if D==.

sum D if D==1
/**4. Figure 1**/
net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace
net install lpdensity, from(https://sites.google.com/site/nppackages/lpdensity/stata) replace
net install DCdensity, from(https://sites.google.com/site/nppackages/DCdensity/stata) replace
rddensity bac1, c(0.08) plot
rddensity bac1, c(0.08) plot graph_options(graphregion(color(white)) title("McCrary Test") xtitle("BAC1") ytitle(Density) legend(off))
/*Since de p value of the test is 0.0276 we reject the null hypothesis with a 5% significance level. This means that there is evidence to say that there was manipulation of bac1 around the cutoff*/
rddensity bac1, all c(0.08) plot graph_options(graphregion(color(white)) title("McCrary Test") xtitle("BAC1") ytitle(Density) legend(off)) //Without using the bias-corrected version of the density test we get the same p value as Hansen (conventional result)
rddensity bac1, all c(0.08) plot
//Figure 1 replicate
 hist bac1, freq discrete xline(0.08)

/**5. Table 2 Panel A**/
//Recentering the data
gen bac1_c = bac1 - 0.08
save Hansen_dwi.dta, replace
clear all
use  Hansen_dwi.dta

//Male
gen bac1D = bac1*D
cd "/Users/paula/Documents/GitHub/RDD/Tables"
reg male D bac1 bac1D, r 
outreg2 using "Tabla2PanelA.doc", replace

//White 
reg white D bac1 bac1D, r 
outreg2 using "Tabla2PanelA.doc", append

//Age
reg aged D bac1 bac1D, r
outreg2 using "Tabla2PanelA.doc", append

//Accident
reg acc D bac1 bac1D, r 
outreg2 using "Tabla2PanelA.doc", append

sum male white aged acc


/**6. Figure 2 **/
ssc install cmogram
cd "/Users/paula/Documents/GitHub/RDD/Tables"
//Accident at scene 
cmogram acc bac1 , cut(0.08) scatter line(0.08) lfitci histopts(width(0.002)) title("Panel A1. Accident at scene")
cmogram acc bac1, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002)) title("Panel A2. Accident at scene")


//Male
cmogram male bac1 , cut(0.08) scatter line(0.08) lfitci histopts(width(0.002)) title("Panel B1. Male")
cmogram male bac1, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002)) title("Panel B2. Male")

//Age
cmogram age bac1 , cut(0.08) scatter line(0.08) lfitci histopts(width(0.002)) title("Panel C1. Age")
cmogram age bac1, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002)) title("Panel C2. Age")

//White
cmogram white bac1 , cut(0.08) scatter line(0.08) lfitci histopts(width(0.002)) title("Panel D1. White")
cmogram white bac1, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002)) title("Panel D2. White")

//Juntar todas 
cd "/Users/paula/Documents/GitHub/RDD/Figures"
graph combine "A1ACC" "B1MALE" "C1AGE" "D1WHITE", title("Figure 3A: Balance Analysis Using Linear Fit")
graph combine "A2ACC" "B2MALE" "C2AGE" "D2WHITE", title("Figure 3B: Balance Analysis Using Quadratic Fit")

/**7. Table 3 column 1 **/
cd "/Users/paula/Documents/GitHub/RDD/Tables"
gen bac1sq=bac1*bac1
gen bac1Dsq=bac1sq*D
//Panel A
//Column 1: control for the bac1 linearly
reg recidivism D bac1 acc male aged white if bac1>=0.03 & bac1<=0.13, r
	outreg2 using "Table3A.doc", replace title("Panel A") 
//Column 2: interact bac1 with cutoff linearly
reg recidivism D bac1 bac1D acc male aged white if bac1>=0.03 & bac1<=0.13, r
	outreg2 using "Table3A.doc", append 
//Column 3: interact bac1 with cutoff linearly and as a quadratic
reg recidivism D bac1 bac1D bac1sq bac1Dsq acc male aged white if bac1>=0.03 & bac1<=0.13, r
	outreg2 using "Table3A.doc", append 
	
sum recidivism if bac1>=0.03 & bac1<=0.13
	
//Panel B
//Column 1: control for the bac1 linearly
reg recidivism D bac1 acc male aged white if bac1>=0.055 & bac1<=0.105, r
	outreg2 using "Table3B.doc", replace title("Panel B") 
//Column 2: interact bac1 with cutoff linearly
reg recidivism D bac1 bac1D acc male aged white if bac1>=0.055 & bac1<=0.105, r
	outreg2 using "Table3B.doc", append 
//Column 3: interact bac1 with cutoff linearly and as a quadratic

reg recidivism D bac1 bac1D bac1sq bac1Dsq acc male aged white if bac1>=0.055 & bac1<=0.105, r
	outreg2 using "Table3B.doc", append 
	
sum recidivism if bac1>=0.055 & bac1<=0.105


/**8. Figure 3 Panel A **/
//a.	Fit linear fit using only observations with less than 0.15 bac on the bac1
cmogram recidivism bac1 if bac1<0.15, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002)) title(" Figure 4A: Panel A. Linear Fit")

//b.	Fit quadratic fit using only observations with less than 0.15 bac on the bac1
cmogram recidivism bac1 if bac1<0.15, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002)) title("Figure 4B: Panel A. Quatratic Fit")

cd "/Users/paula/Documents/GitHub/RDD/Figures"
graph combine "PanelA" "PanelB", title("Figure 4: Linear Fit and Quadratic Fit")














