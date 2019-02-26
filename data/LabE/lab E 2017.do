
// program part

capture program drop estols2 
program define  estols2
args N
drop _all
set obs `N'
gen x=3*invnorm(uniform())
 
//gen e=15*invnorm(uniform())
gen e=15*invibeta(.5, .5, uniform())
global dist="beta"
 
//gen y_alt=3*x 
gen y=3*x+e
* the correct answer is 3
quietly reg y x
 
di "This is the regression estimate of B and the constant"
mat list e(b)
end


// run part

clear
// prog drop _all
 
//estols2 50 
 
global obs 200
global N 200
 
simulate b=_b[x],  reps($obs): estols2 $N
 
sum
 
 
kdensity b, saving(lec7-sim, replace) ti("Observed Distribution of B, $obs reps") ///
sub("N=${N} , e~${dist} ")
