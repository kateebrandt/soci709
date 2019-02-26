
program define  estols2
args N
drop _all

set obs `N'
gen x=3*invnorm(uniform())

* gen e=15*invnorm(uniform())
gen e=15*invnorm(uniform())
global dist="bimodal beta"

gen y=3*x+e

* the correct answer is 3

quietly reg y x

di "This is the regression estimate of B and the constant"
mat list e(b)

end

