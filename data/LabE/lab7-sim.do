
clear
prog drop _all

global obs 10
global N 1000

simulate b=_b[x],  reps($obs): estols2 $N

sum


kdensity b, saving(lec7-sim, replace) ti("Observed Distribution of B, $obs reps") ///
sub("N=${N} , e~${dist} ")

