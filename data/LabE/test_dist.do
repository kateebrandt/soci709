

* test your distribution

* see help density functions for other inverse functions that you could use


clear

set obs 2000

* change the two numbers below to get a different beta distribution

global alpha=.5
global beta=.5

gen x=invibeta($alpha, $beta, uniform())


sum 

kdensity x if x>0 & x<1, bwidth(.05)  xlab(0 .5 1)  title("beta distribution alpha=${alpha}, beta=.${beta}") sub("2000 obs")

* note the graph is a bit off at 0 and 1, because the beta function is bounded by 0 and 1.
