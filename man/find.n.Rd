\name{find.n}
\alias{find.n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
finding satisfactory sample size for a hierarchical Bayesian Clinical Trial design
}
\description{
generates artificial data from binomial model, samples interested parameters from posterior distributions, simulates the operating characteristics, and chooses a design (sample size) that delivers pre-specified desired operating characteristics.
}
\usage{
find.n(del.cut, k, T, p.c, effect.size, alpha.target = 0.1, 
       power.target = 0.9, tol.b1 = 0.02, tol.b2 = 0.02, tol.a = 0.05, 
	   n.min = 5, n.max = 70, nstep=10, rep = 500, pathcode = getwd(), 
	   codefile = "BUGS_Bin_2arm.txt", pathout = getwd(), method = "linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{del.cut}{
$delta_{cut}$, efficacy boundary 
}
  \item{k}{
number of tumor types 
}
  \item{T}{
$T_p$, threshold probability 
}
  \item{p.c}{
$pi_C$, a vector of true response rates for k tumor type in control arm
}
  \item{effect.size}{
$delta_i$, effect size to dectect in treatment group 
}
  \item{alpha.target}{
desired family-wise Type-I error rate, default is 0.1. 
}
  \item{power.target}{
desired level of individual power for each type of tumor, default is 0.9. 
}
  \item{tol.b1}{
tolerance of lower bound of target individual power rate, default is 0.02. 
}
  \item{tol.b2}{
tolerance of upper bound of target individual power rate, default is 0.02. 
}
  \item{tol.a}{
tolerance of upper bound of target family-wise Type\_I error, default is 0.05. 
}
  \item{n.min}{
the minimum sample size needed, default is 5. 
}
  \item{n.max}{
the maximum sample size afforded, default is 70. 
}
  \item{nstep}{
the search precision of "linear" method. Linear method starts searching from nmin, 
and linearly increases n by nstep each time to find the optimal sample size.
}
  \item{rep}{
number of repetition of generating posteriors $Pr(delta_i > delta_{cut} | data) > T_p$, default is 500.
}
  \item{pathcode}{
path to the BRugs model file. If missing, current working directory will be used to search for BRugs model. 
}
  \item{codefile}{
name of the file containing the BRugs model. Defualt is "BUGS\_Bin\_2arm.txt". 
}
  \item{pathout}{
path for the output files. If missing, current working directory will be used to search for BRugs model. 
}
  \item{method}{
available methods "linear" or "bisect". "linear" method linearly increases 
the sample size from n.min by nstep each time until it hits the optimal sample 
size or until it reaches n.max. "bisect" method use the Bisection method to 
find the optimal sample size. Default is "linear" method.
}
}
\value{
For each sample size searched ($n_{searched}$), find.n generates text file "alt5\_0.025\_<$n_{searched}$>.txt" and "null\_0.025\_<$n_{searched}$>.txt" to store posteriors $\{ Pr(delta_i > 0.025 | data) > 0.85; i=1,\ldots,k \}$ at each one of $N_{rep}$ repetitions under alternatives and null hypothesis. After $N_{rep}$ repetitions, the Operating Characteristics corresponding to $n_{searched}$ is output to text file "history\_linear.txt". Once the searching-for-sample-size process is over, find.n outputs a message whether the satisfactory sample size is found or not. Meanwhile, the Operating Characteristics related to the final sample size is print out. 

\item{del.cut}{efficacy boundary $delta_{cut}$ }
\item{T }{threshold probability $T_p$ }
\item{n }{final sample size per arm per group}
\item{alpha.fw}{final family-wise Type-I error rate }
\item{power.ind1,\ldots,power.indk}{final individual power rates for each type of tumor} 
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\note{
find.n calls function bisect, power.fun, and alpha.fun.
}

\examples{
## use Linear method
find.n(del.cut=0.025, k=3, T=0.85, p.c=c(0.5, 0.4, 0.65), effect.size=0.2,
        alpha.target=0.1, power.target=0.9, 
        tol.b1=0.02, tol.b2=0.02, tol.a=0.05,
        rep=500, pathcode=getwd(), codefile="BUGS_Bin_2arm.txt", pathout=getwd())

## use Bisect method
find.n(del.cut=0.025, k=3, T=0.85, p.c=c(0.5, 0.4, 0.65), effect.size=0.2,
        alpha.target=0.1, power.target=0.9, 
        tol.b1=0.02, tol.b2=0.02, tol.a=0.05,
        rep=500, pathcode=getwd(), codefile="BUGS_Bin_2arm.txt", pathout=getwd(),
        method="bisect")
}
