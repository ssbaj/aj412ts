\name{irf22}
\alias{irf22}
\title{Calc: colorful IRF}
\usage{
irf22()
}
\description{
Calculating IRF
}
\examples{

   irf22( irf(re.var, impulse='lp', response='ly', n.ahead=15) )
   irf22( irf(re.var, impulse='ly', n.ahead=15 ) )


}