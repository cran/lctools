\name{spg.sim.test}
\alias{spg.sim.test}
\title{Simulation test (Rey & Smith, 2013))}
\description{This is a simulation test presented by Rey & Smith (2013) as part of the inference for the spatial decomposition of the Gini coefficient.}
\usage{
spg.sim.test(realizations = 1000, nperm = 99)
}
\arguments{
  \item{realizations}{Number of realizations of the simulation.}
  \item{nperm}{Number of permutations of simulated spatial autocorrelation and spatial gini simulations for each realization.}
}
\details{
This function implements the simulation presented in pages 63-66 of the paper by Rey & Smith (2013). The results of this function should be comparable to the Figures 1, 2 and 3. However, the current implementation of this function results in different frequencies.
}
\value{
A list of results:
  \item{res}{}
  \item{moran_res}{}
  \item{gini_mean}{}
  \item{gini_std}{}
  \item{moran_mean}{}
}

\references{
Rey, S.J., Smith, R. J. (2013) A spatial decomposition of the Gini coefficient, Letters in Spatial and Resource Sciences, 6 (2), pp. 55-70.}

\author{Stamatis Kalogirou <skalo@hua.gr>}

\note{This function is under development.}

\section{Warning}{This simulation takes time to complete and the results may not match those presented in the paper by Rey and Smith (2013).}

\seealso{
\code{\link{mc.spGini}}
\code{\link{spGini}}
}

\keyword{Simulation}
\keyword{Spatial Gini}
\keyword{Spatial Inequality}
\keyword{Spatial autocorrelation}