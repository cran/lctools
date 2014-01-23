\name{acc}
\alias{acc}

\title{Spatial Interaction Models: Destination Accessibility}
\description{
Destination accessibility or centrality or competition is a variable that when added to a destination choice model forms the competing destinations choice model. A simple formula for this variable is: \deqn{A_j = \Sigma ( W_m / D_{jm} ) | m<>j } where Aj is the potential accessibility of destination j to all other potential destinations m, Wm is a weight generally measured by population, and Djm is the distance between j and m.
  }

\usage{
acc(X, Y, Pop, Power=1)
}

\arguments{
  \item{X}{a numeric vector of x coordinates}
  \item{Y}{a numeric vector of y coordinates}
  \item{Pop}{a numeric vector of the weights, usually a population variable}
  \item{Power}{a power of the distance; default is 1}
}

\value{
  \item{AccMeasure}{a single column numeric matrix of accessibility scores}
}

\references{
Kalogirou, S., 2003, The Statistical Analysis And Modelling Of Internal Migration Flows Within England And Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. URL: \url{https://theses.ncl.ac.uk/dspace/handle/10443/204} 
}

\author{Stamatis Kalogirou <skalo@hua.gr>}

\note{X,Y should be Cartesian coordinates for the distances to be measured in meters. In the sample dataset GreeceLAs the projection used is the EPSG:2100 (GGRS87 / Greek Grid)}

\examples{
data(GreeceLAs)
aMeasure<-acc(GreeceLAs$X, GreeceLAs$Y,GreeceLAs$totalp,1)
summary(aMeasure)
}

\keyword{accessibility}
\keyword{centrality}
