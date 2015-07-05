\name{w.matrix}
\alias{w.matrix}
\title{Weights Matrix based on a fixed number of nearest neighbours}
\description{This function constructs an n by n weights matrix for a geography with n geographical elements (e.g. points or polygons) using a fixed number of nearest neighbours.}

\usage{
w.matrix(Coords, Bandwidth, WType = "Binary", family = "adaptive")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the geographical elements (data points or geometric / population weighted centroids for polygons)}
  \item{Bandwidth}{in this version this can only be a positive integer that defines the number of nearest neighbours for the calculation of the weights. In future versions this could also be a fixed distance.}
  \item{WType}{a string giving the weighting function used to compute the weights matrix. 
  Options are: "Binary", "Bi-square", "RSBi-square". Default is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise;

  RSBi-square: weight = Bi-square weights / sum (Bi-square weights) for each row in the weights matrix
  }
  \item{family}{a string giving the weighting scheme used to compute the weights matrix. Currently the only option is "adaptive" which refers to a number of nearest neighbours. In future versions of the lctools package, a "fixed" weighting scheme should be supported by setting a fixed distance around each observation's location.
  }
}
\value{
  A matrix of weights
}
\references{
Kalogirou, S., 2003, The Statistical Analysis And Modelling Of Internal Migration Flows Within England And Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. URL: \url{https://theses.ncl.ac.uk/dspace/handle/10443/204}
}
\author{Stamatis Kalogirou <skalo@hua.gr>}

\seealso{
 \code{\link{moransI.w}}, \code{\link{spGini.w}}
}
\examples{
data(GR.Municipalities)
attr<-GR.Municipalities@data
w<-w.matrix(cbind(attr$X, attr$Y),6)
}
\keyword{ Spatial Analysis }
\keyword{ Weights Matrix }
