\name{spGini}
\alias{spGini}
\title{Spatial Gini coefficient}
\description{This is the implementation of the spatial decomposition of the Gini coefficient introduced by Rey and Smith (2013). The function calculates the global Gini and the two components of the spatial Gini: the inequality among nearest (geographically) neighbours and the inequality of non-neighbours. Three weighted schemes are currently supported: binary, bi-square and row standardised bi-square.}
\usage{
spGini(Coords, Bandwidth, x, WType = "Binary")
}
\arguments{
  \item{Coords}{a numeric matrix or vector or dataframe of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{Bandwidth}{a positive value that defines the number of nearest neighbours for the calculation of the weights}
  \item{x}{a numeric vector of a variable}
  \item{WType}{string giving the weighting scheme used to compute the weights matrix. 
  Options are: "Binary", "Bi-square", "RSBi-square". Default is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise;

  RSBi-square: weight = Bi-square weights / sum (Bi-square weights) for each row in the weights matrix}

}

\value{Returns a list of three values Gini=gGini,gwGini=gwGini,nsGini=nsGini

  \item{Gini }{Global gini}
  \item{gwGini}{First component of the spatial Gini: the inequality among nearest (geographically) neighbours}
  \item{nsGini}{Second component of the spatial Gini: the inequality among non-neighbours}
}

\references{
Rey, S.J., Smith J.S., 2013, A spatial decomposition of the Gini coefficient, Letters in Spatial and Resource Sciences, 6 (2), pp. 55-70.}

\author{Stamatis Kalogirou <skalo@hua.gr>}

\examples{
data(GreeceNew)
Coords1<-cbind(GreeceNew$X, GreeceNew$Y)
Bandwidth1<-12
x1<-GreeceNew$UnemplRate
WType1<-'Binary'
spGini(Coords1,Bandwidth1,x1,WType1)
}

\keyword{Gini}
\keyword{Spatial Gini}
\keyword{Spatial Inequality}
\keyword{Spatial autocorrelation}