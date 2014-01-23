\name{GreeceLAs}
\alias{GreeceLAs}
\docType{data}
\title{Local Authorities in Greece in 2001}
\description{
Centroid coordinates and census population in 2001 for the 1033 Local Authorities in Greece
}
\usage{data(GreeceLAs)}
\format{
  A data frame with 1033 observations on the following 4 variables.
  \describe{
    \item{\code{Area}}{A 4-digit area code of each local authority linking to the 2001 Census Data}
    \item{\code{X}}{a numeric vector of x coordinates}
    \item{\code{Y}}{a numeric vector of y coordinates}
    \item{\code{totalp}}{a numeric vector of the total population of the local authority in 2001 (Census)}
  }
}
\details{
The X,Y coordinates refer to the geometric centroids of the Local Authorities in Greece in 2001 (Kapodistrias). 
}
\source{
The shapefile of the corresponding polygons is available from the Public Open Data of the Greek Government at \url{http://geodata.gov.gr/geodata}. The population data is available from the Hellenic Statistical Authority (EL.STAT.) at \url{http://www.statistics.gr}.
}
\references{
The map of local authorities can be seen in this paper:

Kalogirou, S., 2010, Spatial inequalities in income and post-graduate educational attainment in Greece, Journal of Maps, 6, 1, pp. 393-400. doi: 10.4113/jom.2010.1095.
}
\examples{
data(GreeceLAs)
summary(GreeceLAs$totalp)
plot(GreeceLAs$X,GreeceLAs$Y)
}
\keyword{datasets}
