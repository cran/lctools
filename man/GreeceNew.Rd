\name{GreeceNew}
\alias{GreeceNew}
\docType{data}
\title{Municipalities in Greece in 2011}
\description{Centroid coordinates and employment variables from the 2001 Census aggregated to the new 325 Municipalities in Greece (Kallikrates)}

\usage{data(GreeceNew)}
\format{
  A data frame with 325 observations on the following 5 variables.
  \describe{
    \item{\code{ID}}{a numeric vector of area IDs}
    \item{\code{X}}{a numeric vector of x coordinates}
    \item{\code{Y}}{a numeric vector of y coordinates}
    \item{\code{PrimSector}}{a numeric vector of the proportion of economically active working in the primary financial sector (mainly agriculture; fishery; and forestry)}
    \item{\code{UnemplRate}}{a numeric vector of total unemployment rate}
  }
}
\details{
The X,Y coordinates refer to the geometric centroids of the new 325 Municipalities in Greece (Kallikrates) in 2011. The new municipalities were aggregated from the old 1033 Local Authorities (Kapodistrias) in 2007. 
}
\source{
The shapefile of the corresponding polygons is available from the Public Open Data of the Greek Government at \url{http://geodata.gov.gr/geodata}. The population data is available from the Hellenic Statistical Authority (EL.STAT.) at \url{http://www.statistics.gr} but were aggregated to the new municipalities by the author.
}
\references{
Kalogirou, S. (2013) Testing geographically weighted multicollinearity diagnostics, GISRUK 2013, Department of Geography and Planning, School of Environmental Sciences, University of Liverpool, Liverpool, UK, 3-5 April 2013. \url{http://gisc.gr/docs/sk_papers/2_7_Kalogirou_2013.pdf}

}
\examples{
data(GreeceNew)
summary(GreeceNew$UnemplRate)
hist(GreeceNew$PrimSector)
plot(GreeceNew$X,GreeceNew$Y)
}
\keyword{datasets}
