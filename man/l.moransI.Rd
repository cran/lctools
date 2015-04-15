\name{l.moransI}
\alias{l.moransI}

\title{Local Moran's I classic statistic for assessing spatial autocorrelation}

\description{
The local Moran's I proposed by Anselin (1995). The formula to calculate the local I_i which is now used in most textbooks and software is: \deqn{I_i = ((x_i - mean(x))/m_2)*(\Sigma w_{ij}*z_j)} where n is number of observations, w_ij are the weights, z_j=x_j - mean(x), x being the value of the variable at location i and mean(x) being the mean value of the variable in question, and \deqn{m_2 = (\Sigma (x_i - mean(x))^2) / n }
}

\usage{l.moransI(Coords, Bandwidth, x, WType='Binary')
}
\arguments{
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{Bandwidth}{a positive integer that defines the number of nearest neighbours for the calculation of the weights}
  \item{x}{a numeric vector of a variable}
  \item{WType}{string giving the weighting scheme used to compute the weights matrix. 
  Options are: "Binary" and "Bi-square". Default is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise.}
}
\details{The interpretation of the local I_i is similar to that of the global Moran's I.}

\value{Returns the calculated local Moran's I and a list of statistics for the latter's inference: the expected Ei, the variance Vi, the Xi scores and the p-values for the randomization null hypotheses. It also returns the standardized value and the standardized lagged value of the variable to allow creating the Moran's I scatter plot and the classified values for creating the cluster map similar to those available in GeoDa (Anselin et al., 2006).

  \item{ID}{Numeric index from 1 to n}
  \item{Ii}{Classic lobal Moran's I_i statistic}
  \item{Ei}{The expected local Moran's I_i}
  \item{Vi}{The variance of I_i}
  \item{Zi}{The z score calculated for the randomization null hypotheses test}
  \item{p.value}{The p-value (two-tailed) calculated for the randomization null hypotheses test}
  \item{Xi}{The standardised value of the variable x}
  \item{wXj}{The standardised value of the lagged x (weighted some of nearest neighbours)}
  \item{Cluster}{The class each observation belongs based on the sign of Xi and wXj as well as the non-significant local Moran's I values}
}

\references{
Anselin, L.,1995, Local Indicators of Spatial Association-LISA. Geographical Analysis, 27, 93-115. 

Anselin, L., Syabri, I. and Kho., Y., 2006, GeoDa: An Introduction to Spatial Data Analysis. Geographical Analysis 38(1), 5-22.
}
\author{Stamatis Kalogirou <skalo@hua.gr>}

\note{Please note that the weights are row standardised.}

\examples{
data(GR.Municipalities)
l.moran<-l.moransI(cbind(GR.Municipalities$X, GR.Municipalities$Y),6,GR.Municipalities$Income01)
boxplot(l.moran$Ii)
}
\keyword{local Moran's I}
\keyword{Moran's I Clusters}
\keyword{LISA}