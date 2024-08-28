\name{ari}
\alias{ari}
\title{
Calculate Adjusted Rand Index
}
\description{
The function calculate Adjusted Rand Index (ARI) if the true cluster label is available.
}
\usage{
ari(group1, group2)
}
\arguments{
\item{group1}{
The predicted cluster label.
}
\item{group2}{
The expected/true cluster label.
}
}
\value{
A data frame with the transformed data. The dependent variable is moved to the last column of the data.
}

\references{
Hubert, L. and Arabie, P. (1985), ‘Comparing partitions journal of classification 2 193–218’, Google Scholar pp. 193–128.
}
\author{
Min Lu (Maintainer,<luminwin@gmail.com>)
}
\examples{
\donttest{
data(alon)
obj <- srf(data = alon$gene, coordinates = cbind(alon$x,alon$y))
########### performance of Spatial Random Forest
ari(obj$cluster,alon$subtype)
}
}
