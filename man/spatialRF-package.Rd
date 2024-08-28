\name{spatialRF}
\alias{spatialRF}
\alias{spatialRF-package}
\docType{package}
\title{
  Clustering Methods with Spatial Coordinates via Random Forest Distance
}
\description{
The package \code{spatialRF} implements multivariate random forests to calculate distance matrix and runs existing clustering algorithms for clustering problems with additional spatial coordinate data. It firstly reduces the dimension of gene expression profiles using existing methods such as principle components or unsupervised variable priority, then combine the information of gene expression and spatial coordinates to generate the distance matrix using advanced random forest techniques, and assign clusters on the distance matrix using existing methods such as DBSCAN or spectral clustering.
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
