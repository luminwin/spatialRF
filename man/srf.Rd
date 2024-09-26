\name{srf}
\alias{srf}
\title{
Clustering Methods with Spatial Coordinates via Random Forest Distance
}
\description{
The function implements multivariate random forests to calculate distance matrix and runs existing clustering algorithms for clustering problems with additional spatial coordinate data. It firstly reduces the dimension of gene expression profiles using existing methods such as principle components or unsupervised variable priority, then combine the information of gene expression and spatial coordinates to generate the distance matrix using advanced random forest techniques, and assign clusters on the distance matrix using existing methods such as DBSCAN or spectral clustering.   The random forest implementation was directly imported from the \pkg{randomForestSRC}
  package.  Use the command \code{package?randomForestSRC} for more information.
}
\usage{
srf(data,
  coordinates,
  dimReduc = c("PCA", "varPro", "none"),
  clusMethod = c("kmean", "hdbscan", "spectral", "dbscan"),
  numDims = 20,
  numClus = 2,
  eps = 0.7,
  minPts = 5,
  forest = TRUE,
  ntree = 1000,
  nodesize = 3,
  splitrule = c("mse", "mahalanobis"),
  nsplit = 10,
  bootstrap = c("by.root", "none", "by.user"),
  membership = FALSE,
  na.action = c("na.omit", "na.impute"), nimpute = 1,
  case.wt  = NULL,
  seed = 123,
  lowMemory = FALSE,
  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data}{Data frame containing the $p$ dimensional x-variables.}

  \item{coordinates}{Data frame containing the $d$ dimensional coordinates (usually $d$=2).}

  \item{dimReduc}{Methods chosen for dimension reduction for the  x-variables with options c("PCA", "varPro", "none"). \code{PCA}
  means principle component analysis (default); \code{varPro} means unsupervised variable priority method; \code{none} means no dimension reduction procedure is implemented.}

    \item{clusMethod}{ Methods chosen for assignming clusters based the distance matrix: "kmean" (default) for k-mean method, "hdbscan" for Hierarchical Density-Based Spatial Clustering of
Applications with Noise, "spectral" for Spectral Clustering and  "dbscan" for Density-Based Spatial Clustering of Applications with
Noise.}

  \item{numDims}{Number of PCs to be chosen for \code{dimReduc = "PCA"}. Number of top x-variables to be chosen for \code{dimReduc = "varPro"}. }
  \item{numClus}{Number of clusters for \code{clusMethod = "spectral"}. This argument will be ignored for "dbscan" and "hdbscan" since they do not require such specification. }

    \item{eps}{Parameter specified for \code{clusMethod = "dbscan"}. Use the command \code{package?dbscan} for more information.}
    \item{minPts}{Parameter specified for \code{clusMethod = "hdbscan"}. Use the command \code{package?dbscan} for more information.}

    \item{forest}{Save key forest values?  Used for prediction on new data
    and required by many of the package functions. Turn this off if you
    are only interested in training a forest.}

  \item{ntree}{Number of trees for the random forest methods to calculate distance matrix.}

  \item{nodesize}{Minumum size of terminal node for the random forest methods to calculate distance matrix.  It is recommended to experiment with different \code{nodesize} values.}

  \item{splitrule}{Splitting rule for the random forest methods to calculate distance matrix. By default, a multivariate normalized composite splitting rule is used to split nodes (users have the option to use Mahalanobis splitting).}

  \item{nsplit}{Non-negative integer specifying number of random splits
    for splitting a variable for random forest.  When zero, all split values are
    used (deterministic splitting), which can be slower.  By default
    10 is used.}

   \item{bootstrap}{Bootstrap protocol.  Default is \code{by.root} which
    bootstraps the data by sampling without replacement.
    If \code{none}, the data is not bootstrapped (it is not possible to
    return OOB ensembles or prediction error in this case).}

  \item{membership}{Should terminal node membership and inbag
    information be returned?}

  \item{na.action}{Action taken if the data contains \code{NA}'s.
    Possible values are \code{na.omit} or \code{na.impute}.  The default
    \code{na.omit} removes the entire record if any entry is
    \code{NA}. Selecting \code{na.impute} imputes the data (see below
    for details).  Also see the function \command{impute} for fast
    imputation.}

  \item{nimpute}{Number of iterations of the missing data algorithm.
    Performance measures such as out-of-bag (OOB) error rates are
    optimistic if \code{nimpute} is greater than 1.}

  \item{case.wt}{Vector of non-negative weights (does not have to sum to
    1) for sampling cases.  Observations with larger weights will be
    selected with higher probability in the bootstrap (or subsampled)
    samples.  It is generally better to use real weights rather than
    integers. See the breast data example below illustrating its use
    for class imbalanced data.}

  \item{seed}{Negative integer specifying seed for the random number
    generator.}

   \item{lowMemory}{Whether to remove all the returned objects. Set \code{TRUE} if you have low memory in your computer and want to speed up the algorithm. }

  \item{...}{Further arguments passed to or from other methods.}
}

\value{
An object of class \code{(srf)} with the following
  components:
  \item{cluster}{Estimated cluster labels.}
    \item{distance}{Distance matrix of all the data points.}
    \item{obj.dimReduc}{The object for dimension reduction. Use the command \code{package?varPro} for more information if \code{dimReduc = "varPro"}.}
     \item{obj.rf}{The random forest object for calculating the distance matrix. Use the command \code{package?randomForestSRC} for more information}
    \item{obj.clust}{The bject for calculating the cluster assignment. Use the command \code{package?kernlab} for more information if "spectral" is chosen for arguments.  Use the command \code{package?dbscan} for more information if "dbscan" or "hdbscan" is chosen for arguments.}
  \item{membership}{If \code{membership=TRUE}, this is the matrix recording terminal node membership where
    each column records node mebership for a case for a tree (rows). Note that this is not used for clustering assignment; clustering assignment is based on the distance matrix. }

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

