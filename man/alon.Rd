\docType{data}
\name{alon}
\alias{alon}
\title{Gene expression profiles in colon cancer cell}
\description{
Gene expression profiles in colon cancer cells with 62 observations and 200 variables. 180 gene expression values were randomly chosen from the original dataset while 20 gene expression values are most related to the true cancer type based on a lasso model. The outcome variable is \emph{subtype} (tumour/normal = 40/22). The two spatial coordinates are \emph{x,y} which are simulated as the first two PCs from a Principal Component (PC) Analysis on the 20 most related gene expression values.
}
\usage{
data(alon)
}
\examples{
data(alon)
}
\source{
Alon, U., Barkai, N., Notterman, D., Gish, K., Ybarra, S., Mack, D. and Levine, A. (1999), ‘Broad patterns
of gene expression revealed by clustering analysis of tumor and normal colon tissues probed by
oligonucleotide arrays’, \emph{Proceedings of the National Academy of Sciences} 96(12), 6745–6750.
}
