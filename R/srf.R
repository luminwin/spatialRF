srf <- function(data, coordinates,
                dimReduc = c("PCA", "varPro", "none"),
                clusMethod = c("hdbscan", "spectral", "dbscan"),
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
                ...) {
oReturn <- list()

if (ncol(data) <= numDims) {dimReduc[1] <- "none"}
if (dimReduc[1] == "none") {ydta <- data; oReturn$obj.dimReduc <- NULL}
if (dimReduc[1] == "PCA") {
  obj.dimReduc <- stats::prcomp(data, center = FALSE, scale. = FALSE)
  ydta <- obj.dimReduc$x[,1:numDims]
}
if (dimReduc[1] == "varPro") {
  
  cat("PC is actually better and the github package kogalur/varPro is not ready so using PCs instead")
  obj.dimReduc <- stats::prcomp(data, center = FALSE, scale. = FALSE)
  ydta <- obj.dimReduc$x[,1:numDims]
}

if (lowMemory == TRUE) {rm(obj.dimReduc)} else {oReturn$obj.dimReduc <- obj.dimReduc}


colnames(ydta) <- paste("X", 1:ncol(ydta), sep = "")

if (splitrule[1] != "mahalanobis") {splitrule <- NULL}

obj.rf <- randomForestSRC::rfsrc(randomForestSRC::get.mv.formula(colnames(ydta)),
                data.frame(ydta, coordinates), proximity = TRUE,
                ntree = ntree,
                nodesize = nodesize,
                splitrule = splitrule, nsplit = nsplit,
                membership = membership,
                na.action = na.action, nimpute = nimpute,
                case.wt  = case.wt,
                forest = forest,
                seed = seed)
prox <- obj.rf$proximity
oReturn$distance <- exp(-prox) - exp(-1)

if (membership == TRUE) {oReturn$membership <- obj.rf$membership}
if (lowMemory == TRUE) {rm(obj.rf)} else {oReturn$obj.rf <- obj.rf}


if (clusMethod[1] == "spectral"){
res <- kernlab::specc(oReturn$distance, centers= numClus )
oReturn$cluster <- as.numeric(res)
}
if (clusMethod[1] == "dbscan"){
  res <- dbscan::dbscan(oReturn$distance,eps = eps)
  oReturn$cluster <- res$cluster
}
if (clusMethod[1] == "hdbscan"){
  res <- dbscan::hdbscan(oReturn$distance,minPts = minPts)
  oReturn$cluster <- res$cluster
}

if (lowMemory == TRUE) {rm(res)} else {oReturn$obj.clust <- res}
return(oReturn)
}
