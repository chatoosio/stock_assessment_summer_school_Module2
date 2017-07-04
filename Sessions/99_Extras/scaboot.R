library(FLa4a)
data(ple4)
data(ple4.index)
fit <- sca(ple4, FLIndices(ple4.index))
res <- residuals(fit, ple4, FLIndices(ple4.index))

nb <- 50
flq <- propagate(res[[2]], nb)
flq[] <- sample(c(res[[2]]), length(flq), replace=T)
idx <- index(ple4.index)
idx <- propagate(idx, nb)
idx <- exp(log(idx)+flq)
idxs <- FLIndices(ple4.index)
index(idxs[[1]]) <- idx
fit <- sca(ple4, idxs)
plot(ple4+fit)
