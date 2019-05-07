setwd("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/Spectral-Clustering/src")
Rcpp::sourceCpp("spectral_aux.cpp", verbose=TRUE, rebuild=TRUE)
library(ggplot2)


Laplacian_eigen <- function(D, G, k) {
    L <- D - G;
    E <- eigen(L, symmetric=TRUE)$vectors;
    return(E[,(k+1):2]);
}

Kmeans_division <- function(X, k) {
    #https://stackoverflow.com/questions/28273012/kmeans-gives-wrong-cluster-in-r
    Xscaled = X;
    Xscaled[,1] = scale(Xscaled[,1]);
    Xscaled[,2] = scale(Xscaled[,2]);
    clusterResult <- kmeans(Xscaled, k);
    plot(Xscaled, col=clusterResult$cluster);
    text(Xscaled, labels=clusterResult$cluster, cex=0.7, pos=2);
}


# example 1
X1 <- matrix(c(0, 10, 1, 10, 3, 10, 6, 10, 14, 2, 16, 2, 19, 2), ncol=2, byrow=TRUE)
S1 <- Mnn(X1, 3)
S1_expected <- matrix(c(1, 2, 3, 0, 2, 3, 1, 3, 0, 2, 1, 0, 5, 6, 3, 4, 6, 3, 5, 4, 3), ncol=3, byrow=TRUE)
stopifnot(all(S1 == S1_expected))

G1 <- Mnn_graph(S1)
G1_expected <- matrix(c(
    0, 1, 1, 1, 0, 0, 0,
    1, 0, 1, 1, 0, 0, 0,
    1, 1, 0, 1, 0, 0, 0,
    1, 1, 1, 0, 1, 1, 1,
    0, 0, 0, 1, 0, 1, 1,
    0, 0, 0, 1, 1, 0, 1,
    0, 0, 0, 1, 1, 1, 0), ncol=7, byrow=TRUE)
stopifnot(all(G1 == G1_expected))


# example 2
X2 <- matrix(c(1, 1, 3, 1, 6, 10, 10, 10, 12, 10, 19, 1, 21, 1, 24, 6), ncol=2, byrow=TRUE)
S2 <- Mnn(X2, 2)
S2_expected <- matrix(c(1, 2, 0, 2, 3, 4, 4, 2, 3, 2, 6, 7, 5, 7, 6, 5), ncol=2, byrow=TRUE)
stopifnot(all(S2 == S2_expected))

G2 <- Mnn_graph(S2)
G2_expected <- matrix(c(
    0, 1, 1, 0, 0, 0, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 0,
    1, 1, 0, 1, 1, 0, 0, 0,
    0, 0, 1, 0, 1, 0, 0, 0,
    0, 0, 1, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 1,
    0, 0, 0, 0, 0, 1, 0, 1,
    0, 0, 0, 0, 0, 1, 1, 0), ncol=8, byrow=TRUE)
stopifnot(all(G2 == G2_expected))

D2 <- Mnn_graph_D_matrix(G2)
D2_expected <- matrix(c(
    2, 0, 0, 0, 0, 0, 0, 0,
    0, 2, 0, 0, 0, 0, 0, 0,
    0, 0, 4, 0, 0, 0, 0, 0,
    0, 0, 0, 2, 0, 0, 0, 0,
    0, 0, 0, 0, 2, 0, 0, 0,
    0, 0, 0, 0, 0, 2, 0, 0,
    0, 0, 0, 0, 0, 0, 2, 0,
    0, 0, 0, 0, 0, 0, 0, 2), ncol=8, byrow=TRUE)
stopifnot(all(D2 == D2_expected))

G2 <- Mnn_connect_graph(G2)
G2_expected <- matrix(c(
    0, 1, 1, 0, 0, 1, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 0,
    1, 1, 0, 1, 1, 0, 0, 0,
    0, 0, 1, 0, 1, 0, 0, 0,
    0, 0, 1, 1, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 1, 1,
    0, 0, 0, 0, 0, 1, 0, 1,
    0, 0, 0, 0, 0, 1, 1, 0), ncol=8, byrow=TRUE)
stopifnot(all(G2 == G2_expected))

L2 <- D2 - G2
E2 <- Laplacian_eigen(D2, G2, 4)
result2 <- kmeans(E2, 4)



distance <- function(x1, x2, y1, y2) {
    return (sqrt((x1 - x2)**2 + (y1 - y2)**2));
}

