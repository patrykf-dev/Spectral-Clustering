setwd("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/Spectral-Clustering/src")
Rcpp::sourceCpp("spectral_aux.cpp", verbose=TRUE, rebuild=TRUE)


X1 <- matrix(c(0, 10, 1, 10, 3, 10, 6, 10, 14, 2, 16, 2, 19, 2), ncol=2, byrow=TRUE)
X2 <- matrix(c(1, 1, 3, 1, 6, 10, 10, 10, 12, 10, 19, 1, 21, 1, 24, 6), ncol=2, byrow=TRUE)

Kmeans_division <- function(X, k) {
    #https://stackoverflow.com/questions/28273012/kmeans-gives-wrong-cluster-in-r
    Xscaled = X;
    Xscaled[,1] = scale(Xscaled[,1]);
    Xscaled[,2] = scale(Xscaled[,2]);
    clusterResult <- kmeans(Xscaled, k);
    plot(Xscaled, col=clusterResult$cluster);
    text(Xscaled, labels=clusterResult$cluster, cex=0.7, pos=2);
}


Laplacian_eigen <- function(D, G, k) {
    L <- D - G;
    E <- eigen(L, symmetric=TRUE)$vectors;
    return(E[,(k+1):2]);
}

Spectral_clustering <- function(X, k, M) {
    S <- Mnn(X, M);
    G <- Mnn_graph(S);
    D <- Mnn_graph_D_matrix(G);
    G <- Mnn_connect_graph(G);
    E <- Laplacian_eigen(D, G, k);
    return(kmeans(E, k));
}

Spectral_clustering_plot<- function(X, k, M) {
    result <- Spectral_clustering(X, k, M);
    plot(X, col=result$cluster);
    text(X, labels=result$cluster, cex=0.7, pos=2);
}
