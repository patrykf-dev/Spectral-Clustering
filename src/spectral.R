setwd("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/Spectral-Clustering/src")
Rcpp::sourceCpp("spectral_aux.cpp", verbose=TRUE, rebuild=TRUE)
library(dendextend)
library(mclust)
library(ggplot2)
library(genie)

Laplacian_eigen <- function(D, G, k) {
    L <- D - G;
    E <- eigen(L, symmetric=TRUE)$vectors;
    return(E[,(k+1):2]);
}

Spectral_clustering_raw <- function(X, k, M) {
    S <- Mnn(X, M);
    G <- Mnn_graph(S);
    D <- Mnn_graph_D_matrix(G);
    G <- Mnn_connect_graph(G);
    E <- Laplacian_eigen(D, G, k);
    set.seed(100);
    return(kmeans(E, k));
}


Spectral_clustering_plot<- function(X, k, M) {
    result <- Spectral_clustering_raw(as.matrix(X), k, M);
    colors <- factor(result$cluster);
    if (ncol(X) == 2) {
        ggplot(X, aes(X[[1]], X[[2]])) + geom_point(aes(col=colors))+ 
            scale_fill_brewer(palette = "Set1");
    } else {
        ggplot(X, aes(X[[1]], X[[2]], X[[3]])) + geom_point(aes(col=colors))+ 
            scale_fill_brewer(palette = "Set1");
    }
}


Spectral_clustering <- function(data, k, M, plot){
    if(plot) {
        Spectral_clustering_plot(data, k, M);
    } else {
        result <- Spectral_clustering_raw(as.matrix(data), k, M);
        return(result$cluster);
    }
}
