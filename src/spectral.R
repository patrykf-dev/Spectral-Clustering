setwd("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/Spectral-Clustering/src")
Rcpp::sourceCpp("spectral_aux.cpp", verbose=TRUE, rebuild=TRUE)
X1 <- matrix(c(0, 10, 1, 10, 3, 10, 6, 10, 14, 2, 16, 2, 19, 2), ncol=2, byrow=TRUE)
X2 <- matrix(c(1, 1, 3, 1, 6, 10, 10, 10, 12, 10, 19, 1, 21, 1, 24, 6), ncol=2, byrow=TRUE)


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
        ggplot(X, aes(X[[1]], X[[2]])) + geom_point(aes(col=colors));
    } else {
        ggplot(X, aes(X[[1]], X[[2]], X[[3]])) + geom_point(aes(col=colors));
    }
}


Spectral_clustering <- function(name, k, M){
    setwd(paste0("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe/fcps"));
    data <- read.table(paste0(name, ".data.gz"));
    labels <- as.integer(read.table(paste0(name, ".labels0.gz"))[,1]);
    Spectral_clustering_plot(data, k, M);
}


# CHECKING SETS DIMENSIONS
Test_set_dimension <- function(name) {
    data <- as.matrix(read.table(paste0(name, ".data.gz")));
    labels <- as.matrix(read.table(paste0(name, ".labels0.gz"))[,1]);
    print(paste("Data ", name, " is ", ncol(data), "x", nrow(data), " and labels is ", ncol(labels), "x", nrow(labels)))
}
names <- c("atom", "chainlink", "engytime", "lsun", "twodiamonds", "wingnut")
asd <- lapply(names, Test_set)
