setwd("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/Spectral-Clustering/src")
Rcpp::sourceCpp("spectral_aux.cpp", verbose=TRUE, rebuild=TRUE)

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
E2 <- eigen(L2)$vectors


