#include <cmath>
#include <queue>
#include <algorithm>
#include <utility>
#include <list>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


////////////////////////////////////////////////////////
// 4.1 M-nearest neighbours ////////////////////////////
////////////////////////////////////////////////////////
double EuclideanDistance(NumericVector point1, NumericVector point2);
IntegerVector GetNearestNeighbours(NumericMatrix points, int k, int pointIndex);
class CompareDist
{
public:
	bool operator()(pair<int, double> n1, pair<int, double> n2) {
		return n1.second > n2.second;
	}
};


// [[Rcpp::export]]
IntegerMatrix Mnn(NumericMatrix points, int k)
{
	int n = points.nrow();
	IntegerMatrix rc(n, k);

	for (int i = 0; i < n; i++)
	{
		IntegerVector result = GetNearestNeighbours(points, k, i);
		//Rcout << "Value for " << i << " is " << result(0) << " " << result(1) << " " << result(2) << endl;
		rc.row(i) = result;
	}
	return rc;
}


IntegerVector GetNearestNeighbours(NumericMatrix points, int k, int pointIndex)
{
	int n = points.nrow();
	IntegerVector rc(k);

	priority_queue<pair<int, double>, vector<pair<int, int> >, CompareDist> queue;

	for (int i = 0; i < n; i++)
	{
		if (i == pointIndex)
			continue;

		double distance = EuclideanDistance(points.row(i), points.row(pointIndex));
		queue.push(make_pair(i, distance));
	}

	for (int i = 0; i < k ; i++)
	{
		rc(i) = queue.top().first;
		queue.pop();
	}
	return rc;
}


double EuclideanDistance(NumericVector point1, NumericVector point2)
{
	int d = point1.size();
	double result = 0;
	for (int i = 0; i < d; i++)
		result += (point1(i) - point2(i)) * (point1(i) - point2(i));
	return sqrt(result);
}



////////////////////////////////////////////////////////
// 4.2 Adjacency matrix ////////////////////////////////
////////////////////////////////////////////////////////
bool VectorContains(IntegerVector vector, int element);
int VectorSum(IntegerVector vector);


class AdjacencyMatrixGraph
{
	int n;
	IntegerMatrix adjacencyMatrix;
public:
	list<int> ConnectedComponentsRepresentatives()
	{
		bool *visited = new bool[n];
		for (int v = 0; v < n; v++)
		{
			visited[v] = false;
		}

		list<int> rc = list<int>();
		for (int v = 0; v < n; v++)
		{
			if (!visited[v])
			{
				rc.push_back(v);
				DFSRun(v, visited);
			}
		}
		return rc;
	}

	void DFSRun(int v, bool visited[])
	{
		visited[v] = true;
		for (int i = 0; i < n; i++)
		{
			if (!visited[i] && adjacencyMatrix(i, v) == 1)
				DFSRun(i, visited);
		}
	}

	AdjacencyMatrixGraph(IntegerMatrix adjacencyMatrix)
	{
		int n = adjacencyMatrix.nrow();
		this->n = n;
		this->adjacencyMatrix = adjacencyMatrix;
	}
};


// [[Rcpp::export]]
IntegerMatrix Mnn_graph(IntegerMatrix neighbours)
{
	int n = neighbours.nrow();
	IntegerMatrix rc = IntegerMatrix(n, n);

	for (int i = 0; i < n; i++)
	{
		for (int j = i; j < n; j++)
		{
			if (i == j)
			{
				rc(i, j) = 0;
				continue;
			}

			int adjacent = 0;
			if (VectorContains(neighbours.row(i), j))
				adjacent = 1;
			else if (VectorContains(neighbours.row(j), i))
				adjacent = 1;
			else
				adjacent = 0;
			//Rcout << "Adjacent flag for [" << i << ", " << j << "] is " << adjacent << endl;

			rc(i, j) = adjacent;
			rc(j, i) = adjacent;
		}
	}
	return rc;
}

// [[Rcpp::export]]
IntegerMatrix Mnn_graph_D_matrix(IntegerMatrix graphMatrix)
{
	int n = graphMatrix.nrow();
	IntegerMatrix rc = IntegerMatrix(n, n);

	for(int i = 0; i < n; i++)
	{
		rc(i, i) = VectorSum(graphMatrix.row(i));
	}

	return rc;
}


// [[Rcpp::export]]
IntegerMatrix Mnn_connect_graph(IntegerMatrix graphMatrix)
{
	AdjacencyMatrixGraph graph = AdjacencyMatrixGraph(graphMatrix);
	list<int> verticesToConnect = graph.ConnectedComponentsRepresentatives();

	list<int>::iterator it = verticesToConnect.begin();
	int oneVertex = *it;
	it++;

	while (it != verticesToConnect.end())
	{
		int vertex = *it;
		Rcout << "(" <<  oneVertex << ", "<< vertex << ") ";
		graphMatrix(oneVertex, vertex) = 1;
		graphMatrix(vertex, oneVertex) = 1;
		it++;
	}
	return graphMatrix;
}


bool VectorContains(IntegerVector vector, int element)
{
	return (find(vector.begin(), vector.end(), element) != vector.end());
}

int VectorSum(IntegerVector vector)
{
	int sum = 0;
	for(IntegerVector::iterator it = vector.begin(); it != vector.end(); it++)
	{
		sum += *it;
	}
	return sum;
}
