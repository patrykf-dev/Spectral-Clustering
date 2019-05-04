#include <cmath>
#include <queue>
#include <utility>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


////////////////////////////////////////////////////////
// 4.1 M-nearest neighbours ////////////////////////////
////////////////////////////////////////////////////////

class CompareDist
{
public:
    bool operator()(pair<int,double> n1, pair<int,double> n2) {
        return n1.second > n2.second;
    }
};

double EuclideanDistance(NumericVector point1, NumericVector point2);
IntegerVector GetNearestNeighbours(NumericMatrix points, int k, int pointIndex);


// [[Rcpp::export]]
IntegerMatrix Mnn(NumericMatrix points, int k)
{
	int n = points.nrow();
	IntegerMatrix rc(n, k);

    for(int i = 0; i < n; i++)
    {
    	IntegerVector result = GetNearestNeighbours(points, k, i);
        //Rcout << "Value for " << i << " is " << result(0) << " " << result(1) << " " << result(2) << std::endl;
    	rc.row(i) = result;
    }
	return rc;
}


IntegerVector GetNearestNeighbours(NumericMatrix points, int k, int pointIndex)
{
	int n = points.nrow();
	IntegerVector rc(k);

	priority_queue<pair<int, double>, vector<pair<int,int> >, CompareDist> queue;

	for(int i = 0; i < n; i++) 
	{
		if(i == pointIndex)
			continue;

		double distance = EuclideanDistance(points.row(i), points.row(pointIndex));
		queue.push(make_pair(i, distance));
	}

	for(int i = 0; i < k ; i++)
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
	for(int i = 0; i < d; i++)
		result += (point1(i) - point2(i)) * (point1(i) - point2(i));
	return sqrt(result);
}



////////////////////////////////////////////////////////
// 4.2 Adjacency matrix ////////////////////////////////
////////////////////////////////////////////////////////

// TODO...

