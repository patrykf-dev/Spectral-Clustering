#include <iostream>
#include <cmath>
#include<queue>
#include<utility>
using namespace std;

int** mNearestNeighbours(double** points, int n, int d, int k);
int* getNearestNeighbours(double** points, int n, int d, int k, int pointIndex);
double euclideanDistance(double* point1, double* point2, int d);


class CompareDist
{
public:
    bool operator()(pair<int,double> n1,pair<int,double> n2) {
        return n1.second > n2.second;
    }
};

int main() 
{
	double** points = new double*[7];
	for(int i = 0; i < 7; i++)
    	points[i] = new double[2];

    points[0][0] = 0;
    points[0][1] = 10;

    points[1][0] = 1;
    points[1][1] = 10;

    points[2][0] = 3;
    points[2][1] = 10;

    points[3][0] = 6;
    points[3][1] = 10;

    points[4][0] = 14;
    points[4][1] = 2;

    points[5][0] = 16;
    points[5][1] = 2;

    points[6][0] = 19;
    points[6][1] = 2;



    int** result = mNearestNeighbours(points, 7, 2, 3);

    for(int j = 0; j < 7; j++)
    {
	    printf("[%d] nearest : %d %d %d\n", j, result[j][0], result[j][1], result[j][2]);
    }

    return 0;
}




int** mNearestNeighbours(double** points, int n, int d, int k)
{
	int** rc = new int*[n];

    for(int i = 0; i < n; i++)
    {
    	int* result = getNearestNeighbours(points, n, d, k, i);
    	rc[i] = result;
    }


	return rc;
}


int* getNearestNeighbours(double** points, int n, int d, int k, int pointIndex)
{
	int* rc = new int[k];

	priority_queue<pair<int, double>, vector<pair<int,int>>, CompareDist> queue;

	for(int i = 0; i < n; i++) 
	{
		if(i == pointIndex)
			continue;

		double distance = euclideanDistance(points[i], points[pointIndex], d);
		queue.push(make_pair(i, distance));
	}

	for(int i = 0; i < k ; i++)
	{
		rc[i] = queue.top().first;
		queue.pop();
	}


	return rc;
}

double euclideanDistance(double* point1, double* point2, int d)
{
	double result = 0;
	for(int i = 0; i < d; i++)
		result += (point1[i] - point2[i]) * (point1[i] - point2[i]);
	return sqrt(result);
}