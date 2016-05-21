#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

class Matrix {
public:
	vector<vector<int>>a;

	Matrix(): a(vector<vector<int>>()) {
	};

	Matrix operator +(Matrix& that)
	{
		Matrix ret = Matrix();
		int cols = this->a[0].size();
		int rows = this->a.size();
		for (int i = 0; i < rows; i++) {
			vector < int >nr;
			for (int j = 0; j < cols; j++) {
				nr.push_back(this->a[i][j] + that.a[i][j]);
			}
			ret.a.push_back(nr);
		}
		return ret;
	}
};

int main()
{
	int cases, k;
	cin >> cases;
	for (k = 0; k < cases; k++) {
		Matrix x;
		Matrix y;
		Matrix result;
		int n, m, i, j;
		cin >> n >> m;
		for (i = 0; i < n; i++) {
			vector < int >b;
			int num;
			for (j = 0; j < m; j++) {
				cin >> num;
				b.push_back(num);
			}
			x.a.push_back(b);
		}
		for (i = 0; i < n; i++) {
			vector < int >b;
			int num;
			for (j = 0; j < m; j++) {
				cin >> num;
				b.push_back(num);
			}
			y.a.push_back(b);
		}
		result = x + y;
		for (i = 0; i < n; i++) {
			for (j = 0; j < m; j++) {
				cout << result.a[i][j] << " ";
			}
			cout << endl;
		}
	}
	return 0;
}
