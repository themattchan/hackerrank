#include <iostream>
#include <vector>
#define foreach(var,until) for(int var = 0; var < until; var++)

using namespace std;

int withVectors()
{
	int N,Q;
	cin >> N >> Q;
	vector<vector<int>> v(N);

	foreach (i, N) {
		int k;
		cin >> k;
		v[i].resize(k);
		foreach (j, k) {
			cin >> v[i][j];
		}
	}

	foreach (i, Q) {
		int a,b;
		cin >> a >> b;
		cout << v[a][b] << endl;
	}

	return 0;
}

int withMalloc()
{
	int N,Q;
	cin >> N >> Q;
	int** v = new int*[N];

	foreach (i, N) {
		int k;
		cin >> k;
		int* vi = new int[k];
		foreach (j, k) {
			cin >> vi[j];
		}
		v[i] = vi;
	}

	foreach (i, Q) {
		int a,b;
		cin >> a >> b;
		cout << v[a][b] << endl;
	}

	foreach (i, N) {
		delete[] v[i];
	}
	delete[] v;

	return 0;
}

int main()
{
	withVectors();
	withMalloc();
	return 0;
}
