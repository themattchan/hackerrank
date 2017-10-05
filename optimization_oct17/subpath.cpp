#include <bits/stdc++.h>

using namespace std;

bool dfs(vector<vector<int>>&g, int v, int p, int target, vector<int>& path) {
  if (v == target) {
    return true;
  }
  path.push_back(v);
  for (auto u : g[v]) {
    if (u == p) continue;
    bool found = dfs(g, u, v, target, path);
    if (found) return true;
  }
  path.pop_back();
  return false;
}

/*
 * Kadane's algorith: https://en.wikipedia.org/wiki/Maximum_subarray_problem
 */
int kadane(const vector<int>& a) {
  if (a.empty()) return 0;
  int max_ending_here = max(a[0], 0);
  int max_so_far = max_ending_here;
  for (int i = 1; i < a.size(); ++i) {
    max_ending_here = max(max(0, a[i]), max_ending_here+a[i]);
    max_so_far = max(max_so_far, max_ending_here);
  }
  return max_so_far;
}

vector <int> skippingSubpathSum(int n, vector<int> c,
                                vector<vector<int>> graph,
                                vector<pair<int, int>> queries) {
  vector<int> answers(queries.size());
  for (int qid = 0; qid < queries.size(); ++qid) {
    int u = queries[qid].first;
    int v = queries[qid].second;
    vector<int> path;
    dfs(graph, u, -1, v, path);
    vector<int> oddPath;
    vector<int> evenPath;
    for (int i = 0; i < path.size(); ++i) {
      if ((i+1) % 2 == 0) {
        evenPath.push_back(c[path[i]]);
      } else {
        oddPath.push_back(c[path[i]]);
      }
    }
    int s1 = kadane(evenPath);
    int s2 = kadane(oddPath);
    answers[qid] = max(s1, s2);
  }
  return answers;
}


int main() {
  int n;
  cin >> n;
  vector<int> c(n);
  for(int c_i = 0; c_i < n; c_i++){
    cin >> c[c_i];
  }
  vector<vector<int>> graph(n);
  for (int i = 0; i < n-1; ++i) {
    int u, v;
    cin >> u >> v;
    graph[u].push_back(v);
    graph[v].push_back(u);
  }
  int q;
  cin >> q;
  vector<pair<int, int>> queries;
  for (int i = 0; i < q; ++i) {
    int u, v;
    cin >> u >> v;
    queries.push_back(make_pair(u,v));
  }
  vector <int> answers = skippingSubpathSum(n, c, graph, queries);
  for (ssize_t i = 0; i < answers.size(); i++) {
    cout << answers[i] << (i != answers.size() - 1 ? "\n" : "");
  }
  cout << endl;


  return 0;
}
