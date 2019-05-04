#include <stdio.h>
#include <stdlib.h>

struct Node {
  int city;
  struct Node *next;
};

typedef struct Node** Graph;

Graph make_graph(int n) {
  Graph graph = malloc(n * sizeof(struct Node *));
  for (int i=0;i<n;i++) graph[i]=NULL;
  return graph;
}

struct Node * link_node(int n, struct Node * node) {
  struct Node * new = malloc(sizeof(struct Node));
  new->city = n;
  new->next = node;
  return new;
}

void add_vertex(Graph g, int v, int w) {
  g[v] = link_node(w, g[v]);
  g[w] = link_node(v, g[w]);
}

void free_graph(Graph g, int siz) {
  for (int i = 0; i < siz; i++) {
    struct Node *cur = g[i];
    struct Node *next;
    while (cur) {
      next = cur->next;
      free(cur);
      cur = next;
    }
  }
  free(g);
}

void dfs(Graph cities, int *visit, int to_visit, int num_cities, int cc) {
  visit[to_visit] = cc;
  struct Node * adjs = cities[to_visit];
  while (adjs) {
    if (visit[adjs->city] == -1) dfs(cities, visit, adjs->city, num_cities, cc);
    adjs = adjs->next;
  }
}

int main()
{
  int q;
  scanf("%d\n",&q);
  while (q-- > 0) {
    int num_cities, cost_lib, cost_road;
    long long num_roads;
    scanf("%d %lld %d %d\n", &num_cities, &num_roads, &cost_lib, &cost_road);
    Graph G = make_graph(num_cities);
    //    printf("num_cities=%d   num_roads=%lld    cost_lib=%d    cost_road=%d\n",num_cities,num_roads,cost_lib,cost_road);

    for (long long i = 0; i < num_roads; i++) {
      int u,v;
      scanf("%d %d\n", &u, &v);
      add_vertex(G, u-1, v-1);
    }

    // If cost of building a library is cheaper than cost of building a road,
    // build a library at every city.
    if (cost_lib <= cost_road) {
      printf("%ld\n", (long)cost_lib * (long)num_cities);
    }
    // Otherwise, find number of connected components.
    // For each cc, build one library and n-1 roads.
    else {
      int visit[num_cities]; // fill in connected component number
      for (int i = 0; i < num_cities; i++) visit[i] = -1;

      int cc = 0;
      for (int i = 0; i < num_cities; i++) {
        if (visit[i] == -1) {
          dfs(G, visit, i, num_cities, cc++);
        }
      }

      int cc_count[cc];
      for (int i = 0; i < cc; i++) cc_count[i]=0;
      for (int i = 0; i < num_cities; i++) cc_count[visit[i]]++;

      long long ret = 0;
      for (int i = 0; i < cc; i++) {
        ret += cost_lib;
        ret += (cc_count[i]-1)*cost_road;
      }
      printf("%lld\n", ret);
    }

    free_graph(G, num_cities);
  }
}
