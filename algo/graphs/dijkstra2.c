#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

typedef struct {
	int * weights;
	int * nodes; // nodes in minheap order
	int   size;
} heap_t;

#define LEFT_CHILD(i)  (((2*i) + 1))
#define RIGHT_CHILD(i) (((2*i) + 2))
#define PARENT(i)      (((i-1)/2))

void heapify(heap_t *heap) {
	// todo
}
int remove_min(heap_t *heap) {
	return 0;
}
int in_heap(heap_t *heap, int vertex) {
	return 0;
}

void printshorts(int *dist, int n, int s) {
	for (int i = 0; i < n; i++) {
		if (s != i) {
			printf("%d ", dist[i] == INT_MAX ? -1 : dist[i]);
		}
	}
	printf("\n");
}

void dijkstra(int32_t *GRAPH, int *dist, int32_t *prev, int n, int s) {
	int* nodes = calloc(n, sizeof(int));
	heap_t heap = { .weights = dist, .nodes = nodes, .size = n };

	for (int v = 0; v < n; v++) {
		dist[v] = INT_MAX;
		prev[v] = -1;
		nodes[v] = v;
	}
	dist[s] = 0;
	heapify(&heap);
	while (heap.size > 0) {
		int u = remove_min(&heap);
		int32_t* vs = GRAPH + (u-1)*n;
		for (int v = 0; v < n; v++) {
			if (*(vs + v) != -1 && in_heap(&heap, v)) {
				int alt = dist[u] == INT_MAX ? INT_MAX : dist[u] + vs[v];
				if (alt < dist[v]) {
					dist[v] = alt;
					prev[v] = u;
				}
			}
		}
	}
}

int main() {
	int t;
	scanf("%d",&t);
	for(int a0 = 0; a0 < t; a0++) {
		int n;
		int m;
		scanf("%d %d",&n,&m);
		int32_t *GRAPH = calloc(n * n, sizeof(int32_t));
		for (int i = 0; i < n*n; i++) {
			GRAPH[i] = -1;
		}
		for(int a1 = 0; a1 < m; a1++){
			int x;
			int y;
			int r;
			scanf("%d %d %d",&x,&y,&r);
			// nodes are labelled 1 to N
			x--;
			y--;
			GRAPH[x*n+y] = r;
			GRAPH[y*n+x] = r;
		}
		int s;
		scanf("%d",&s);
		s--;
		int *dist = calloc(n, sizeof(int));
		int32_t *prev = calloc(n, sizeof(int32_t));
		dijkstra(GRAPH, dist, prev, n, s);
		printshorts(dist, n, s);
		free(GRAPH);
		free(dist);
		free(prev);
	}
	return 0;
}
