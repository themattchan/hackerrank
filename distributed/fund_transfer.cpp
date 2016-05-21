#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <errno.h>
#include <sys/socket.h>
#include <resolv.h>
#include <unistd.h>

#include <iostream>
#include <fstream>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <queue>

/*
  Use this function to write data to socket
  void write_string_to_socket(int sock_descriptor, char* message, uint32_t length);

  Use this function to read data from socket
  void read_string_from_socket(int sock_descriptor, char** message, uint32_t *length);
*/
using namespace std;

struct Node {
	int id;
	int parent;
	int dist;
};
typedef struct Node Node;

typedef unordered_map<int, unordered_set<int>> Graph;

unique_ptr<Graph> topology;

/*
  This function is called only once before any client connection is accepted by
  the server. Read any global datasets or configurations here.
*/

void init_server()
{
	topology = unique_ptr<Graph>(new Graph());

	FILE *topo = fopen("training.txt", "r");
	int size;
	fscanf(topo, "%d", &size);
	for (int i = 0; i < size; i++) {
		int from, to;
		fscanf(topo, "%d,%d", &from, &to);
		auto& dir1 = (*topology)[from];
		dir1.insert(to);
		auto& dir2 = (*topology)[to];
		dir2.insert(from);
	}

	fclose(topo);
}

bool exists_path(int src, int dst, int hops)
{
	if (src == dst) return true;

	unordered_map<int,Node> seens;
	queue<Node> Q;
	Node root = { .id = src, .parent = src, .dist = 1 };
	Q.push(root);
	while (!Q.empty()) {
		Node cur = Q.front();
		seens[cur.id] = cur;
		Q.pop();
		auto adjs = (*topology)[cur.id];
		for (auto it = adjs.begin(); it != adjs.end(); it++) {
			if (seens.find(*it) == seens.end()) {
				Node n = { .id = *it, .parent = cur.id, .dist = cur.dist+1 };
				if (n.id == dst) {
					if (n.dist <= hops) return true;
					else return false;
				}

				Q.push(n);
			}
		}
	}
	return false;
}

/*
  Write your code here
  This function is called everytime a new connection is accepted by the server
*/
void * process_client_connection(void * ptr)
{
	connection_t * conn;

	if (!ptr) pthread_exit(0);
	conn = (connection_t *)ptr;

	printf("Connection received\n");
	fflush(stdout);

	int terminate_client = 0;
	do
	{
		/* read length of message */
		char *message = NULL;
		uint32_t message_length = 0;

		int src,dst,hops;

		/* read message */
		read_string_from_socket(conn->sock, &message, &message_length);

		if (strcmp(message, "END") != 0) {
			sscanf(message,"%d,%d,%d", &src, &dst, &hops);

			printf("Received = %s\n", message);

			bool path = exists_path(src,dst,hops);
		    const char *reply = path ? "YES" : "NO";
			int reply_len = path? 3:2;

			write_string_to_socket(conn->sock, reply, reply_len);

		} else {
			terminate_client = 1;
		}

		free(message);

	} while(!terminate_client);

	/* close socket and clean up */
	printf("Closing client on socket %d\n", conn->sock);
	fflush(stdout);
	close(conn->sock);
	free(conn);
	pthread_exit(0);
}
