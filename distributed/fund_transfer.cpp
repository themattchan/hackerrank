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
#include <stack>

/*
  Use this function to write data to socket
  void write_string_to_socket(int sock_descriptor, char* message, uint32_t length);

  Use this function to read data from socket
  void read_string_from_socket(int sock_descriptor, char** message, uint32_t *length);
*/
using namespace std;

struct Node {
	int id;
	Node* parent;
};
typedef struct Node Node;

typedef unordered_map<int, unordered_set<int>> Graph;

unique_ptr<unordered_map<int,Node*>> topology;

/*
  This function is called only once before any client connection is accepted by
  the server. Read any global datasets or configurations here.
*/
bool is_root(Node* node)
{
	return node->parent == nullptr;
}

void init_server()
{
	topology = unique_ptr<unordered_map<int,Node*>>();

	FILE *topo = fopen("training.txt", "r");
	int N;
	fscanf(topo, "%d", &N);
	for (int i = 1; i <= N; i++) {
		Node *node = new Node();
		node->id = i;
		node->parent = nullptr;
	}

	for (int i = 0; i < N; i++) {
		int parent, child;
		fscanf(topo, "%d,%d", &parent, &child);
		auto& child_node = (*topology)[child];
		auto& parent_node = (*topology)[parent];
		child_node->parent = parent_node;
	}

	fclose(topo);
}

bool exists_path(int src, int dst, int hops)
{
	Node* src_node = (*topology)[src];
	Node* dst_node = (*topology)[dst];

	stack<Node**> src_path;
	stack<Node**> dst_path;

	while (!is_root(src_node)) {
		src_path.push(&src_node);
		src_node = src_node->parent;
	}
	while (!is_root(dst_node)) {
		dst_path.push(&dst_node);
		dst_node = dst_node->parent;
	}
	while (src_path.top() == dst_path.top()) {
		src_path.pop();
		dst_path.pop();
	}

	int path_len = src_path.size() + dst_path.size() +1;
	return hops <= path_len;
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
