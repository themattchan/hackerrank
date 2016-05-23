#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <errno.h>
#include <sys/socket.h>
#include <resolv.h>
#include <unistd.h>

#include <iostream>
#include <fstream>
#include <stack>

/*
  Use this function to write data to socket
  void write_string_to_socket(int sock_descriptor, char* message, uint32_t length);

  Use this function to read data from socket
  void read_string_from_socket(int sock_descriptor, char** message, uint32_t *length);
*/
using namespace std;

int* nodes;

/*
  This function is called only once before any client connection is accepted by
  the server. Read any global datasets or configurations here.
*/
bool is_root(int node)
{
	return nodes[node] == -1;
}

void init_server()
{
	FILE *topo = fopen("training.txt", "r");

	int N;
	fscanf(topo, "%d", &N);

	nodes = (int*) malloc((N+1) * sizeof(int));
	for (int i = 1; i <= N; i++) {
		nodes[i] = -1;
	}

	for (int i = 0; i < N; i++) {
		int parent, child;
		char comma;
		fscanf(topo, "%d%c%d", &parent, &comma, &child);
		nodes[child] = parent;
	}

	fclose(topo);
}

int path_len(int src, int dst)
{
	int path_len = 1;
	if (src == dst) return path_len;

	int src_node = src;
	int dst_node = dst;

	stack<int> src_path;
	stack<int> dst_path;

	if (is_root(src_node))
		src_path.push(src_node);
	while (!is_root(src_node)) {
		src_path.push(src_node);
		src_node = nodes[src_node];
	}

	if (is_root(dst_node))
		dst_path.push(dst_node);
	while (!is_root(dst_node)) {
		dst_path.push(dst_node);
		dst_node = nodes[dst_node];
	}

	while (!src_path.empty() && !dst_path.empty() &&
		   src_path.top() == dst_path.top()) {
		src_path.pop();
		dst_path.pop();
	}

	path_len += src_path.size() + dst_path.size();

	return path_len;
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
		char comma;

		/* read message */
		read_string_from_socket(conn->sock, &message, &message_length);
		printf("Received = %s\n", message);

		if (strcmp(message, "END") != 0) {
			sscanf(message,"%d%c%d%c%d", &src, &comma, &dst, &comma, &hops);

			bool path = path_len(src,dst) <= hops;
			const char *reply = path ? "YES" : "NO";
			uint32_t reply_len = path ? 3 : 2;

			write_string_to_socket(conn->sock, reply, reply_len);
		} else {
			write_string_to_socket(conn->sock, message, message_length);
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
