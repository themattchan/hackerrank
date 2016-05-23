#include <string.h>
#include <stdlib.h>

#include <errno.h>
#include <sys/socket.h>
#include <resolv.h>
#include <unistd.h>

#include <cmath>
#include <iostream>
#include <fstream>
#include <stack>

/*
Use this function to write data to socket
void write_string_to_socket(int sock_descriptor, char* message, uint32_t length);

Use this function to read data from socket
void read_string_from_socket(int sock_descriptor, char** message, uint32_t *length);
*/

// All global declarations go here
using namespace std;

struct Node {
	int transmit_prob;
	int parent;
};
Node* nodes;

/*
This function is called only once before any client connection is accepted by the server.
Read any global datasets or configurations here
*/
void init_server()
{
	FILE *topo = fopen("training.txt", "r");

	int N;
	fscanf(topo, "%d", &N);

	nodes = (Node*) malloc((N+1) * sizeof(Node));
	for (int i = 1; i <= N; i++) {
		nodes[i].parent = -1;
		nodes[i].transmit_prob = 1;
	}

	for (int i = 0; i < N; i++) {
		int parent, child, transmit_prob;
		char comma;
		fscanf(topo, "%d%c%d%c%d", &parent, &comma, &child, &comma, &transmit_prob);
		nodes[child].parent = parent;
		nodes[child].transmit_prob = transmit_prob;
	}

	fclose(topo);
}

bool is_root(int node)
{
	return nodes[node].parent == -1;
}

int ipow(int base, int exp)
{
    int result = 1;
    while (exp)
    {
        if (exp & 1)
            result *= base;
        exp >>= 1;
        base *= base;
    }

    return result;
}

bool success_prob(int src, int dst, double& threshold)
{
	if (src == dst) return 1.00;

	int src_node = src;
	int dst_node = dst;

	stack<int> src_path;
	stack<int> dst_path;

	while (!is_root(src_node)) {
		src_path.push(src_node);
		src_node = nodes[src_node].parent;
	}

	while (!is_root(dst_node)) {
		dst_path.push(dst_node);
		dst_node = nodes[dst_node].parent;
	}

	while (!src_path.empty() && !dst_path.empty() &&
		   src_path.top() == dst_path.top()) {
		src_path.pop();
		dst_path.pop();
	}

	int exp = 2 * (src_path.size() + dst_path.size());

	int prob = 1;
	while (src_path.size() > 0) {
		int n = src_path.top();
		src_path.pop();
		prob *= nodes[n].transmit_prob;
		while ((prob % 10 == 0) && (prob / 10) > 0) {
			prob /= 10;
			exp--;
		}
	}
	while (dst_path.size() > 0) {
		int n = dst_path.top();
		dst_path.pop();
		prob *= nodes[n].transmit_prob;
		while ((prob % 10 == 0) && (prob / 10) > 0) {
			prob /= 10;
			exp--;
		}
	}

	return (((double)prob / (double)ipow(10,exp)) >= pow(10.0, threshold));
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
	do {
		/* read length of message */
		char *message = NULL;
		uint32_t message_length = 0;

		int src,dst;
		double threshold;
		char comma;

		/* read message */
		read_string_from_socket(conn->sock, &message, &message_length);
		printf("Received = %s\n", message);

		if (strcmp(message, "END") != 0) {
			sscanf(message,"%d%c%d%c%lf", &src, &comma, &dst, &comma, &threshold);

			bool success = success_prob(src,dst,threshold);
			const char *reply = success ? "YES" : "NO";
			uint32_t reply_len = success ? 3 : 2;

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
