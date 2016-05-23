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

// All global declarations go here
using namespace std;

struct Node {
	int transmit_prob;
	int parent;
};
Node** nodes;

/*
This function is called only once before any client connection is accepted by the server.
Read any global datasets or configurations here
*/
void init_server()
{
	FILE *topo = fopen("training.txt", "r");

	int N;
	fscanf(topo, "%d", &N);

	nodes = (Node**) malloc((N+1) * sizeof(Node*));
	for (int i = 1; i <= N; i++) {
		Node* n = new Node();
		n->parent = -1;
		nodes[i] = n;
	}

	for (int i = 0; i < N; i++) {
		int parent, child, transmit_prob;
		char comma;
		fscanf(topo, "%d%c%d%c%d", &parent, &comma, &child, &comma, &transmit_prob);
		nodes[child]->parent = parent;
	}

	fclose(topo);
}

bool is_root(int node)
{
	return nodes[node]->parent == -1;
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

		int src,dst;
		double threshold;
		char comma;

       	/* read message */
		read_string_from_socket(conn->sock, &message, &message_length);
		printf("Received = %s\n", message);

		if (strcmp(message, "END") != 0) {
			sscanf(message,"%d%c%d%c%lf", &src, &comma, &dst, &comma, &threshold);

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
