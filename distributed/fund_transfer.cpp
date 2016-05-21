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
/*
  Use this function to write data to socket
  void write_string_to_socket(int sock_descriptor, char* message, uint32_t length);

  Use this function to read data from socket
  void read_string_from_socket(int sock_descriptor, char** message, uint32_t *length);
*/
using namespace std;
typedef unordered_map<int, shared_ptr<unordered_set<int>>> Graph;

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
		dir1->insert(to);
		auto& dir2 = (*topology)[to];
		dir2->insert(from);
	}

	fclose(topo);
}

bool exists_path(int src, int dst, int hops)
{

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

			if (exists_path(src,dst,hops)) {
				message = "YES";
				message_length = 3;
			} else {
				message = "NO";
				message_length = 2;
			}

			write_string_to_socket(conn->sock, message, message_length);
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
