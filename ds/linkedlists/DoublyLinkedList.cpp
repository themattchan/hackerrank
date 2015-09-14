
struct Node
{
	int data;
	Node *next;
	Node *prev;
};

Node* Reverse(Node *head)
{
	Node *tmp = NULL;

	/* swap next and prev for all nodes of
	   doubly linked list */
	while (head) {
		tmp = head->prev;
		head->prev = head->next;
		head->next = tmp;
		head = head->prev;
	}

	if (tmp)
		return tmp->prev;
	else
		return tmp;
}
