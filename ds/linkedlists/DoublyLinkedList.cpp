#include <stdio.h>

struct Node
{
	int data;
	Node *next;
	Node *prev;
};


Node* SortedInsert(Node *head, int data)
{
	Node *n = new Node();
	n->data = data;

	if (head == NULL)
		return n;

	// special case: new node is new head
	if (head->data >= data) {
		n->next = head;
		head->prev = n;
		return n;
	}

	// is there a way to do this with one pointer?
	Node *c = head;
	Node *p = NULL;
	while (c && c->data < data) {
		p = c;
		c = c->next;
	}

	n->next = c;
	n->prev = p;
	p->next = n;
	if (c)
		c->prev = n;

	return head;
}


Node* Reverse(Node *head)
{
	Node *tmp;

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
