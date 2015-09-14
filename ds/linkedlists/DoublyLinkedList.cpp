#include <stdio.h>

struct Node
{
	int data;
	Node *next;
	Node *prev;
};
/*
Node* SortedInsert(Node *head,int data)
{
	Node *n = new Node();
	n->data = data;
	n->next = NULL;
	n->prev = NULL;

	if (head == NULL) {
		return n;
	}

	Node *c = head;
	while (c->data < data && c->next) {
		c = c->next;
	}
	if (c->next) {
		Node *d = c->next;
		c->next = n;
		d->prev = n;
		n->prev = c;
		n->next = d;
	} else {
		c->next = n;
		n->prev = c;
	}
	return head;
}
*/

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
