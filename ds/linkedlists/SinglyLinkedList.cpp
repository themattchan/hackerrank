#include <stdio.h>

struct Node
{
	int data;
	struct Node *next;
};

void Print(Node *head)
{
	if (head) {
		printf ("%d\n", head->data);
		Print(head->next);
	}
}

Node* InsertTail(Node *head,int data)
{
	Node *qn = head;
	Node *nn = new Node();
	nn->data = data;
	nn->next = NULL;
	if (qn == NULL) {
		return nn;
	}
	while (qn->next != NULL) {
		qn = qn->next;
	}
	qn->next = nn;
	return head;

}

Node* InsertHead(Node *head,int data)
{
	Node *nn = new Node();
	nn->data = data;
	nn->next = head;
	return nn;
}

// Fuck loop indices, use recursion
Node* InsertNth(Node *head, int data, int position)
{
	Node *nn = new Node();
	nn->data = data;
	nn->next = NULL;

	if (head == NULL) {
		return nn;
	}
	if (position == 0) {
		nn->next = head;
		return nn;
	}

	head->next = InsertNth(head->next, data, position-1);
	return head;
}

Node* Delete(Node *head, int position)
{
	if (position == 0) {
		return head->next;
	} else {
		head->next = Delete(head->next, position -1);
		return head;
	}
}

void ReversePrint(Node *head)
{
	if (head) {
		ReversePrint(head->next);
		printf("%d\n", head->data);
	}
}

int CompareLists(Node *headA, Node* headB)
{
	if (headA == NULL && headB == NULL) {
		return 1;
	} else if ((headA == NULL && headB != NULL) ||
			   (headA != NULL && headB == NULL)) {
		return 0;
	} else if (headA->data == headB->data) {
		return	CompareLists(headA->next, headB->next);
	} else {
		return 0;
	}
}

Node* MergeLists(Node *headA, Node* headB)
{
	if (headA == NULL) {
		return headB;
	}
	if (headB == NULL) {
		return headA;
	}
	if (headA->data < headB->data) {
		headA->next = MergeLists(headA->next,headB);
		return headA;
	} else {
		headB->next = MergeLists(headA,headB->next);
		return headB;
	}
}

int GetNode(Node *head,int positionFromTail)
{
	Node *n = head;
	int len = 0;
	while (n->next) {
		len++;
		n = n->next;
	}

	len -= positionFromTail;

	n = head;

	while(len--) {
		n = n->next;
	}
	return n->data;
}

Node* RemoveDuplicates(Node *head)
{

}
