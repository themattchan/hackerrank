#include <iostream>
#include <string>
#include <stdio.h>
#include <algorithm>
#include <queue>

using namespace std;

typedef struct node
{
    int val;
    struct node *left;
    struct node *right;
    int ht;
} node;

bool isLeaf(node *n)
{
	return (!n->left && !n->right);
}

int balance_factor(node *root)
{
	return (root->left->ht) - (root->right->ht);
}

bool isBalanced(int balance_factor)
{
	return !(abs(balance_factor) > 1);
}

node* rotateR(node *n)
{
	node *pivot = n->left;
	n->left = pivot->right;
	pivot->right = n;
	return n;
}

node* rotateL(node *n)
{
	node *pivot = n->right;
	n->left = pivot->left;
	pivot->left = n;
	return n;
}

node* rotateLR(node *n)
{
	n->left = rotateL(n->left);
	return rotateR(n);
}

node* rotateRL(node *n)
{
	n->right = rotateR(n->right);
	return rotateL(n);
}


node* insert(node *root, int value)
{
	node* ins = new node();
	ins->val = value;
	ins->ht = 1;
	ins->left = NULL;
	ins->right = NULL;

	if (root == NULL) {
		return ins;
	}

	if (root->val < value) {
		root->left = insert(root->left, value);

		if (root->left->ht - root->right->ht == 2) {
			if (value < root->left->val)
				root = rotateR(root);
			else
				root = rotateLR(root);
		}

		root->ht = max(root->left->ht, root->right->ht) +1;
	} else {
		root->right = insert(root->right, value);

		if (root->left->ht - root->right->ht == -2) {
			if (value < root->right->val)
				root = rotateL(root);
			else
				root = rotateRL(root);
		}

		root->ht = max(root->left->ht, root->right->ht) +1;
	}

   return root;
}
