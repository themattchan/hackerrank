#include <iostream>
#include <string>
#include <stdio.h>
#include <algorithm>
#include <queue>

using namespace std;

struct node
{
    int data;
    node *left;
    node *right;
};

bool isLeaf(node *n)
{
	return (!n->left && !n->right);
}

void Preorder(node *root)
{
	printf("%d ", root->data);

	if (isLeaf(root)) return;

	if (root->left)
		Preorder(root->left);
	if (root->right)
		Preorder(root->right);
}

void Postorder(node *root)
{
	if (root->left)
		Postorder(root->left);
	if (root->right)
		Postorder(root->right);

	printf("%d ", root->data);
	if (isLeaf(root)) return;
}

void Inorder(node *root)
{
	if (root->left)
		Inorder(root->left);

	printf("%d ", root->data);

	if (root->right)
		Inorder(root->right);

	if (isLeaf(root)) return;
}

int height(node *root)
{
	if (isLeaf(root))
		return 1;

	int l = 0;
	if (root->left)
		l = height(root->left);

	int r = 0;
	if (root->right)
		r = height(root->right);

	return max(l+1, r+1);
}

void go_left(node *root) {
	if (root->left)
		go_left(root->left);
	printf("%d ", root->data);
}

void top_view(node *root)
{
	go_left(root);

	// go right
	node *r = root->right;
	while (r) {
		printf("%d ", r->data);
		r = r->right;
	}
}

void LevelOrder(node *root)
{
	queue<node*> q;
	q.push(root);
	while (!q.empty()) {
		node *n = q.front();
		q.pop();
		printf("%d ", n->data);
		if (n->left)
			q.push(n->left);
		if (n->right)
			q.push(n->right);
	}
}


node* insert(node *root, int value)
{
	node *n = root;

	node* ins = new node();
	ins->data = value;
	ins->left = NULL;
	ins->right = NULL;

	if (root == NULL) {
		return ins;
	}

	// yuck
	while (true) {
		if (n->data > value) {
			if (n->left) {
				n = n->left;
			} else {
				n->left = ins;
				return root;
			}
		} else {
			if (n->right) {
				n = n->right;
			} else {
				n->right = ins;
				return root;
			}
		}
	}

   return root;
}

/*
typedef struct node
{
	int freq;
	char data;
	node *left;
	node *right;
} node;
*/
void decode_huff(node *root, string s)
{
	node *n = root;
	for (int i = 0; i < s.size(); i++) {
		if (s[i] == '0')
			n = n->left;
		else
			n = n->right;

		if (isLeaf(n)) {
			cout << n->data;
			n = root;
		}
	}
}

node* lca(node *root, int v1, int v2)
{
	if (root == NULL)
		return root;

	int d = root->data;

	if (d == v1 || d == v2)
		return root;

	else if ((v1 < d && v2 > d) || (v1 > d && v2 < d))
		return root;

	else if (v1 < d && v2 < d)
		return lca(root->left, v1, v2);

	else
		return lca(root->right, v1, v2);
}


int maxL(node *node) {
  while (node->right) node = node->right;
  return node->data;
}

int minR(node *node) {
  while (node->left) node = node->left;
  return node->data;
}

bool checkBST(node* root) {
  return (root->left ? (root->data > maxL(root->left)) && checkBST(root->left) : true)
      && (root->right ? (root->data < minR(root->right)) && checkBST(root->right) : true);
	}
