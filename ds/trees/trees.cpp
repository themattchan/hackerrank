#include <stdio.h>
#include <algorithm>

struct node
{
    int data;
    node* left;
    node* right;
};

bool isLeaf(node *n) {
	return (!n->left && !n->right);
}

void Preorder(node *root) {
	printf("%d ", root->data);

	if (isLeaf(root)) return;

	if (root->left)
		Preorder(root->left);
	if (root->right)
		Preorder(root->right);
}

void Postorder(node *root) {

	if (root->left)
		Postorder(root->left);
	if (root->right)
		Postorder(root->right);

	printf("%d ", root->data);
	if (isLeaf(root)) return;
}


void Inorder(node *root) {
	if (root->left)
		Inorder(root->left);

	printf("%d ", root->data);

	if (root->right)
		Inorder(root->right);

	if (isLeaf(root)) return;
}

int height(node * root)
{
	if (isLeaf(root))
		return 1;

	int l = 0;
	if (root->left)
		l = height(root->left);

	int r = 0;
	if (root->right)
		r = height(root->right);

	return std::max(l+1,r+1);
}
