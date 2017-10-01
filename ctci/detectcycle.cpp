/*
  Detect a cycle in a linked list. Note that the head pointer may be 'NULL' if the list is empty.

  A Node is defined as:

*/
struct Node {
  int data;
  struct Node* next;
};

bool has_cycle(Node* head) {
  if (head == nullptr) return false;
  if (head->next == nullptr) return false;
  if (head->next->next == nullptr) return false;

  Node *slow = head->next;
  Node *fast = head->next->next;
  while (slow != fast) {
    if (fast == nullptr) return false;
    slow = slow->next;
    if (slow != nullptr)
      fast = fast->next->next;
    else
      return false;
  }
  return true;
}
