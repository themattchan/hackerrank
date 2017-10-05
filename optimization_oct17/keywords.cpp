#include <bits/stdc++.h>
using namespace std;

#define MAXVAL (int)1e9

// TODO
int minimumLength(string text, vector<string> keys) {

  int answer = MAXVAL;
  text += " $";

  // scan text: for every starting position
  for (int i = 0; i < text.size(); i++) {
    vector<string> dup = keys;
    string word = "";
    // skip space
    if (i > 0 and text[i - 1] != ' ')
      continue;
    // accum word/check
    for (int j = i; j < text.size(); j++) {
      // on space
      if (text[j] == ' ') {
        // scan all remaining dups
        for (int k = 0; k < dup.size(); k++) {
          if (dup[k] == word) {
            dup.erase(dup.begin() + k);
          }
        }
        word = "";
      }
      else word += text[j];
      if ((int)dup.size() == 0) {
        answer = min(answer, j - i);
        break;
      }
    }
  }
  if (answer == MAXVAL)
    answer = -1;

  return answer;
}

int main() {

  string text, buf;
  vector <string> keys;
  getline(cin, text);
  int keyWords;
  cin >> keyWords;
  for (int i = 0; i < keyWords; i++) {
    cin >> buf;
    keys.push_back(buf);
  }
  cout << minimumLength(text, keys) << endl;

  return 0;
}
