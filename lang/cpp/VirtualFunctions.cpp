#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

// virtual == "non-final"
class Person
{
public:
	virtual void getdata() {
		cin >> name;
		cin >> age;
	}
	virtual void putdata() {
		cout << name << " " << age << " ";
	}

private:
	string name;
	int age;
};

class Professor : public Person
{
public:
	Professor() : cur_id(id++) {}

	void getdata() {
		Person::getdata();
		cin >> publications;
	}

	void putdata() {
		Person::putdata();
		cout << publications << " " << cur_id << endl;
	}

private:
	static int id;
	int publications,cur_id;
};

class Student : public Person
{
public:
	Student() : cur_id(id++) {}

	void getdata() {
		Person::getdata();
		for (int i = 0; i < 6; i++) {
			int m;
			cin >> m;
			marks += m;
		}
	}

	void putdata() {
		Person::putdata();
		cout << marks << " " << cur_id << endl;
	}

private:
	static int id;
	int marks, cur_id;
};

int Professor::id = 1;
int Student::id = 1;


int main()
{
	int n, val;
	cin >> n;
	Person *per[n];

	for (int i = 0; i < n; i++) {
		cin >> val;
		if (val == 1) {
			// If val is 1 current object is of type Professor
			per[i] = new Professor;
		} else {
			per[i] = new Student;
		}

		per[i]->getdata();
	}

	for (int i = 0; i < n; i++)
		per[i]->putdata();

	return 0;
}
