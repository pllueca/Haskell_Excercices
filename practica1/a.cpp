#include <iostream>
#include <list>
using namespace std;

int main() {
    list<int> l;
    list<int>::iterator it;
    it = l.begin();
    int x = 2;
    l.insert(it, x);
    if (it == l.end()) cout << "1" << endl;
    else cout << "2" << endl;
}
