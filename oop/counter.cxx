#include <iostream>

using namespace std;

class Counter {
private:
  int n;

public:
  Counter(int ctor_n) : n(ctor_n) {}
  int increase() { return ++this->n; }
};

int main() {
  Counter *counter = new Counter(7);
  counter->increase();
  int result = counter->increase();
  cout << "Done, result= " << result << endl;
  delete counter;
}
