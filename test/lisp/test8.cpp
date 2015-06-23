#include <iostream>

using namespace std;

extern "C" {
    int ackermann(int m, int n);
}

int ack(int m, int n) {
    if (m == 0) return n + 1;
    if (n == 0 && m > 0) return ack(m - 1, 1);
    if (m > 0 && n > 0) return ack(m - 1, ack(m, n-1));
    return 0;
}

int main()
{
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 3; j++) {
              if (ack(i, j) != ackermann(i, j)) {
                cerr << "ack = " << ack(i, j)
                     << " while ackermann = " << ackermann(i, j)
                     << " for m = " << i
                     << " for n = " << j << endl;
                return i;
              }
        }
    }
    return 0;
}
