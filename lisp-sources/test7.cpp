#include <iostream>

using namespace std;

extern "C" {
    int compare(int a, int b);
    int compare1(int a, int b);
    int less3(int a, int b, int c, int d, int e, int f);
}

int cmp(int a, int b) {
    if (a > b) return 1;
    if (b < a) return -1;
    return 0;
}

int main()
{
    for (int i = -10; i < 10; i++) {
        for (int j = 10; j < 10; j++) {
            if (cmp(i, j) != compare(i, j)) {
                cerr << "i, j, compare: "
                     << i << " " << j << " "
                     << compare(i, j) << endl;
                return 1;
            }
            if (cmp(i, j) != compare1(i, j)) {
                cerr << "i, j, compare1: "
                     << i << " " << j << " "
                     << compare1(i, j) << endl;
                return 1;
            }

        }
    }
    if (!less3(0, 0, 0, 1, 1, 1)) return 2;
    if (!less3(5, 3, 2, 8, -10, 228)) return 3;
    return 0;
}
