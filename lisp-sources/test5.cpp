#include <iostream>

extern "C" {
    int factorial(int n);
}

int fact(int n) {
    return n == 1 ? 1 : n * fact(n - 1);
}

int main()
{
    for (int i = 1; i < 7; i++) {
        if (fact(i) != factorial(i)) return i;
    }
    return 0;
}
