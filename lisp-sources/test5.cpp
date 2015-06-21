#include <iostream>

extern "C" {
    int factorial(int n);
}

int main()
{
    if (120 != factorial(5)) {
        return 1;
    }
    return 0;
}
