#include <iostream>

using namespace std;

extern "C" {
    int fibonacci(int n);
}

int fib(int n) {
    return n == 1 ? 1 :
        n == 0 ? 1 :
        fib(n-1) + fib(n-2);
}

int main()
{
    for (int i = 1; i < 10; i++) {
        if (fib(i) != fibonacci(i)) {
            cerr << "fib = " << fib(i)
                 << " while fibbonaci = " << fibonacci(i)
                 << " for i = " << i << endl;
            return i;
        }
    }
    return 0;
}
