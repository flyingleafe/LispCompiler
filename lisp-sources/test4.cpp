#include <cstdio>

extern "C" {
    int foo(int a, int b);
}

int main()
{
    for(int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            if (foo(i,j) != (i + 3) * (j - 2)) {
                return 1;
            }
        }
    }
    return 0;
}
