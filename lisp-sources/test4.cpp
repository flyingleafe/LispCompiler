#include <cstdio>

extern "C" {
    int foo(int a, int b, int c, int d, int e, int f, int g, int h);
}

int main()
{
    for(int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            if (foo(i,j,1,2,3,4,5,6) != (i + 3) * (6 * (j - 2))) {
                return 1;
            }
        }
    }
    return 0;
}
