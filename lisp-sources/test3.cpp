#include <iostream>
#include <stdio.h>

using namespace std;

extern "C" {
    int foo();
}

int main() {
    int res = foo();
    if (res != 5) return 1;
    return 0;
}
