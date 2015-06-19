#include <stdio.h>

int foo();

int main() 
{
    if(7 == foo()) {
        printf("ok\n");
    } else {
        printf("fail\n");
    } 
    return 0;
}
