#include <stdio.h>

int foo();

int main() 
{
    if(5 == foo()) {
        printf("ok\n");
    } else {
        printf("fail\n");
    } 
    return 0;
}
