#include <iostream>

using namespace std;

extern "C" {
    int plus1(int);
    int sub1(int);
    int mul1(int);
    int div1(int);

    int plus5(int, int, int, int, int);
    int sub5(int, int, int, int, int);
    int mul5(int, int, int, int, int);
    int div5(int, int, int, int, int);

    bool and5(bool, bool, bool, bool, bool);
    bool or5(bool, bool, bool, bool, bool);
}

int here_plus1(int a) { return a; }
int here_sub1(int a) { return -a; }
int here_mul1(int a) { return a; }
int here_div1(int a) { return a; }

int here_plus5(int a, int b, int c, int d, int e) { return a + b + c + d + e; }
int here_sub5(int a, int b, int c, int d, int e) { return a - (b + c + d + e); }
int here_mul5(int a, int b, int c, int d, int e) { return a * b * c * d * e; }
int here_div5(int a, int b, int c, int d, int e) { return a / (b * c * d * e); }

bool here_and5(bool a, bool b, bool c, bool d, bool e) {
    return a && b && c && d && e;
}
bool here_or5(bool a, bool b, bool c, bool d, bool e) {
    return a || b || c || d || e;
}

#define compare(a,b,c) if (a != b) {                                    \
        cerr << "Error: " << c << ". Difference: " << a << ", " << b << endl; \
        return 1;                                                       \
    }

int main()
{
    int lb = 2;
    int ub = 10;
    for (int a = lb; a < ub; a++) {
        compare(here_plus1(a), plus1(a), "plus1");
        compare(here_sub1(a), sub1(a), "sub1");
        compare(here_mul1(a), mul1(a), "mul1");
        compare(here_div1(a), div1(a), "div1");
        for (int b = lb; b < ub; b++) {
            for (int c = lb; c < ub; c++) {
                for (int d = lb; d < ub; d++) {
                    for (int e = lb; e < ub; e++) {
                        compare(here_plus5(a,b,c,d,e), plus5(a,b,c,d,e), "plus5");
                        compare(here_sub5(a,b,c,d,e), sub5(a,b,c,d,e), "sub5");
                        compare(here_mul5(a,b,c,d,e), mul5(a,b,c,d,e), "mul5");
                        compare(here_div5(a,b,c,d,e), div5(a,b,c,d,e), "div5");

                        int a1 = a % 2;
                        int b1 = b % 2;
                        int c1 = c % 2;
                        int d1 = d % 2;
                        int e1 = e % 2;

                        compare(here_or5(a1,b1,c1,d1,e1), or5(a1,b1,c1,d1,e1), "or5");
                        compare(here_and5(a1,b1,c1,d1,e1), and5(a1,b1,c1,d1,e1), "and5");
                    }}}}}
    return 0;
}
