#include <stdio.h>  // printf

struct pair_t {
    char first[100];
    size_t second;
};

int main() {
    struct pair_t types[] = {
        { "char", sizeof(char) },
        { "unsigned char", sizeof(unsigned char) },
        { "short", sizeof(short) },
        { "unsigned short", sizeof(unsigned short) },
        { "int", sizeof(int) },
        { "unsigned int", sizeof(unsigned int) },
        { "long", sizeof(long) },
        { "unsigned long", sizeof(unsigned long) },
        { "long long", sizeof(long long) },
        { "unsigned long long", sizeof(unsigned long long) },
        { "float", sizeof(float) },
        { "double", sizeof(double) },
        { "long double", sizeof(long double) }
    };

    for (size_t i = 0; i < sizeof(types) / sizeof(*types); ++i) {
        printf("\"%s\" is %zu bytes.\n", types[i].first, types[i].second);
    }

    return 0;
}
