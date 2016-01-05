#include<stdio.h>
#include<stdlib.h>
#include<string.h>

void printInt(int n) {
    printf("%d\n", n);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error(void) {
    puts("runtime error");
    exit(EXIT_FAILURE);
}

static void custom_error(char *s) {
    puts(s);
    exit(EXIT_FAILURE);
}

int readInt(void) {
    int a;
    if (EOF == scanf("%d\n", &a)) {
        custom_error("runtime error: readInt");
    }
    return a;
}

char *readString(void) {
    char *buf = NULL;
    size_t n;
    ssize_t size = getline(&buf, &n, stdin);
    if (size == -1) {
        custom_error("runtime error: readString");
    }
    if (buf[size - 1] == '\n') {
        buf[--size] = 0;
    }
    char *buf2 = realloc(buf, size + 1);
    if (buf2 == NULL) {
        custom_error("runtime error: readString");
    }
    return buf2;
}

char *concatStrings(char* s1, char* s2) {
    size_t l1 = strlen(s1);
    size_t l2 = strlen(s2);
    char* buf = malloc(l1 + l2 + 1);
    if (buf == NULL) {
        custom_error("runtime error: concatStrings");
    }
    strcpy(buf, s1);
    strcat(buf, s2);
    return buf;
}
