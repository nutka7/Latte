#include<stdio.h>
#include<stdlib.h>
#include<string.h>

void printInt(int);
void printString(char*);
void error(void);
void custom_error(char);
int readInt(void);
char *readString(void);
char *concatStrings(char*, char*);


int main() {
    printInt(4);
    printString(concatStrings(concatStrings("aa", "bb"), "cc"));
    printInt(readInt());
    printString(concatStrings(readString(), readString()));
    error();
    return 0;
}
