#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <stdbool.h>
#include <string.h>

const int BUFSIZE = 1000;

regex_t *re_create(char *r) {
    regex_t *regex;
    if ((regex = malloc (sizeof(regex_t))) == NULL) {
        return NULL;
    }
    if (regcomp(regex, r, 0)) return NULL;
    return regex;
}

bool re_match(regex_t *r, char *s) {
    return regexec(r, s, 0, NULL, 0) == 0;
}

/*
int main() {
    regex_t *regex;
    regmatch_t buf[BUFSIZE];

    regex = re_create("a.*");


    if (re_match(regex,"abc")) {
        printf("MATCH\n");
    } else {
        printf("NO MATCH\n");
    }

    ret = regexec(&regex, "abc", BUFSIZE, buf, 0);
    for (int i = 0; i < BUFSIZE; i++) {
        printf("%lld %lld\n", buf[i].rm_so, buf[i].rm_eo);
    }
}
*/
