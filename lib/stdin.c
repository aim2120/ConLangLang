#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <limits.h>
#include "linked_list.h"
#include "malloc_manager.h"

#define LRGBUFSIZE 4096

ll_node *ll_of_stdin() {
    char buf[1];
    char largebuf[LRGBUFSIZE];
    int strlen = 0;
    ll_node *ll_head = NULL;

    fseek (stdin, 0, SEEK_END);
    if (ftell (stdin) == 0) {
        return NULL;
    }

    fseek(stdin, 0, SEEK_SET);
    memset(largebuf, 0, sizeof(char) * (LRGBUFSIZE - 1));

    while(read(0, buf, sizeof(buf))>0) {
        char c = *buf;
        char *s;
        char **s_addr;
        bool is_whitespace = (c == ' ' || c == '\n' || c == '\t' || c == '\r');

        if (strlen > 0 && is_whitespace) {
            s = malloc(sizeof(char) * (strlen + 1));
            if (s == NULL) {
                exit(1);
            }
            add_malloc_addr(s);

            s_addr = malloc(sizeof(char *));
            if (s_addr == NULL) {
                exit(1);
            }
            add_malloc_addr((char *)s_addr);

            memcpy(s, largebuf, sizeof(char) * strlen);
            s[strlen] = '\0';
            memcpy(s_addr, &s, sizeof(char *));

            // remember: ll data is janky universal char * addresses, not values
            ll_head = ll_add(ll_head, (char *)s_addr, INT_MAX);

            strlen = 0;
            memset(largebuf, 0, sizeof(char) * (LRGBUFSIZE - 1));
        } else if (!is_whitespace) {
            largebuf[strlen] = c;
            strlen++;

            if (strlen == LRGBUFSIZE) {
                exit(1);
            }
        }
    }

    return ll_head;
}

