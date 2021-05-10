#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#define INITSIZE 4096

char **malloc_addr;
int malloc_addr_size;
int addr_i;

void init_malloc_addr() {
    malloc_addr = malloc(sizeof(char *) * INITSIZE);
    memset(malloc_addr, 0, sizeof(char *) * INITSIZE);
    if (malloc_addr == NULL) {
        exit(1);
    }
    malloc_addr_size = INITSIZE;
    addr_i = 0;
}

void add_malloc_addr(char *addr) {
    printf("adding %lu\n", (unsigned long) addr);
    malloc_addr[addr_i] = addr;
    addr_i++;

    if (addr_i == malloc_addr_size) {

        int old_malloc_addr_size = malloc_addr_size;
        malloc_addr_size *= 2;
        char **new_malloc_addr = malloc(sizeof(char *) * malloc_addr_size);
        memset(new_malloc_addr, 0, sizeof(char *) * malloc_addr_size);
        memcpy(new_malloc_addr, malloc_addr, sizeof(char *) * old_malloc_addr_size);
        malloc_addr = new_malloc_addr;
    }
}

void free_malloc_addrs() {
    for (int i = 0; i < malloc_addr_size; i++) {
        if (malloc_addr[i] == NULL) {
            break;
        }
        printf("freeing %lu\n", (unsigned long) malloc_addr[i]);
        free(malloc_addr[i]);
    }
}

