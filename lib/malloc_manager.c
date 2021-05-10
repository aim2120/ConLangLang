#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#define INITSIZE 4096

uint8_t **malloc_addr;
int malloc_addr_size;
int addr_i;

void init_malloc_addr() {
    malloc_addr = malloc(sizeof(uint8_t *) * INITSIZE);
    memset(malloc_addr, 0, sizeof(uint8_t *) * INITSIZE);
    if (malloc_addr == NULL) {
        exit(1);
    }
    malloc_addr_size = INITSIZE;
    addr_i = 0;
}

void add_malloc_addr(uint8_t *addr) {
    malloc_addr[addr_i] = addr;
    addr_i++;

    if (addr_i == malloc_addr_size) {
        int old_malloc_addr_size = malloc_addr_size;
        malloc_addr_size *= 2;
        uint8_t **new_malloc_addr = malloc(sizeof(uint8_t *) * malloc_addr_size);
        memset(new_malloc_addr, 0, sizeof(uint8_t *) * malloc_addr_size);
        memcpy(new_malloc_addr, malloc_addr, sizeof(uint8_t *) * old_malloc_addr_size);
        malloc_addr = new_malloc_addr;
    }
}

void free_malloc_addrs() {
    for (int i = 0; i < malloc_addr_size; i++) {
        free(malloc_addr[i]);
    }
}

