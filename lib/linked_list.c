#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct ll_node_s {
    char *data;
    struct ll_node_s *next;
};

typedef struct ll_node_s ll_node;

ll_node *ll_create(char *data) {
    printf("begin create\n");
    ll_node *new_node;

    if ((new_node = malloc(sizeof (ll_node))) == NULL) {
        return NULL;
    }

    if ((new_node->data = strdup(data)) == NULL) { return NULL;
    }

    new_node->next = NULL;
    printf("made node %lu\n", (unsigned long)data);
    return new_node;
}

ll_node *ll_push(ll_node *head, char *data) {
    ll_node *curr = head;
    if (curr == NULL) return NULL;

    while (curr != NULL && curr->next!= NULL) {
        curr = curr->next;
    }

    ll_node *new_node = ll_create(data);
    curr->next = new_node;
    return new_node;
}

ll_node *ll_pop(ll_node *head) {
    ll_node *prev = head;
    ll_node *curr = head->next;

    if (curr == NULL) return NULL;

    while(curr->next != NULL) {
        prev = curr;
        curr = curr->next;
    }

    prev->next = NULL;
    return curr;
}

char *ll_get(ll_node *head, int n) {
    ll_node *curr = head;
    for ( int i = 0; i < n && curr != NULL; i++ ) {
        curr = curr->next;
    }
    return curr->data;
}

void ll_print_list(ll_node *head) {
    ll_node *curr = head;
    while(curr != NULL) {
        if (curr->data != NULL) {
            printf("%lu ",(unsigned long)curr->data);
        }
        curr = curr->next;
    }
    printf("\n");
}

/*
int main() {

    ll_node *head = ll_create("a");
    ll_push(head, "b");
    ll_push(head, "c");
    ll_print_list(head);
    ll_pop(head);
    ll_print_list(head);

    return 0;
}
*/
