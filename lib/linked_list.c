#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

struct ll_node_s {
    char *data;
    struct ll_node_s *next;
};

typedef struct ll_node_s ll_node;

ll_node *ll_create(char *data) {
    ll_node *new_node;

    if ((new_node = malloc(sizeof (ll_node))) == NULL) {
        return NULL;
    }

    if ((new_node->data = malloc(sizeof (char *))) == NULL) {
        return NULL;
    }

    memcpy(new_node->data, data, sizeof (char *));

    new_node->next = NULL;
    return new_node;
}

ll_node *ll_add(ll_node *head, char *data, int n) {
    ll_node *curr = head;
    ll_node *prev = NULL;
    ll_node *new_node;

    if (curr == NULL) {
        return NULL;
    }

    for (int i = 0; i < n && curr != NULL; i++) {
        prev = curr;
        curr = curr->next;
    }

    new_node = ll_create(data);

    if (new_node == NULL) {
        return NULL;
    }

    if (prev != NULL) {
        prev->next = new_node;
        new_node->next = curr;
    } else {
        new_node->next = head;
        head = new_node;
    }

    return head;
}

int ll_mem(ll_node *head, char *data, bool is_string) {
    ll_node *curr = head;
    char *data_;
    char *currdata_;
    int n = -1;
    int i = 0;
    bool addr_cmp = false, str_cmp = false;

    if (is_string) {
        data_ = *(char **)data;
    }

    while (curr != NULL) {
        addr_cmp = memcmp( data, curr->data, 1) == 0;

        if (is_string) {
            currdata_ = *(char **)(curr->data);
            str_cmp = strcmp(data_, currdata_) == 0;
        }

        if (addr_cmp || str_cmp) {
            n = i;
            break;
        }
        curr = curr->next;
        i++;
    }

    return n;
}

/* returns new head node */
ll_node *ll_remove(ll_node *head, int n) {
    ll_node *curr = head;
    ll_node *prev = NULL;

    if (curr  == NULL) {
        return NULL;
    }

    for (int i = 0; i < n && curr->next != NULL; i++) {
        prev = curr;
        curr = curr->next;
    }

    if (prev != NULL) {
        prev->next = curr->next;
    } else {
        head = head->next;
    }

    free(curr);

    return head;
}

ll_node *ll_next(ll_node *node) {
    return node->next;
}

char *ll_get(ll_node *head, int n) {
    ll_node *curr = head;
    for ( int i = 0; i < n && curr->next != NULL; i++ ) {
        curr = curr->next;
    }
    return curr->data;
}

int ll_print(ll_node *head) {
    int i = 0;
    ll_node *curr = head;
    while(curr != NULL) {
        if (curr->data != NULL) {
            printf("%lu ",(unsigned long)curr->data);
            i++;
        }
        curr = curr->next;
    }
    printf("\n");
    return i;
}

int ll_size(ll_node *head) {
    int i = 0;
    ll_node *curr = head;
    while(curr != NULL) {
        i++;
        curr = curr->next;
    }
    return i;
}

/* TODO: implement later if necessary
ll_node *ll_dup(ll_node *head) {
    ll_node *newhead;
    ll_node *curr = head;

    if (curr == NULL) {
        return NULL;
    }

    newhead = ll_create(curr->data);
    curr = curr->next;
    while (curr != NULL) {
        ll_add(newhead, curr->data, );
        curr = curr->next;
    }

    return newhead;
}
*/

void ll_del(ll_node *head) {
    ll_node *curr = head;
    ll_node *temp;

    while (curr != NULL) {
        if (curr->data != NULL) {
            free(curr->data);
        }
        temp = curr;
        curr = curr->next;
        free(temp);
    }
}

/*
int main() {

    char *x0;
    char *x1;
    char *x2;
    ll_node *head = ll_create("a");
    ll_add(head, "b");
    ll_add(head, "c");
    x0 = ll_get(head, 0);
    x1 = ll_get(head, 1);
    x2 = ll_get(head, 2);
    printf("%s %s %s\n", x0, x1, x2);

    return 0;
}
*/
