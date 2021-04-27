struct ll_node_s {
    char *data;
    node *next;
}

typedef struct ll_node_s ll_node;

ll_node *ll_create(char *data) {
    ll_node *new_node;

    if ((new_node = malloc(sizeof (ll_node))) == NULL) {
        return NULL;
    }

    if ((new_node->data = strdup(data)) == NULL) {
        return NULL;
    }

    return new_node;
}

ll_node *ll_push(node *head, char *data) {
    node *last = head->next;
    while (last != NULL) {
        last = last->next;
    }

    node *new_node = create_node(data);
    last->next = new_node;
    return new_node;
}

ll_node *ll_pop(node *head) {
    node *curr = head;
    node *last = head->next;

    while(last != NULL) {
        curr = last;
        last = last->next;
    }

    curr->next = NULL;
    return last;
}
