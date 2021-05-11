#include <stdbool.h>
struct ll_node_s;
typedef struct ll_node_s ll_node;
ll_node *ll_create(char *);
ll_node *ll_add(ll_node *, char *, int);
int ll_mem(ll_node *, char *, bool);
ll_node *ll_remove(ll_node *, int);
ll_node *ll_next(ll_node *);
char *_get(ll_node *, int);
int ll_print(ll_node *);
int ll_size(ll_node *);
void ll_del(ll_node *);
