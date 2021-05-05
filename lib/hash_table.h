struct entry_s;
typedef struct entry_s entry_t;
struct hashtable_s;
typedef struct hashtable_s hashtable_t;

hashtable_t *ht_create( int, bool );
hashtable_t *ht_grow( hashtable_t * );
void ht_delete( hashtable_t * );
int ht_hash( hashtable_t *, char * );
entry_t *ht_newpair( char *, char * );
hashtable_t *ht_add( hashtable_t *, char *, char * );
hashtable_t *ht_remove( hashtable_t *, char * );
char *ht_get( hashtable_t *, char * );
char **ht_keys( hashtable_t * );
int ht_print ( hashtable_t * );
