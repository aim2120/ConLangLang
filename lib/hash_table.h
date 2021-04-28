struct entry_s;
typedef struct entry_s entry_t;
struct hashtable_s;
typedef struct hashtable_s hashtable_t;

hashtable_t *ht_create( int );
hashtable_t *ht_grow( hashtable_t * );
void ht_delete( hashtable_t * );
int ht_hash( hashtable_t *, char * );
entry_t *ht_newpair( char *, char * );
hashtable_t *ht_set( hashtable_t *, char *, char * );
char *ht_get( hashtable_t *, char * );
int ht_print ( hashtable_t * );
