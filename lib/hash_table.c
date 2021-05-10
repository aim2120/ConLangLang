/*
 * -----------------------------------------------
 * Simple hash table implementation
 * Sourced from https://gist.github.com/tonious/1377667
 * with many changes made
 * -----------------------------------------------
 */
/* Read this comment first: https://gist.github.com/tonious/1377667#gistcomment-2277101
 * 2017-12-05
 *
 *  -- T.
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdbool.h>
#include "hash_table.h"
#include "find_prime.h"
#include "linked_list.h"
#include "malloc_manager.h"

struct entry_s {
    char *key;
    char *value;
    struct entry_s *next;
};

typedef struct entry_s entry_t;

struct hashtable_s {
    int size;
    int filled;
    struct entry_s **table;
    bool key_is_string;
};

typedef struct hashtable_s hashtable_t;


/* Create a new hashtable. */
hashtable_t *ht_create( int size, bool key_is_string ) {

    hashtable_t *hashtable = NULL;
    int i;

    if( size < 1 ) size = 1;

     size = find_prime(size * 2);

    /* Allocate the table itself. */
    if( ( hashtable = malloc( sizeof( hashtable_t ) ) ) == NULL ) {
        exit(1);
    }

    add_malloc_addr((char *)hashtable);

    /* Allocate pointers to the head nodes. */
    if( ( hashtable->table = malloc( sizeof( entry_t * ) * size ) ) == NULL ) {
        exit(1);
    }

    add_malloc_addr((char *)hashtable->table);


    for( i = 0; i < size; i++ ) {
        hashtable->table[i] = NULL;
    }

    hashtable->filled = 0;
    hashtable->size = size;
    hashtable->key_is_string = key_is_string;

    return hashtable;
}

/* Copy old table into new larger table */
hashtable_t *ht_grow( hashtable_t *hashtable ) {
    int new_size;

    new_size = hashtable->filled;
    new_size = find_prime(new_size * 2);

    hashtable_t* new_hashtable = ht_create(new_size, hashtable->key_is_string);

    for (int i = 0; i < hashtable->size; i++) {
        entry_t *pair = hashtable->table[i];
        while ( pair != NULL && pair->key != NULL ) {
            ht_add(new_hashtable, pair->key, pair->value);
            pair = pair->next;
        }
    }

    return new_hashtable;
}


/* Hash a string for a particular hash table. */
int ht_hash( hashtable_t *hashtable, char *key ) {

    unsigned long int hashval = 5381;
    int i = 0;

    if (hashtable->key_is_string) {
        /* Convert our string to an integer */
        /* djb2 hash function */
        while( hashval < ULONG_MAX && i < strlen( key ) ) {
            hashval += key[ i ];
            hashval = hashval << 5;
            i++;
        }
    }
    else {
        /* https://stackoverflow.com/questions/664014/
         * what-integer-hash-function-are-good-that-accepts-an-integer-hash-key */
        hashval += key[0];
        hashval = ((hashval >> 16) ^ hashval) * 0x45d9f3b;
        hashval = ((hashval >> 16) ^ hashval) * 0x45d9f3b;
        hashval = (hashval >> 16) ^ hashval;
    }

    return hashval % hashtable->size;
}

/* Create a key-value pair. */
entry_t *ht_newpair( char *key, char *value ) {
    entry_t *newpair;

    if( ( newpair = malloc( sizeof( entry_t ) ) ) == NULL ) {
        exit(1);
    }
    add_malloc_addr((char *)newpair);

    if( ( newpair->key = malloc( sizeof( char * ) ) ) == NULL ) {
        exit(1);
    }
    add_malloc_addr((char *)newpair->key);
    memcpy( newpair->key, key, sizeof ( char * ) );

    if( ( newpair->value = malloc( sizeof( char * ) ) ) == NULL ) {
        exit(1);
    }
    add_malloc_addr((char *)newpair->value);
    memcpy( newpair->value, value, sizeof ( char * ) );

    newpair->next = NULL;

    return newpair;
}

/* Insert a key-value pair into a hash table. */
hashtable_t *ht_add( hashtable_t *hashtable, char *key, char *value ) {
    int bin = 0;
    bool kis = hashtable->key_is_string;
    entry_t *newpair = NULL;
    entry_t *next = NULL;
    entry_t *last = NULL;

    char *to_hash = key;
    if (kis) {
        to_hash = *(char **)key;
    }
    bin = ht_hash( hashtable, to_hash );

    next = hashtable->table[ bin ];

    char **key_ptr;
    char *key_;
    char **nextkey_ptr;
    char *nextkey_;

    if (kis) {
        key_ptr = (char **) key;
        key_ = *key_ptr;
    }

    bool addr_cmp, str_cmp = false;
    while( next != NULL && next->key != NULL ) {
        addr_cmp = !kis && memcmp( key, next->key, 1) == 0;
        if (kis) {
            nextkey_ptr = (char **) (next->key);
            nextkey_ = *nextkey_ptr;
            str_cmp = strcmp( key_, nextkey_ ) == 0;
        }
        if (addr_cmp || str_cmp) {
            break;
        }

        last = next;
        next = next->next;
    }

    /* There's already a pair.  Let's replace that string. */
    if( next != NULL && next->key != NULL && (addr_cmp || str_cmp) ) {

        memcpy( next->value, value, sizeof( char * ) );

    /* Nope, could't find it.  Time to grow a pair. */
    } else {
        hashtable->filled++;

        newpair = ht_newpair( key, value );

        /* We're at the start of the linked list in this bin. */
        if( next == hashtable->table[ bin ] ) {
            newpair->next = next;
            hashtable->table[ bin ] = newpair;

        /* We're at the end of the linked list in this bin. */
        } else if ( next == NULL ) {
            last->next = newpair;
        /* We're in the middle of the list. */
        } else  {
            newpair->next = next;
            last->next = newpair;
        }
    }

     /* Growing the table if the size is < 1.3 * filled */
    if ((hashtable->filled * 1.3) > (float)hashtable->size) {
        hashtable = ht_grow(hashtable);
    }

    return hashtable;
}

hashtable_t *ht_remove(hashtable_t *hashtable, char *key) {
    int bin = 0;
    bool kis = hashtable->key_is_string;
    entry_t *newpair = NULL;
    entry_t *next = NULL;
    entry_t *last = NULL;

    char *to_hash = key;
    if (kis) {
        to_hash = *(char **)key;
    }
    bin = ht_hash( hashtable, to_hash );

    next = hashtable->table[ bin ];

    char **key_ptr;
    char *key_;
    char **nextkey_ptr;
    char *nextkey_;

    if (kis) {
        key_ptr = (char **) key;
        key_ = *key_ptr;
    }

    bool addr_cmp, str_cmp = false;
    while( next != NULL && next->key != NULL ) {
        addr_cmp = !kis && memcmp( key, next->key, 1) == 0;
        if (kis) {
            nextkey_ptr = (char **) (next->key);
            nextkey_ = *nextkey_ptr;
            str_cmp = strcmp( key_, nextkey_ ) == 0;
        }
        if (addr_cmp || str_cmp) {
            break;
        }

        last = next;
        next = next->next;
    }

    /* found the key to remove */
    if( next != NULL && (addr_cmp || str_cmp) ) {

        if (last != NULL) {
            last->next = next->next;
        } else {
            hashtable->table[ bin ] = next->next;
        }

        hashtable->filled--;

    } /* else -> couldn't find key, return hashtable unchanged */

    return hashtable;
}

bool ht_mem( hashtable_t *hashtable, char *key ) {
    if (ht_get(hashtable, key) == NULL) {
        return false;
    } else {
        return true;
    }
}

/* Retrieve a key-value pair from a hash table. */
char *ht_get( hashtable_t *hashtable, char *key ) {
    int bin = 0;
    bool kis = hashtable->key_is_string;
    entry_t *pair;

    char *to_hash = key;
    if (kis) {
        to_hash = *(char **)key;
    }
    bin = ht_hash( hashtable, to_hash );

    /* Step through the bin, looking for our value. */
    pair = hashtable->table[ bin ];

    char **key_ptr;
    char *key_;
    char **pairkey_ptr;
    char *pairkey_;

    if (kis) {
        key_ptr = (char **) key;
        key_ = *key_ptr;
    }

    bool addr_cmp, str_cmp = false;

    while( pair != NULL && pair->key != NULL ) {
        if (kis) {
            pairkey_ptr = (char **) (pair->key);
            pairkey_ = *pairkey_ptr;
            str_cmp = strcmp( key_, pairkey_ ) == 0;
        }
        addr_cmp = !kis && memcmp( key, pair->key, 1) == 0;
        if (addr_cmp || str_cmp) {
            break;
        }

        pair = pair->next;
    }

    /* Did we actually find anything? */
    if( pair == NULL || pair->key == NULL || !(addr_cmp || str_cmp) ) {
        exit(1);

    } else {
        return pair->value;
    }
}

char **ht_keys(hashtable_t *hashtable) {
    char **keys;

    if ((keys = malloc((sizeof (char**)) * hashtable->filled)) == NULL) {
        exit(1);
    }
    add_malloc_addr((char *)keys);

    int j = 0;

    for (int i = 0; i < hashtable->size; i++) {
        entry_t *pair = hashtable->table[i];
        while (pair != NULL) {
            keys[j] = pair->key;
            pair = pair->next;
            j++;
        }
    }

    for (int i = 0; i < hashtable->filled; i++) {
    };

    return keys;
};

ll_node *ht_keys_list(hashtable_t *hashtable) {
    ll_node *head_key = NULL;

    for (int i = 0; i < hashtable->size; i++) {
        entry_t *pair = hashtable->table[i];
        while (pair != NULL) {
            head_key = ll_add(head_key, pair->key, 0);
            pair = pair->next;
        }
    }

    return head_key;
};

// for debugging, not pretty
int ht_print (hashtable_t *hashtable) {
    int i = 0;
    for (int i = 0; i < hashtable->size; i++) {
        entry_t *pair = hashtable->table[i];
        printf("(%d) ", i);
        while ( pair != NULL) {
            if (hashtable->key_is_string) {
                printf("%s : ", *(char**) pair->key);
            }
            else {
                printf("%lu : ", (unsigned long) pair->key);
            }
            printf("%lu; ", (unsigned long) pair->value);
            pair = pair->next;
        }
        printf("\n");
    }
    return i;
}

int ht_size (hashtable_t *hashtable) {
    return hashtable->filled;
}

/*
int main( int argc, char **argv ) {

    hashtable_t *hashtable = ht_create( 0 );

    ht_add( hashtable, "key1", "inky" );
    ht_print(hashtable);
    ht_add( hashtable, "key2", "pinky" );
    ht_print(hashtable);
    ht_add( hashtable, "key3", "blinky" );
    ht_print(hashtable);
    ht_add( hashtable, "key4", "floyd" );
    ht_print(hashtable);

    ht_remove( hashtable, "key1" );
    ht_print(hashtable);

    printf( "%s\n", ht_get( hashtable, "key1" ) );
    printf( "%s\n", ht_get( hashtable, "key2" ) );
    printf( "%s\n", ht_get( hashtable, "key3" ) );
    printf( "%s\n", ht_get( hashtable, "key4" ) );

    return 0;
}
*/
