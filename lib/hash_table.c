/*
 * -----------------------------------------------
 * Simple hash table implementation
 * Source: https://gist.github.com/tonious/1377667
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
#include "hash_table.h"
#include "find_prime.h"

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
};

typedef struct hashtable_s hashtable_t;


/* Create a new hashtable. */
hashtable_t *ht_create( int size ) {

    hashtable_t *hashtable = NULL;
    int i;

    if( size < 1 ) size = 1;

     size = find_prime(size * 2);

    /* Allocate the table itself. */
    if( ( hashtable = malloc( sizeof( hashtable_t ) ) ) == NULL ) {
        return NULL;
    }

    /* Allocate pointers to the head nodes. */
    if( ( hashtable->table = malloc( sizeof( entry_t * ) * size ) ) == NULL ) {
        return NULL;
    }
    for( i = 0; i < size; i++ ) {
        hashtable->table[i] = NULL;
    }

    hashtable->filled = 0;
    hashtable->size = size;

    return hashtable;
}

/* Copy old table into new larger table */
hashtable_t *ht_grow( hashtable_t *hashtable ) {
    int new_size;
    printf("growing table\n");

    new_size = hashtable->filled;
    new_size = find_prime(new_size * 2);

    hashtable_t* new_hashtable = ht_create(new_size);

    for (int i = 0; i < hashtable->size; i++) {
        entry_t *pair = hashtable->table[i];
        while ( pair != NULL && pair->key != NULL ) {
            ht_set(new_hashtable, pair->key, pair->value);
            pair = pair->next;
        }
    }

    ht_delete(hashtable);
    return new_hashtable;
}

/* Freeing the table */
void ht_delete( hashtable_t *hashtable ) {
    for (int i = 0; i < hashtable->size; i++) {
        entry_t *pair = hashtable->table[i];
        while ( pair != NULL) {
            if (pair->key != NULL) {
                free(pair->key);
            }
            if (pair->value != NULL) {
                free(pair->value);
            }
            entry_t *temp = pair;
            pair = pair->next;
            free(temp);
        }
    }
    free(hashtable->table);
    free(hashtable);
}

/* djb2 hash function */
/* Hash a string for a particular hash table. */
int ht_hash( hashtable_t *hashtable, char *key ) {

    unsigned long int hashval = 5381;
    int i = 0;

    /* Convert our string to an integer */
    while( hashval < ULONG_MAX && i < strlen( key ) ) {
        hashval = hashval << 5;
        hashval += key[ i ];
        i++;
    }

    return hashval % hashtable->size;
}

/* Create a key-value pair. */
entry_t *ht_newpair( char *key, char *value ) {
    entry_t *newpair;

    if( ( newpair = malloc( sizeof( entry_t ) ) ) == NULL ) {
        return NULL;
    }

    if( ( newpair->key = malloc( sizeof( char * ) ) ) == NULL ) {
        return NULL;
    }
    memcpy( newpair->key, key, sizeof ( char * ) );

    if( ( newpair->value = malloc( sizeof( char * ) ) ) == NULL ) {
        return NULL;
    }
    memcpy( newpair->value, value, sizeof ( char * ) );

    newpair->next = NULL;

    return newpair;
}

/* Insert a key-value pair into a hash table. */
hashtable_t *ht_set( hashtable_t *hashtable, char *key, char *value ) {
    int bin = 0;
    entry_t *newpair = NULL;
    entry_t *next = NULL;
    entry_t *last = NULL;

    bin = ht_hash( hashtable, key );

    next = hashtable->table[ bin ];

    while( next != NULL && next->key != NULL && memcmp( key, next->key, 1 ) != 0 ) {
        last = next;
        next = next->next;
    }

    /* There's already a pair.  Let's replace that string. */
    if( next != NULL && next->key != NULL && memcmp( key, next->key, 1 ) != 0 ) {

        memcpy( next->value, value, sizeof( char * ) );

    /* Nope, could't find it.  Time to grow a pair. */
    } else {
        hashtable->filled++;

        newpair = ht_newpair( key, value );

        if ( newpair == NULL ) {
            return NULL;
        }

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

/* Retrieve a key-value pair from a hash table. */
char *ht_get( hashtable_t *hashtable, char *key ) {
    int bin = 0;
    entry_t *pair;

    bin = ht_hash( hashtable, key );

    /* Step through the bin, looking for our value. */
    pair = hashtable->table[ bin ];
    while( pair != NULL && pair->key != NULL && memcmp( key, pair->key, 1 ) != 0 ) {
        pair = pair->next;
    }

    /* Did we actually find anything? */
    if( pair == NULL || pair->key == NULL || memcmp( key, pair->key , 1 ) != 0 ) {
        return NULL;

    } else {
        return pair->value;
    }
}

int ht_print (hashtable_t *hashtable) {
    int i = 0;
    for (int i = 0; i < hashtable->size; i++) {
        entry_t *pair = hashtable->table[i];
        printf("(%d) ", i);
        while ( pair != NULL) {
            printf("%lu : ", (unsigned long) pair->key);
            printf("%lu; ", (unsigned long) pair->value);
            pair = pair->next;
        }
        printf("\n");
    }
    return i;
}

/*
int main( int argc, char **argv ) {

    hashtable_t *hashtable = ht_create( 0 );

    ht_set( hashtable, "key1", "inky" );
    ht_print(hashtable);
    ht_set( hashtable, "key2", "pinky" );
    ht_print(hashtable);
    ht_set( hashtable, "key3", "blinky" );
    ht_print(hashtable);
    ht_set( hashtable, "key4", "floyd" );
    ht_print(hashtable);

    printf( "%s\n", ht_get( hashtable, "key1" ) );
    printf( "%s\n", ht_get( hashtable, "key2" ) );
    printf( "%s\n", ht_get( hashtable, "key3" ) );
    printf( "%s\n", ht_get( hashtable, "key4" ) );

    return 0;
}
*/
