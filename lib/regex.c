/*
 * Author: Annalise Mariottini (aim2120)
 */

#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <stdbool.h>
#include <string.h>

const int BUFSIZE = 5;

regex_t *re_create(char *r) {
    regex_t *regex;
    if ((regex = malloc (sizeof(regex_t))) == NULL) {
        return NULL;
    }
    if (regcomp(regex, r, REG_EXTENDED)) return NULL;
    return regex;
}

bool re_match(regex_t *r, char *s) {
    return regexec(r, s, 0, NULL, 0) == 0;
}

char *re_sub(regex_t *r, char *s, char *t, int n) {
    char *new_s;
    char *s_ = s;
    int s_len, t_len, match_len, new_s_len;
    int match_start, match_end;
    int ret;

    while (1) {
        regmatch_t buf[n+1];

        if (regexec(r, s_, n+1, buf, 0)) return s_;
        
        match_start = buf[n].rm_so;
        match_end = buf[n].rm_eo;
        if (match_start < 0) {
            printf("finished\n");
            break;
        }

        s_len = strlen(s_);
        t_len = strlen(t);
        match_len = match_end-match_start;

        new_s_len = (s_len - match_len) + t_len + 1;

        if ((new_s = malloc (sizeof(char *) * new_s_len)) == NULL) {
            return NULL;
        }

        memmove(new_s, s_, match_start);
        memmove(&new_s[match_start], t, t_len);
        memmove(&new_s[match_start+t_len], &s_[match_end], s_len-match_end);
        new_s[new_s_len-1] = '\0';

        s_ = new_s;
    }

    return s_;
}

