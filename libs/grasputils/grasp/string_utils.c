#include <math.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <assert.h>
#include <locale.h>

void discard_rest_of_line(FILE *fp) {
  while (fgetc(fp) != '\n') {
    if (feof(fp)) break;
  }
}

char *strtolower(const char *data) {
    char *c;
    int i, max;

    max = strlen(data);
    // I make a copy of input string converting it to lower case
    c = (char *) malloc(sizeof (char)*max + 1);
    for (i = 0; i < max; i++) {
        c[i] = tolower(data[i]);
    }
    c[i] = '\0';

    return c;
}

int safe_atoi (const char *s, int *result){
   long lval;
   char *end;

   lval = strtol (s, &end, 10);
   if (lval > INT_MAX || lval < INT_MIN){
      // The number was not in range int.
      return -1;
   }else if (end == s){
      // Unrecognised string
      return -2;
   }else if (*end){
      // This string has more than a integer number
      return -3;
   }

   *result= (int)lval;
   return 0;
}

int safe_atofloat (const char *s, float *result){
    char *endptr;

    // strtof is dependent on the locale set in the system, but the grasp settings
    // will always have floating point numbers formatted with a point as comma, therefore
    // we set the locale here to POSIX C in order to properly parse the floating point numbers
    setlocale(LC_ALL, "C");
    float d = strtof(s, &endptr);

    if (s == endptr){
        // Unrecognised string
        return -2;
    }else{
        if(isinf(d)){
            // Is grater than the range
            return -1;
        }else{
            if(isnan(d)){
                return -3;
            }else{
                *result=d;
                return 0;
            }
        }
    }
}

char **chunkify( char *line, const char *sep, int *count ){
    char **array, *tmp;
    char *token;
    int i=0;
    char *copy;
    char *ptr, *rest;

    // We allocate to pointers to maximum of tokens we could have
    array= (char **)malloc(sizeof(char *) * strlen(line) + 1);
    assert(array!=NULL);
    copy = (char *) malloc(sizeof (char)*(strlen(line) + 1));
    assert(copy!=NULL);
    strcpy(copy, line);
    ptr=copy;


    while((token=strtok_r( ptr, sep, &rest ))){
        tmp = (char *) malloc(sizeof (char)*(strlen(token)+1));
        strcpy(tmp,token);
        // Save token
        array[i] = tmp;
        // Prepare to next loop
        ptr = rest;
        i++;
    }

    if (count!=NULL){
        *count=i;
    }

    free(copy);

    // As improvement here we could optimize the memory used.

    return array;
}
