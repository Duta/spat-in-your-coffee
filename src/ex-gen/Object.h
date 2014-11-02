#ifndef OBJECT_H
#define OBJECT_H

#include <stddef.h>
#include <stdlib.h>

#define NEW_Object() new_Object(sizeof(Object), Object_base)

typedef struct Object {
    void (*init)(void *this);
    void (*free)(void *this);
} Object;

Object Object_base;

void * new_Object(size_t size, Object base);

void init_Object(void *this);
void free_Object(void *this);

#endif
