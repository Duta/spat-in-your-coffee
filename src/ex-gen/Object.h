#ifndef OBJECT_H
#define OBJECT_H

#include <stddef.h>
#include <stdlib.h>

#define NEW_Object() new_Object(sizeof(Object), Object_base)

typedef struct {
    int classID;
} Object_fields;

typedef struct {
    void (*printID)(void *this);
} Object_methods;

typedef struct Object {
    Object_methods _methods;
    Object_fields _fields;
} Object;

void * new_Object(size_t size, void (*init)(void *));

void init_Object(void *this);
void free_Object(void *this);

void Object_printID(void *this);

#endif
