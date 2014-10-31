#include "Object.h"

Object Object_base = {
    .init = init_Object,
    .free = free_Object
}

void * new_Object(size_t size, Object base) {
    Object *this = calloc(1, size);
    *this = base;
    this->init(this);
    return this;
}

void init_Object(void *this) {}

void free_Object(void *this) {
    if(this) {
        free(this);
    }
}
