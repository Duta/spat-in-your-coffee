#include "Object.h"

void * new_Object(size_t size, void (*init_methods)(void *)) {
    void *this = calloc(1, size);
    init_methods(this);
    return this;
}

void init_Object(void *_this) {
    Object_methods *this = _this;
    this->printID = Object_printID;
}

void free_Object(void *this) {
    printf(" > free_Object\n");
    if(this) {
        printf("   freeing object\n");
        free(this);
        printf("   freed object\n");
    }
    printf(" < free_Object\n");
}

void Object_printID(void *fields) {
    //
}
