#ifndef POINT_H
#define POINT_H

#include "Object.h"
#include <stddef.h>

#define NEW_Point() new_Point(sizeof(Point), Point_base)
#define NEW_Point_int_int(x, y) new_Point_int_int(sizeof(Point), Point_base, x, y)

typedef struct Point {
    Object super;
    int x;
    int y;
    void (*setX)(void *this, int x);
    void (*setY)(void *this, int y);
    int (*getX)(void *this);
    int (*getY)(void *this);
} Point;

Object Point_base;

void * new_Point(size_t size, Object base);
void * new_Point_int_int(size_t size, Object base, int x, int y);

void init_Point(void *this);
void free_Point(void *this);

void Point_setX(void *this, int x);
void Point_setY(void *this, int y);
int Point_getX(void *this);
int Point_getY(void *this);

#endif
