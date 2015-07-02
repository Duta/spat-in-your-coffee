#ifndef POINT_H
#define POINT_H

#include "Object.h"
#include <stddef.h>

#define NEW_Point() new_Point(sizeof(Point), Point_base)
#define NEW_Point_int_int(x, y) new_Point_int_int(sizeof(Point), Point_base, x, y)

typedef struct {
    Object_fields _inherited;
    int x;
    int y;
} Point_fields;

typedef struct {
    Object_methods _inherited;
    void (*setX)(void *this, int x);
    void (*setY)(void *this, int y);
    int (*getX)(void *this);
    int (*getY)(void *this);
} Point_methods;

typedef struct Point {
    Point_methods _methods;
    Point_fields _fields;
    Object_methods super;
} Point;

Point_methods Point_base;

void * new_Point(size_t size, void (*init)(void *));
void * new_Point_int_int(size_t size, void (*init)(void *), int x, int y);

void init_methods_Point(void *this);
void free_Point(void *this);

void Point_setX(void *this, int x);
void Point_setY(void *this, int y);
int Point_getX(void *this);
int Point_getY(void *this);

#endif
