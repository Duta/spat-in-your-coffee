#include "Point.h"

Object Point_base = {
    .init = init_Point,
    .free = free_Point
};

void * new_Point(size_t size, Object base) {
    Point *this = new_Object(size, base);
    super.init(&super);
    this->setX(this, 0);
    this->setY(this, 0);
    return this;
}

void * new_Point_int_int(size_t size, Object base, int x, int y) {
    Point *this = new_Object(size, base);
    super.init(&super);
    this->setX(this, x);
    this->setY(this, y);
    return this;
}

void init_Point(void *_this) {
    Point *this = _this;
    this->super = Object_base;
    this->setX = Point_setX;
    this->setY = Point_setY;
    this->getX = Point_getX;
    this->getY = Point_getY;
    return 0;
}

void free_Point(void *_this) {
    Point *this = _this;
    this->super.free(_this);
}

void Point_setX(void *this, int x) {
    this->x = x;
}

void Point_setY(void *this, int y) {
    this->y = y;
}

int Point_getX(void *this) {
    return this->x;
}

int Point_getY(void *this) {
    return this->y;
}
