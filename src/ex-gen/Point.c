#include "Point.h"

Object Point_base = {
    .init = init_Point,
    .free = free_Point
};

void * new_Point(size_t size, Object base) {
    Point *this = new_Object(size, base);
    this->super.init(&this->super);
    this->setX(this, 0);
    this->setY(this, 0);
    return this;
}

void * new_Point_int_int(size_t size, Object base, int x, int y) {
    Point *this = new_Object(size, base);
    this->super.init(&this->super);
    this->setX(this, x);
    this->setY(this, y);
    return this;
}

void init_Point(void *_this) {
    init_Object(_this);
    Point *this = _this;
    this->super = Object_base;
    this->setX = Point_setX;
    this->setY = Point_setY;
    this->getX = Point_getX;
    this->getY = Point_getY;
}

void free_Point(void *_this) {
    Point *this = _this;
    free_Object(_this);
    this->super.free(_this);
}

void Point_setX(void *_this, int x) {
    Point *this = _this;
    this->x = x;
}

void Point_setY(void *_this, int y) {
    Point *this = _this;
    this->y = y;
}

int Point_getX(void *_this) {
    Point *this = _this;
    return this->x;
}

int Point_getY(void *_this) {
    Point *this = _this;
    return this->y;
}
