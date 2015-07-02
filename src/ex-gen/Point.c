#include "Point.h"

void * new_Point(size_t size, void (*init)(void *)) {
    Point *this = new_Object(size, base, init);
    this->_methods.setX(this, 0);
    this->_methods.setY(this, 0);
    return this;
}

void * new_Point_int_int(size_t size, void (*init)(void *), int x, int y) {
    Point *this = new_Object(size, base, init);
    this->_methods.setX(this, x);
    this->_methods.setY(this, y);
    return this;
}

void init_Point(void *_this) {
    Point *this = _this;
    this->super = Object_base;
    this->_fields._inherited.classID = 2;
    this->_methods.setX = Point_setX;
    this->_methods.setY = Point_setY;
    this->_methods.getX = Point_getX;
    this->_methods.getY = Point_getY;
}

void free_Point(void *_this) {
    printf(" > free_Point\n");
    Point *this = _this;
    free_Object(_this);
    this->super.free(_this);
    printf(" < free_Point\n");
}

void Point_setX(void *_this, int x) {
    Point *this = _this;
    this->_fields.x = x;
}

void Point_setY(void *_this, int y) {
    Point *this = _this;
    this->_fields.y = y;
}

int Point_getX(void *_this) {
    Point *this = _this;
    return this->_fields.x;
}

int Point_getY(void *_this) {
    Point *this = _this;
    return this->_fields.y;
}
