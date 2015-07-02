#include "Point.h"
#include <stdio.h>

int main(int argc, const char **argv) {
    Point *point = NEW_Point_int_int(5, 3);
    printf("%d\n", point->_fields._inherited.classID);
    printf("%d, %d\n", point->_methods.getX(point), point->_methods.getY(point));
    printf("test\n");
    point->_methods._inherited.free(point);
}
