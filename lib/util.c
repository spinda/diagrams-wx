#include <stddef.h>
#include <string.h>

void copyCairoToWxWidgets ( float r, float g, float b
                          , ptrdiff_t size, unsigned char * cairo
                          , unsigned char * wx
                          ) {

  unsigned char * cairoEnd = cairo + size;

  while (cairo < cairoEnd) {
    float a = 1 - (float) cairo[3] / 255;

    wx[0] = (unsigned char) ((float) cairo[2] + r * a);
    wx[1] = (unsigned char) ((float) cairo[1] + g * a);
    wx[2] = (unsigned char) ((float) cairo[0] + b * a);

    cairo += 4;
    wx += 3;
  }
}

