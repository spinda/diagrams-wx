#include <stddef.h>
#include <string.h>

void copyCairoToWxWidgets ( float r, float g, float b
                          , ptrdiff_t size, unsigned char * cairo
                          , unsigned char * wx
                          ) {

  unsigned char * cairoEnd = cairo + size;

  while (cairo < cairoEnd) {
    float a1 = (float) cairo[3] / 255;
    float a2 = 1 - a1;

    wx[0] = (unsigned char) ((float) cairo[2] * a1 + r * a2);
    wx[1] = (unsigned char) ((float) cairo[1] * a1 + g * a2);
    wx[2] = (unsigned char) ((float) cairo[0] * a1 + b * a2);

    cairo += 4;
    wx += 3;
  }
}

