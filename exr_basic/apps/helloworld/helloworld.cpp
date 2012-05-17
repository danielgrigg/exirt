#include <exr_basic/exr_basic.h>
#include <stdio.h>
#include <vector>
#include <stdlib.h>
#include <algorithm>

double frandom() {
  return (double)random() / (double)RAND_MAX;
}

struct rgba {
  float r;
  float g;
  float b;
  float a;
};

rgba make_rgba() {
  rgba x = {frandom(), .5 * frandom(), .2 * frandom(), 1.0};
  return x;
}

int main() {
  printf("Calling write_rgba...\n");
  const int w = 512;
  const int h = 512;

  printf("Randoms = %f %f %f\n", frandom(), frandom(), frandom());

  std::vector<rgba> raw_ps(w * h);
  std::generate(raw_ps.begin(), raw_ps.end(), make_rgba);

  return write_rgba(w, h, (float*)&raw_ps[0]);
}

