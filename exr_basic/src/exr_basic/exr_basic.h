#ifndef EXR_BASIC_H
#define EXR_BASIC_H

extern "C" {
  int write_rgba(int width, 
      int height, 
      char* filename,
      const float* raw_rgbas);
}

#endif
