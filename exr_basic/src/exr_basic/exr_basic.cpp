#include "exr_basic.h"
#include <stdio.h>
#include <vector>
#include <ImfRgbaFile.h>

//#include <OpenEXR/ImfOutputFile.h>
//#include <OpenEXR/ImfInputFile.h>
//#include <OpenEXR/ImfChannelList.h>
//#include <OpenEXR/ImfStringAttribute.h>
//#include <OpenEXR/ImfMatrixAttribute.h>

int write_rgba(int width, 
    int height, 
    char* filename, 
    const float* raw_rgbas) {

  //const char fname[] = "/tmp/basic_exr.exr";
  std::vector<Imf::Rgba> pixels(width * height);
  for (int p = 0; p < width * height; ++p) {
    pixels[p] = Imf::Rgba(raw_rgbas[4*p+0], 
                          raw_rgbas[4*p+1], 
                          raw_rgbas[4*p+2],
                          raw_rgbas[4*p+3]);
  }
  Imf::RgbaOutputFile file (filename, width, height, Imf::WRITE_RGBA);
  file.setFrameBuffer(&pixels[0], 1, width);
  file.writePixels (height); 
  return 1;
}

