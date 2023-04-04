#ifndef _TCUASTCUTIL_HPP
#define _TCUASTCUTIL_HPP

#include <stdint.h>

namespace basisu
{
    namespace astc
    {

// Unpacks a single ASTC block to pDst
// If isSRGB is true, the spec requires the decoder to scale the LDR 8-bit endpoints to 16-bit before interpolation slightly differently,
// which will lead to different outputs. So be sure to set it correctly (ideally it should match whatever the encoder did).
        uint32_t transferToRGBA( uint8_t *astc_file_data, uint32_t astc_file_size, unsigned char **outRGBA ,uint32_t *outWidth, uint32_t *outHeight );
    } // astc
} // basisu

#endif
