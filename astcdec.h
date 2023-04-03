#ifndef _TCUASTCUTIL_HPP
#define _TCUASTCUTIL_HPP

#include <vector>

namespace basisu
{
    namespace astc
    {

// Unpacks a single ASTC block to pDst
// If isSRGB is true, the spec requires the decoder to scale the LDR 8-bit endpoints to 16-bit before interpolation slightly differently,
// which will lead to different outputs. So be sure to set it correctly (ideally it should match whatever the encoder did).
        bool decompress(uint8_t* pDst, const uint8_t* data, bool isSRGB, int blockWidth, int blockHeight);

    } // astc
} // basisu

#endif
