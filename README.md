# astcdecode
ASTC to RGBA

ASTC压缩纹理软解，基于 https://github.com/richgel999/astc_dec


我在此基础上做了一定性能计算优化。
1280x720ASTC纹理，原始解码时长950毫秒左右，经优化后时长750毫秒左右（测试设备Honor 7X）


```
#define ASTC_HEADER_SIZE 16

#define ASTC_HEADER_MAGIC 4

const unsigned char *filedata = ...
                uint32_t dataLen = ...
                const auto *header = static_cast<const astc_byte *>(filedata);
                if (astcIsValid(header)){
                    auto start = std::chrono::high_resolution_clock::now();
                    int width = astcGetWidth(header);
                    int height = astcGetHeight(header);
                    dataLen = dataLen - ASTC_HEADER_SIZE;
                    // astc data block
                    const uint8_t * astc_data = filedata + ASTC_HEADER_SIZE;
                    //         bool decompress(uint8_t* pDst, const uint8_t* data, bool isSRGB, int blockWidth, int blockHeight);
                    uint32_t size_rgba = width * height <<2;
                    uint8_t* img_data = static_cast<unsigned char *>(malloc(size_rgba * sizeof(unsigned char)));
                    uint8_t k_size_in_bytes = 16;
                    uint8_t k_bytes_per_pixel_unorm8 = 4;
                    uint8_t block_width = header[ASTC_HEADER_MAGIC];
                    uint8_t block_height = header[ASTC_HEADER_MAGIC + 1];
                    int blocks_wide = (width + block_width - 1) / block_width;
                    int row_length = block_width * k_bytes_per_pixel_unorm8;
                    uint8_t block[block_width * block_height << 2];
                    int block_index = 0;
                    for (int i=0;i<dataLen;i+= k_size_in_bytes ){
                        if (!basisu::astc::decompress( block,&astc_data[i],false,block_width, block_height )){
//                            printf("decompress failed %s", path.c_str());
                        }
                        int block_x = block_index % blocks_wide;
                        int block_y = block_index / blocks_wide;
                        for (uint8_t j = 0;j<block_height;++j){
                            int py = block_height * block_y + j;
                            int px = block_width * block_x;
                            int dst_pixel_pos = (py * width + px) <<2;
                            int src_pixel_pos = (j * block_width) <<2;
                            memcpy(&img_data[dst_pixel_pos],&block[src_pixel_pos], row_length);
                        }
                        ++block_index;
                    }

img_data 和 size_rgba 就是转码后的RGBA数据内容

```
