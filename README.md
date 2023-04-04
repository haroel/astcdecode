# astcdecode
ASTC to RGBA

ASTC压缩纹理软解，基于 https://github.com/richgel999/astc_dec


我在此基础上做了一定性能计算优化。
1280x720ASTC纹理，原始解码时长950毫秒左右，经优化后时长740毫秒左右（测试设备Honor 7X）



GET STARTED

```
  uint32_t width;
  uint32_t height;
  uint8_t* data_rgba = nullptr;
  uint32_t size_rgba = basisu::astc::transferToRGBA(filedata, dataLen,&data_rgba,&width,&height);
  
  
data_rgba 和 size_rgba 就是转码后的RGBA数据

```
