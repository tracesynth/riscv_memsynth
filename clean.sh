# 删除所有缓存的字节码文件
find . -name "*.zo" -delete
find . -name "*.dep" -delete

# 如果你把 rosette 复制/链接到了 memsynth/rosette 目录，也要清除它的缓存
find ./rosette -name "*.zo" -delete
find ./rosette -name "*.dep" -delete

