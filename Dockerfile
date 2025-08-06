# 使用官方 Racket 镜像作为基础环境（包含 Racket 解释器）
FROM racket/racket:latest

# 安装系统工具：更新包索引并安装 make（用于运行测试）
RUN apt-get update && \
    apt-get install -y --no-install-recommends make && \
    rm -rf /var/lib/apt/lists/*

# 安装 Ocelot 包（自动包含 Rosette 等依赖）
RUN raco pkg install --auto ocelot

# 设置工作目录并复制 MemSynth 源码到容器中
# WORKDIR /memsynth
# COPY . /memsynth

# （可选）运行 MemSynth 的测试，确保一切正常
# 如果不希望在构建时运行测试，可以将以下行注释或移除
# RUN make test
