import os

# 设置文件路径
log_file_path = 'analysis_final.txt'              # 第一个文件：包含 Non-pass Files 和 ./output/*.log
start_file_path = 'litmus_final.txt'        # 第二个文件：每行一个路径+标志位
output_file_path = 'litmus_final1.txt'       # 输出文件

# 1. 提取 Non-pass 文件中出现的 .litmus 文件名
litmus_names = set()
start_collecting = False

with open(log_file_path, 'r') as f:
    for line in f:
        line = line.strip()
        if not start_collecting:
            if 'Non-pass Files' in line:
                start_collecting = True
            continue
        if line.startswith('./output_final/'):
            # 提取 `xxx.litmus.log` 的中间部分
            filename = os.path.basename(line)
            if filename.endswith('.litmus.log'):
                litmus_name = filename[:-4]  # 去掉 `.log`
                litmus_names.add(litmus_name)

# 2. 遍历 CSV 文件并匹配文件名
matched_lines = []

with open(start_file_path, 'r') as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        # 提取最后的 .litmus 名字
        path_part = line.split(',')[0]
        litmus_file = os.path.basename(path_part)  # 提取例如 2+2W+porlrls+NEW_3.litmus

        if litmus_file in litmus_names:
            matched_lines.append(line)

# 3. 写入输出文件
with open(output_file_path, 'w') as f:
    for line in matched_lines:
        f.write(line + '\n')

print(f'匹配完成，共输出 {len(matched_lines)} 条记录到 {output_file_path}')
