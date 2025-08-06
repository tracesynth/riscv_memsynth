import os

def check_last_line(folder_path):
    success_files = []
    non_success_files = []
    non_pass_files = []
    with open('litmus_final_allow.txt') as f:
        results = {}
        contents = f.readlines()
        for content in contents:
            path, result = content.strip().split(',')[0], content.strip().split(',')[1]
            litmus = path.split('/')[-1]
            results[litmus] = int(result)
    for root, dirs, files in os.walk(folder_path):
        for filename in files:
            file_path = os.path.join(root, filename)
            name = filename[:-4]
            # print(name)
            try:
                with open(file_path, 'r') as f:
                    lines = f.readlines()
                    if not lines:
                        non_success_files.append(file_path)
                        continue
                    last_line = lines[-1].strip()
                    if last_line in {'#f', '#t'}:
                        if last_line == '#f' and results[name] == 0:
                            success_files.append(file_path)
                        if last_line == '#f' and results[name] == 1:
                            non_pass_files.append(file_path)
                        if last_line == '#t' and results[name] == 0:
                            non_pass_files.append(file_path)
                        if last_line == '#t' and results[name] == 1:
                            success_files.append(file_path)
                    else:
                        non_success_files.append(file_path)
            except Exception as e:
                print(f"Error reading {file_path}: {e}")
                

    return success_files, non_pass_files, non_success_files

if __name__ == "__main__":
    folder = "./output_final_allow"  # 可以改成你要遍历的目录
    success, non_pass, non_success = check_last_line(folder)
    with open('analysis_final_allow.txt','w') as wf:

        wf.write("✅ Success Files:\n")
        for f in success:
            wf.write(f)
            wf.write('\n')

        wf.write("\n❌ Non-pass Files:\n")
        for f in non_pass:
            wf.write(f)
            wf.write('\n')

        wf.write("\n❌ Non-Success Files:\n")
        for f in non_success:
            wf.write(f)
            wf.write('\n')
            # wf.write('---------------------------------------------------------------------------------\n')
            # with open(f,'r') as rf:
            #     wf.write(rf.read())
            # wf.write('---------------------------------------------------------------------------------\n')