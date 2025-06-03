
#!/bin/bash

# 输入文件路径
input_file="/home/lipengwei2024phd/03GW_eu/02EukCC/eukcc.log"

# 初始化变量
best_taxid=""
completeness=""
contamination=""

# 打印表格头部
echo -e "Best TaxID\tCompleteness\tContamination"

# 逐行读取文件
while IFS= read -r line; do
    # 查找Best TaxID
    if [[ "$line" =~ "Best TaxID" ]]; then
        best_taxid=$(echo "$line" | grep -oP 'Best TaxID: \K[0-9]+')
    fi
    
    # 查找Completeness
    if [[ "$line" =~ "Completeness" ]]; then
        completeness=$(echo "$line" | grep -oP 'Completeness: \K[0-9.]+')
    fi

    # 查找Contamination
    if [[ "$line" =~ "Contamination" ]]; then
        contamination=$(echo "$line" | grep -oP 'Contamination: \K[0-9.]+')
    fi

    # 如果找到所有需要的字段，输出结果
    if [[ -n "$completeness" && -n "$contamination" && -n "$best_taxid" ]]; then
        echo -e "$best_taxid\t$completeness\t$contamination"
        # 重置变量以准备提取下一个模块的数据
        completeness=""
        contamination=""
        best_taxid=""
    fi
done < "$input_file"
