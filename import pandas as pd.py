import pandas as pd
import json

# 读取 CSV
df = pd.read_csv("D:\Econ_5029_data\Econ-5880W-data-analysis\Econ 5880w data rought cleaning\canadian_election.csv")

# 转换格式
train_data = []
for _, row in df.iterrows():
    train_data.append({
        "instruction": row["question"],
        "input": row["context"],
        "output": row["answer"]
    })

# 保存 JSON
with open("train_data.json", "w", encoding="utf-8") as f:
    json.dump(train_data, f, ensure_ascii=False, indent=4)

print("CSV 数据转换完成！")
