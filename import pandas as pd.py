import pandas as pd
import re
import nltk
from nltk.corpus import stopwords
from wordcloud import WordCloud
import matplotlib.pyplot as plt
from langdetect import detect
from deep_translator import GoogleTranslator


# 下载停用词
nltk.download("stopwords")
stop_words_en = set(stopwords.words("english"))
stop_words_fr = set(stopwords.words("french"))
stop_words = stop_words_en.union(stop_words_fr)  # 合并英语和法语停用词


# 读取数据
df = pd.read_csv("D:\Econ_5029_data\Econ-5880W-data-analysis\Econ 5880w data rought cleaning\canadian_election.csv")

text_column = "cps21_imp_loc_iss"
text_data = df[text_column].dropna()  # 移除空值

# 文本清理函数
def clean_text(text):
    text = text.lower()  # 转小写
    text = re.sub(r'[^\w\s]', '', text)  # 去除标点
    text = " ".join([word for word in text.split() if word not in stop_words])  # 过滤停用词
    return text

# 处理整个数据集
text_data = text_data.apply(clean_text)

# 合并所有文本
all_text = " ".join(text_data)

# 生成词云
wordcloud = WordCloud(width=800, height=400, background_color="white", colormap="coolwarm").generate(all_text)

# 显示词云
plt.figure(figsize=(10, 5))
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")  # 不显示坐标轴
plt.show()
