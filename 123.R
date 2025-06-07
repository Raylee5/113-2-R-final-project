# -- 第一步：載入 tidyverse 套件（如果尚未安裝請先 install.packages("tidyverse")） --
library(tidyverse)

# -- 第二步：讀取 CSV 檔案（請確認檔案與這份 R 檔在同一資料夾） --
df <- read_csv("a05038102-2143395063.csv", locale = locale(encoding = "UTF-8"))

# -- 第三步：轉換為長格式（寬轉長） --
df_long <- df %>%
  pivot_longer(
    cols = c("研究所[人]", "大專校院[人]", "中等學校[人]", "軍警學校[人]"),
    names_to = "學歷類別",
    values_to = "人數"
  )

# -- 第四步：畫出每一屆的學歷圓餅圖，並儲存為 PNG 圖片 --
# 建立資料夾
if (!dir.exists("pie_charts")) dir.create("pie_charts")

# 遍歷每一屆期別
unique(df_long$`選舉屆期別`) %>% walk(~{
  屆期 <- .x
  
  df_subset <- df_long %>%
    filter(`選舉屆期別` == 屆期)
  
  p <- ggplot(df_subset, aes(x = "", y = 人數, fill = 學歷類別)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = paste0(屆期, " 學歷分布圓餅圖")) +
    theme_void() +
    theme(legend.position = "right")
  
  ggsave(filename = paste0("pie_charts/pie_", 屆期, ".png"),
         plot = p, width = 6, height = 6, dpi = 300)
})

# -- 第五步：計算比例，用於歷屆總分析（折線圖） --
df_total <- df %>%
  select(`選舉屆期別`, 總計 = `總計[人]`)

df_long_pct <- df_long %>%
  left_join(df_total, by = "選舉屆期別") %>%
  mutate(比例 = 人數 / 總計)

# 使用這個順序建立 factor
df_long <- df_long %>%
  mutate(`選舉屆期別` = factor(`選舉屆期別`, levels = 屆數順序))

df_long_pct <- df_long_pct %>%
  mutate(`選舉屆期別` = factor(`選舉屆期別`, levels = 屆數順序))

# 提取數字排序（進階用法）
屆數順序 <- df %>%
  distinct(`選舉屆期別`) %>%
  mutate(數字 = readr::parse_number(`選舉屆期別`)) %>%
  arrange(數字) %>%
  pull(`選舉屆期別`)

# 使用這個順序建立 factor
df_long <- df_long %>%
  mutate(`選舉屆期別` = factor(`選舉屆期別`, levels = 屆數順序))

df_long_pct <- df_long_pct %>%
  mutate(`選舉屆期別` = factor(`選舉屆期別`, levels = 屆數順序))

# -- 第六步：畫出歷屆學歷比例變化折線圖 --
p2 <- ggplot(df_long_pct, aes(x = `選舉屆期別`, y = 比例, color = 學歷類別, group = 學歷類別)) +
  geom_line(linewidth = 1.2)
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "歷屆候選人學歷比例變化",
    x = "選舉屆期別",
    y = "比例",
    color = "學歷"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 顯示圖
print(p2)

# 儲存折線圖
ggsave("pie_charts/歷屆學歷比例變化.png", plot = p2, width = 8, height = 6, dpi = 300)
# 

