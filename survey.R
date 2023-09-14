library(readxl)
raw_data <- read_excel("survey.xlsx")

# 提取需要的数据
extracted_data <- subset(raw_data, select = grep("^[aci]", names(raw_data)))
names(extracted_data)

# 归一化
scaled_data <- scale(extracted_data)

# PCA
pca <- prcomp(scaled_data)
variance <- pca$sdev^2
variance / sum(variance)

# 降维图
library(ggplot2)
evals <- data.frame(variance)
names(evals)<-"eigen.vals"
evals$component.num<-as.integer(seq(nrow(evals)))
library(ggplot2)
png("降维图.png")
ggplot(evals,aes(x=component.num,y=eigen.vals))+geom_point()+ geom_hline(aes(yintercept=1), alpha = 0.65)
dev.off()

# 因子分析
library(psych)
library(GPArotation)
fit <-principal(scaled_data, nfactors=6, rotate="varimax", scores=T, method="Bartlett")
write.csv(fit$loadings, "fa_result6.csv")
scores <- fit$scores
write.csv(fit$scores, "fa_scores6.csv")

# future柱状图
library(dplyr)

future_data <- subset(raw_data, select = grep("^[f]", names(raw_data)))
count_score <- colSums(future_data)
name <- c("AI always\n threaten\n human", "Ai\n may be\n safe", "Ai cannot\n be better", "AI can\n be better", "AI causes\n unemployment", "No develop\n AI")
name <- factor(name, levels = name)
bar_data <- data.frame(Variable = name, Count = count_score)

library(ggplot2)

png("attitude.png")
ggplot(data=bar_data, aes(x = Variable, y = Count, fill = Variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue", "green", "yellow", "red", "red"))+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5) +
  labs(
    title = "Attitude toward AI' Future",
    x = "Opinions",
    y = "Percentage"
  ) +
  theme_minimal()
dev.off()

# 箱线图
score_data <- subset(raw_data, select = grep("^[ac]", names(raw_data)))
score_data <- scale(score_data)
total_score <- rowSums(score_data)
png("box_plot.png")
boxplot(total_score)
points(mean(total_score))
dev.off()

# 典型相关分析
rank_data <- subset(raw_data, select = grep("^rank", names(raw_data)))
rank_data <- scale(rank_data)
dim(score_data)
dim(rank_data)
ccres <- cancor(score_data, rank_data)
ccres$cor
ccres$xcoef
ccres$ycoef

# cs和ai相关知识
rank12_index <- which(raw_data$rank!=3)
dim(rank12_index)
rank3_index <- which(raw_data$rank==3)
dim(rank3_index)
median <- median(total_score)
rank12_score <- total_score[rank12_index]
rank3_score <- total_score[rank3_index]

bar_data <- c(sum(rank12_score >= median),
              sum(rank12_score < median),
              sum(rank3_score >= median),
              sum(rank3_score < median))
names <- c("top50%\n above median", "top 50%\n below median",
           "the last 50%\n above median", "the last 50%\n below median")  # 每个柱子的名称

# 将category变量转换为顺序型
names <- factor(names, levels = names)

png("rank_and_ai.png")
ggplot(data=data.frame(bar_data), aes(x = names, y = bar_data, fill = names)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue", "red", "blue"))+
  geom_text(aes(label=bar_data), vjust=-0.3, size=3.5) +
  labs(
    title = "rank and score",
    x = "rank",
    y = "counts"
  ) +
  theme_minimal()
dev.off()





