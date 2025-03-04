library(progmod)
library(dplyr)

mydata=read.csv("C:\")
# 先清理数据
mydata_clean <- mydata[complete.cases(mydata[, c("MoCA", "updrs3", "updrs1")]), ]

# 第一张图====================================================================================================
if (require(ggplot2)) {
  ggplot(mydata, aes(x = time, y = updrs3)) +
    geom_line(aes(group = PATNO, color = side)) +
    ylim(c(75,0)) +
    xlab('time-updrs3')
}


# 简单模型====================================================================================================
data1 <- mydata_clean %>%
  select(PATNO, updrs3, time, MoCA,blstatus)

# 转换数据类型
data1$time<-as.numeric(data1$time)
data1$PATNO<-as.factor(data1$PATNO)

# 修正初始值
fixed_start_coef <- c(MoCA = 10000)  # 为每个固定效应指定初始值
updrs3_progmod <- progmod(
  updrs3 ~ time,
  data = subset(data1, !is.na(updrs3)),
  fixed = list(time ~ MoCA - 1),
  random = time ~ 1 | PATNO,
  start = fixed_start_coef,
  verbose = TRUE
)

summary(updrs3_progmod)

# Predict from model and visualize results
data1$fixed_shift_updrs3 <- with(data1,
                                        MoCA * fixed.effects(updrs3_progmod)[1] )

pred_rand <- random.effects(updrs3_progmod)


data1$random_shift_updrs3 <- pred_rand[match(data1$PATNO, rownames(pred_rand)), 's.(Intercept)']

if (require(ggplot2)) {
  ggplot(data1, aes(x = time + fixed_shift_updrs3, y = updrs3)) +
    geom_line(aes(group = PATNO, color = blstatus)) +
    ylim(c(75, 0)) +
    xlab('Time')
}

data2 <- mydata_clean %>%
  select(PATNO, updrs3, time, updrs1,blstatus)
# 修正初始值
fixed_start_coef <- c(updrs1 = 10000)  # 为每个固定效应指定初始值
updrs3_progmod <- progmod(
  updrs3 ~ time,
  data = subset(data2, !is.na(updrs3)),
  fixed = list(time ~ updrs1 - 1),
  random = time ~ 1 | PATNO,
  start = fixed_start_coef,
  verbose = TRUE
)

summary(updrs3_progmod)

# Predict from model and visualize results
data2$fixed_shift_updrs3 <- with(data2,
                                 updrs1 * fixed.effects(updrs3_progmod)[1] )

pred_rand <- random.effects(updrs3_progmod)


data2$random_shift_updrs3 <- pred_rand[match(data2$PATNO, rownames(pred_rand)), 's.(Intercept)']

if (require(ggplot2)) {
  ggplot(data2, aes(x = time + fixed_shift_updrs3, y = updrs3)) +
    geom_line(aes(group = PATNO, color = blstatus)) +
    ylim(c(75, 0)) +
    xlab('Time')
}
# ============================================================================================================================
# 第二张图=======================================================================================================================
# 修正初始值

# 调试
data3=mydata_clean %>%
  select(PATNO, updrs3, time, right,blstatus)

fixed_start_coef <- c(l = 5, s.right = 10, g = 5, v = 5)

# 检查数据并确保 updrs3、MoCA、updrs1 和 PATNO 列存在，并且没有缺失值。
updrs3_progmod <- progmod(
  updrs3 ~ exp_model(time, l, s, g, v), # 模型公式，使用时间作为主要自变量
  data = subset(data3, !is.na(updrs3)), # 过滤掉 updrs3 中的缺失值
  fixed = list(
    l ~ 1,                    # 参数 l 的固定效应：简单截距
    s ~ right,    # 参数 s 的固定效应：根据 MoCA 和 updrs1 调整
    g ~ 1,                    # 参数 g 的固定效应：简单截距
    v ~ 1                     # 参数 v 的固定效应：简单截距
  ),
  random = s +v~ 1 | PATNO,     # 随机效应：针对 s 使用 PATNO 分组
  start = fixed_start_coef,   # 初始参数估计值
  covariance = NULL           # 未定义额外的协方差结构
)



data3=mydata_clean %>%
  select(PATNO, updrs3, time, left,blstatus)

fixed_start_coef <- c(l = 1, s.left = 10, g = 5, v = 10)

# 检查数据并确保 updrs3、MoCA、updrs1 和 PATNO 列存在，并且没有缺失值。
updrs3_progmod <- progmod(
  updrs3 ~ exp_model(time, l, s, g, v), # 模型公式，使用时间作为主要自变量
  data = subset(data3, !is.na(updrs3)), # 过滤掉 updrs3 中的缺失值
  fixed = list(
    l ~ 1,                    # 参数 l 的固定效应：简单截距
    s ~ left -1,    # 参数 s 的固定效应：根据 MoCA 和 updrs1 调整
    g ~ 1,                    # 参数 g 的固定效应：简单截距
    v ~ 1                     # 参数 v 的固定效应：简单截距
  ),
  random = s +v~ 1 | PATNO,     # 随机效应：针对 s 使用 PATNO 分组
  start = fixed_start_coef,   # 初始参数估计值
  covariance = NULL           # 未定义额外的协方差结构
)


# 查看模型结果
summary(updrs3_progmod)

# Predict from model and visualize results
data3$fixed_shift_updrs3 <- with(data3,
                                        MoCA * fixed.effects(updrs3_progmod)[2] +
                                          updrs1 * fixed.effects(updrs3_progmod)[3])

pred_rand <- random.effects(updrs3_progmod)


data3$random_shift_updrs3 <- pred_rand[match(data3$PATNO, rownames(pred_rand)), 's.(Intercept)']

if (require(ggplot2)) {
  ggplot(data3, aes(x = time + fixed_shift_updrs3, y = updrs3)) +
    geom_line(aes(group = PATNO, color = blstatus)) +
    ylim(c(75, 0)) +
    xlab('Time')
}

# 第三张图====================================================================================================
if (require(ggplot2)) {
  ggplot(data3, aes(x = time + fixed_shift_updrs3 + random_shift_updrs3, y = updrs3)) +
    geom_line(aes(group = PATNO, color = blstatus)) +
    ylim(c(75, 0)) +
    xlab('Months since baseline')
}
