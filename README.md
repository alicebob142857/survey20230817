问卷统计方法如下，辛苦大家了！
第9题答对的人太少了（难道是大家不知道质数是什么？），暂时不作为筛选问卷的标准，只要问卷大家觉得是认真填的就录上，
那些明显乱选（比如单选都选多选的，以及那些大家觉得很有问题的）可以不记录
- Q1
  - gender
  - 男：1 女：2 你猜你猜你猜：3
- Q2
  - grade
  - 初一：1 初二：2 初三：3 高一：4 高二：5 高三：6
- Q3
  - 对应选项不勾选则相对应的变量为0，否则
  - 数学物理：subject_mp：1
  - 化学生物：subject_cb：1
  - 语文英语：subject_ce：1
  - 政治历史：subject_ph：1
  - 其他：
    - 地理：1
    - 美术：2
    - 体育：3
    - 电脑（？）：4
- Q4
  - rank
  - 前10%：1 10%～50%：2 后50%：3
- Q5
  - ai_general
  - 一无所知：0 懂一点：1 很懂：2
- Q6
  - chatgpt_abbr
  - 答对：1 错：0
- Q7
  - chatgpt_usage
  - 答对：1 错：0
- Q8
  - ai_scene
  - 答对：1 错：0（只要选了百度地图，作业帮拍照，语音转换文字就算答对）
- Q9
  - reliability
  - 答对：1 错：0
- Q10
  - ai_concept
  - 答对：1 错：0
- Q11
  - ai_turing
  - 答对：1 错：0
- Q12
  - definition
  - 选2:2 选3:1 其他：0
- Q13
  - ai_alphago
  - 选1:2 选2：1 选3:0
- Q14
  - ai_commender
  - 答对：1 错：0
- Q15
  - cs_gpu
  - 答对：1 错：0
- Q16
  - cs_test
  - 答对：1 错：0
- Q17
  - cs_knowledge
  - 规则有点复杂：
    - 选5或者没选：0
    - 只选了1:1
    - 如果选了2，3，4中的一个，则基础分为1，选2+1分，选3+1分，选4+2分
    - 例子：某位同学选了2，3，那么基础分+1，选了2，3分别+1，共3分
    - 例子：某位同学只选了1，1分
- Q18
  - cs_virus
  - 答对：1 错：0
- Q19
  - cs_ctrlc
  - 答对：1 错：0
- Q20
  - cource
  - 答对：1 错：0
- Q21
  - interest
  - 按兴趣从小到大分别为1，2，3，4，5
- Q22
  - future_1~future6
  - 问题勾选后，相关变量为1，否则为0