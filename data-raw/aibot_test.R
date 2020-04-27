library(tsai)

bb <- aibot('发现神行多少行',3)
#print(bb)
View(bb)

type =aibot_answer_type(bb,min = 0.9,high=0.95)
print(type)



res <- ai(keyword = '发现多少钱',n = 3,low = 0.8,high = 0.95,detail = T
            )
print(res)


res2 <- ai(keyword = '发现多少钱',n = 3,low = 0.9,high = 0.95,detail = T
)
print(res2)

res3 <- ai(keyword = '发现多少钱',n = 3,low = 0.6,high = 0.8,detail = T
)
print(res3)

tip <- ai_tip('发现神行什么配置',4)
print(tip)



