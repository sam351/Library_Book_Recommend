# install.packages("qpcR")
library(qpcR)

# 1. 아리랑도서관 2020년 1월 추천도서 목록 csv 생성
arirang_list = readRDS('data/200103_19년12월_아리랑어린이도서관_인기도서별 추천도서 리스트.rds')

arirang_df = data.frame(dummy=1:10)
col_vec = c()
for (tmp_list in arirang_list) {
  col_vec = c(col_vec, tmp_list$recommend_books[1])
  arirang_df = qpcR:::cbind.na(arirang_df, tmp_list$recommend_books[-1])
}
arirang_df = arirang_df[-1]
colnames(arirang_df) = col_vec
# View(arirang_df)
write.csv(arirang_df, 
          'data/200103_2020년1월_아리랑어린이도서관_인기 아동도서별 추천도서 목록.csv',
          row.names=FALSE)



# 2. 월곡꿈그린도서관 2020년 1월 추천도서 목록 csv 생성
wolkok_list = readRDS('data/200103_19년12월_월곡꿈그림도서관_인기도서별 추천도서 리스트.rds')

wolkok_df = data.frame(dummy=1:10)
col_vec = c()
for (tmp_list in wolkok_list) {
  col_vec = c(col_vec, tmp_list$recommend_books[1])
  wolkok_df = qpcR:::cbind.na(wolkok_df, tmp_list$recommend_books[-1])
}
wolkok_df = wolkok_df[-1]
colnames(wolkok_df) = col_vec
# View(wolkok_df)
write.csv(wolkok_df, 
          'data/200103_2020년1월_월곡꿈그림도서관_인기 아동도서별 추천도서 목록.csv',
          row.names=FALSE)

