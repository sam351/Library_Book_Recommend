# setwd('')
rm(list = ls())

library(dplyr)
library(ggplot2)

# 1. 데이터 수집 - 크롤링한 파일 불러오기 (아리랑어린이, 월곡꿈그린)
src = './data/2019_Dec_SeongbookLibs/'
src_file_vec = list.files(src)
src_file_vec = src_file_vec[c(6,7)]


# 2. 데이터 전처리 - 각 도서관 내에서 부가기호(0~9) 별 '미대출 도서 비율' 산정
## 최종 결과 리스트로 저장
## 산정 시 도서권수 = 0 인 도서 제외 (이상치 제거)
res_list = list()
lib_names = substr( src_file_vec, 1, 
                    regexpr('도서관',src_file_vec)-1 )

load_col = c('도서권수','부가기호','대출건수')
for (idx in 1:length(src_file_vec)) {
  tmp_file = paste0(src, src_file_vec[idx])
  
  tmp_df = read.csv(tmp_file, stringsAsFactors = F)[load_col]
  tmp_df = tmp_df %>% filter(도서권수 != 0) %>% select(-도서권수)  # 이상치
  
  ## 부가기호 별 미대출 도서 비율 계산
  topic_nums = c()
  tmp_percnt_vec = c()
  for (topic_n in 0:9) {
    topic_tmp_df = tmp_df %>% filter(부가기호 == topic_n) %>% select(-부가기호)
    tmp_cnt_vec = topic_tmp_df$대출건수
    tmp_percnt = length(tmp_cnt_vec[tmp_cnt_vec==0]) / length(tmp_cnt_vec) * 100
    
    tmp_percnt_vec = c(tmp_percnt_vec, tmp_percnt)
    topic_nums = c(topic_nums, topic_n)
  }
  
  noLoans_df = data.frame( topic_n = as.character(topic_nums), noLoan_percnt = tmp_percnt_vec )
  res_list[[lib_names[idx]]] = noLoans_df
}

res_list$아리랑어린이 %>% arrange(desc(noLoan_percnt))
res_list$월곡꿈그림 %>% arrange(desc(noLoan_percnt))



# 3. 데이터 시각화 - 도서관 별 미대출 아동 도서 비율 (상위 5개 도서관)
plot_df_1 = res_list$아리랑어린이
plot_df_2 = res_list$월곡꿈그림

ggplot(data = plot_df_1, aes(x=topic_n, y=noLoan_percnt)) +
  geom_col() +
  labs(title="아리랑어린이도서관 부가기호별 미대출 도서 비율",
       y = '미대출 도서 비율(%)',
       x = '부가기호') +
  ylim(0, max(plot_df_1$noLoan_percnt, na.rm = T)+10)

ggplot(data = plot_df_2, aes(x=topic_n, y=noLoan_percnt)) +
  geom_col() +
  labs(title="월곡꿈그린도서관 부가기호별 미대출 도서 비율",
       y = '미대출 도서 비율(%)',
       x = '부가기호') +
  ylim(0, max(plot_df_1$noLoan_percnt, na.rm = T)+10)
