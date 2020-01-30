# setwd('')
rm(list = ls())

library(dplyr)
library(ggplot2)

# 1. 데이터 수집 - 크롤링한 파일 불러오기
src = './data/2019_Dec_SeongbookLibs/'
src_file_vec = list.files(src)


# 2. 데이터 전처리 - 도서관 별로 '미대출 도서 비율' 산정
## 산정 시 도서권수 = 0 인 도서 제외 (이상치 제거)
noLoan_percnt_vec = c()
load_col = c('도서권수','대출건수')
for (tmp_file in src_file_vec) {
  tmp_file = paste0(src, tmp_file)
  
  tmp_df = read.csv(tmp_file, stringsAsFactors = F)[load_col]
  tmp_df = tmp_df %>% filter(도서권수 != 0) %>% select(-도서권수)  # 이상치
  loan_cnt_vec = tmp_df$대출건수
  
  tmp_noLoan_percnt = length(loan_cnt_vec[loan_cnt_vec==0]) / length(loan_cnt_vec) * 100
  
  noLoan_percnt_vec = c(noLoan_percnt_vec, tmp_noLoan_percnt)
}


# 3. 데이터 전처리 - 도서관 별 '미대출 도서 비율' df 생성
lib_names = substr( src_file_vec, 1, 
                    regexpr('도서관',src_file_vec)-1 )
noLoans_df = data.frame( lib_name = lib_names,
                         noLoan_percnt = noLoan_percnt_vec)


# 4. 데이터 시각화 - 도서관 별 미대출 아동 도서 비율 (상위 5개 도서관)
plot_df = noLoans_df %>% arrange(desc(noLoan_percnt)) %>% head(5)

ggplot(data = plot_df, 
       aes(x=reorder(lib_name, -noLoan_percnt), y=noLoan_percnt)) +
  geom_col() +
  labs(title="성북구 도서관별 미대출 도서 비율 (상위 5개 도서관)",
       y = '미대출 아동 도서 비율(%)',
       x = '도서관') +
  ylim(0, 40)

