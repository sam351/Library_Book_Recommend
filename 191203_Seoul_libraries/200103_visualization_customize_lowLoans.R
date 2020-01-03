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
                    regexpr(' ',src_file_vec)-1 )
noLoans_df = data.frame( lib_name = lib_names,
                         noLoan_percnt = noLoan_percnt_vec)


# 4. 데이터 시각화 - 도서관 별 미대출 도서 비율
## version1 - vertical bar
plot_df = noLoans_df %>% arrange(desc(noLoan_percnt)) %>% head(5)

ggplot(data = plot_df, 
       aes(x=reorder(lib_name, -noLoan_percnt), y=noLoan_percnt)) +
  geom_col( fill = c('red','red',rep('gray',3)) ) +
  geom_text(aes( label=paste0(round(noLoan_percnt, 1), '%') ), 
            vjust=-0.3, size=5) +
  labs(title="2019년 12월 성북구 도서관별 미대출 도서 비율 (상위 5개 도서관)", y = '미대출 도서 비율(%)', x = '') +
  ylim(0, 40) + 
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 35, alpha = .2, fill="red") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        axis.title.y = element_text(face="bold", size = 15, color = "black"),
        axis.text.x = element_text(face="bold", color="#0040ff",size = 15),
        axis.text.y = element_text(color="#0040ff"),
        
        plot.margin = margin(10,10,10,10),
        panel.background = element_rect(fill = "white",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightblue"))
# ggsave("200103_19년 12월 성북구 도서관별 미대출 도서 비율.png", width = 9, height = 6)


## version2 - horizontal bar
plot_df2 = noLoans_df %>% arrange(desc(noLoan_percnt)) %>% head(10)
ggplot(data = plot_df2, 
       aes(x=reorder(lib_name, -noLoan_percnt), y=noLoan_percnt,
           fill = noLoan_percnt)) +
  geom_col() + ylim(0,35) +
  scale_fill_gradient(low="gray", high="red") +
  geom_text(aes(label = paste0(round(noLoan_percnt, 1), '%')), hjust=-0.2, size=5) +
  
  labs(title="2019년 12월 성북구 도서관별\n미대출 도서 비율 (상위 10개 도서관)",
       y = '미대출 도서 비율(%)', x="") +
  
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        
        axis.title.x = element_text(face="bold", size = 15, color = "black"),
        axis.text.y = element_text(face="bold", color="#0040ff",size = 15),
        axis.text.x = element_text(color="#0040ff"),
        
        plot.margin = margin(10,10,10,10),
        panel.background = element_rect(fill = "white",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightblue")) +
  coord_flip()
  # + annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 35, alpha = .2, fill="red") 

# ggsave("200103_19년 12월 성북구 도서관별 미대출 도서 비율_barh.png", width = 9, height = 6)

