setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')
library(dplyr)

# 성북구 도서관 월별 대출 데이터(by 승우씨) 로드
rm(list = ls())
sb_cumsum_list = readRDS('sb_lib_list2.rds')

# 기본 구성 확인
View(sb_cumsum_list)
tmp_df = sb_cumsum_list$달빛마루도서관[[1]]
head(tmp_df)
nrow(tmp_df)

# 결측치 확인
any(is.na(tmp_df))

for (tmp_lib_name in names(sb_cumsum_list)) {  # 정상이면 메세지 없음
  tmp_lib_list = sb_cumsum_list[[tmp_lib_name]]
  for (df in tmp_lib_list) {
    if (any(is.na(df))) {
      print('NA 발견')
    }
  }
}

# 도서관 A의 월별 도서 대출 데이터프레임 생성
tmp_lib_list = sb_cumsum_list$아리랑어린이도서관
tmp_df = tmp_lib_list[[1]]
tmp_df$도서명 = as.character(tmp_df$도서명)
dalbit_monthly_df = data.frame( tmp_lib_list[[1]], stringsAsFactors = F )

for (idx in 2:length(tmp_lib_list)) {
  tmp_df = tmp_lib_list[[idx]]
  tmp_df$도서명 = as.character(tmp_df$도서명)
  dalbit_monthly_df = full_join(dalbit_monthly_df, tmp_df, by='도서명')
}

str(dalbit_monthly_df)
head(dalbit_monthly_df, 1)
table(is.na(dalbit_monthly_df))

length(colnames(dalbit_monthly_df))
colnames(dalbit_monthly_df) <- c('도서명', paste0(2:12, '월'))
head(dalbit_monthly_df)

View(dalbit_monthly_df)



# 원 데이터(tmp_lib_list)에서 결측치 확인
tmp_lib_list[[5]] [tmp_lib_list[[5]]$도서명=='돈키호테',]
tmp_lib_list[[6]] [tmp_lib_list[[6]]$도서명=='돈키호테',]
tmp_lib_list[[7]] [tmp_lib_list[[7]]$도서명=='돈키호테',]

