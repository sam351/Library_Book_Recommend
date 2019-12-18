# < 대목표 : 이중 리스트(도서관-연월)에서, 각 도서관의 월별 df(누적 대출건수)로부터, 각 도서관의 월별 df(월간 대출건수) 생성
## 1차 작업(현재 파일) : (K월 건수 - K-1월 건수)를 통해 월간 건수를 담은 이중 리스트 생성
## 2차 작업(추후 타진) : 가능하다면, 한 도서관 리스트 내의 전체 df 6~12개를 full_join해 하나의 df로 통합 → 구내 도서관 별 리스트에 통합 df 저장
setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')

library(dplyr)
gu = 'seongbook'
file_name = paste('2018', gu, 'library cumsum data.rds')
sb_cumsum_list = readRDS(file_name)


# 0. sampling - 하나의 도서관(아리랑어린이)에서, 한 월에 대한 월간 대출건수 df 생성
tmp_list = sb_cumsum_list$아리랑어린이

month = 2
tmp_df = tmp_list[[month]]  # 1월은 전월 데이터 없으므로 건너뜀
tmp_df_pre = tmp_list[[month-1]]  # K월 df에서 빼주기 위해 K-1월 df 로드

books = tmp_df$도서명
cnts = rep(0, length(books))  # 도서명 수만큼 대출건수 기본값(0) 세팅
for (idx in 1:length(books)) {
  book = books[idx]
  if (book %in% tmp_df_pre$도서명) {
    cnts[idx] = tmp_df$대출건수[ tmp_df$도서명 == book ] - tmp_df_pre$대출건수[ tmp_df_pre$도서명 == book ]
  }
  
  # if (idx%%10000==0) {
  #   cat(idx, '번째 도서 계산 완료')
  # }
}
tmp_monthly_df = data.frame( 도서명 = books, 대출건수 = cnts, stringsAsFactors=F )
head(tmp_monthly_df)

identical( tmp_monthly_df$도서명, tmp_df$도서명 )  # 기존 df와 도서목록 같은지 확인 - 정상이면 TRUE
any( tmp_monthly_df$대출건수 > tmp_df$대출건수 )  # 월간 대출건수가 누적 대출건수보다 많은 월이 있는지(이상치 존재 여부) 확인 - 정상이면 FALSE



# 0. sampling2 - 하나의 도서관(아리랑어린이) 3개월치 월간 대출건수 df 생성
res_lib_list = list()

for (list_idx in 4:length(tmp_list)) {   # 첫 월은 전월 데이터 없으므로 건너뜀
  
  month_name = names(tmp_list)[list_idx]
  
  tmp_df = tmp_list[[list_idx]]
  tmp_df_pre = tmp_list[[list_idx-1]]    # K월 df에서 빼주기 위해 K-1월 df 로드
  
  books = tmp_df$도서명
  cnts = rep(0, length(books))  # 도서명 수만큼 대출건수 기본값(0) 세팅
  for (idx in 1:length(books)) {
    book = books[idx]
    
    if (book %in% tmp_df_pre$도서명) {
      cnts[idx] = tmp_df$대출건수[ tmp_df$도서명 == book ] - tmp_df_pre$대출건수[ tmp_df_pre$도서명 == book ]
    }
    
    # check
    if (idx%%10000==0) {
      cat(idx, '번째 도서 계산 완료')
    }
    # check
  }
  
  tmp_monthly_df = data.frame( 도서명 = books, 대출건수 = cnts, stringsAsFactors=F )
  res_lib_list[[month_name]] = tmp_monthly_df
  
  # check
  flag1 = identical( tmp_monthly_df$도서명, tmp_df$도서명 )  # 정상이면 TRUE
  flag2 = any( tmp_monthly_df$대출건수 > tmp_df$대출건수 )  # 정상이면 FALSE
  cat(month_name, '완료', flag1, flag2, '\n')
  # check
}



# 1. scale-up - 성북구 전체 리스트에 적용
rm(list = ls())
gu = 'seongbook'
file_name = paste('2018', gu, 'library cumsum data.rds')
sb_cumsum_list = readRDS(file_name)


final_list = list()

for (tmp_lib in names(sb_cumsum_list)) {    # 도서관 이름 수만큼 반복
  tmp_list = sb_cumsum_list[[tmp_lib]]  # 도서관 A의 리스트
  
  
  res_lib_list = list()
  
  for (list_idx in 2:length(tmp_list)) {   # 첫 월은 전월 데이터 없으므로 건너뜀
    
    month_name = names(tmp_list)[list_idx]
    
    tmp_df = tmp_list[[list_idx]]
    tmp_df_pre = tmp_list[[list_idx-1]]    # K월 df에서 빼주기 위해 K-1월 df 로드
    
    books = tmp_df$도서명
    cnts = rep(0, length(books))  # 도서명 수만큼 대출건수 기본값(0) 세팅
    for (idx in 1:length(books)) {
      book = books[idx]
      
      if (book %in% tmp_df_pre$도서명) {
        cnts[idx] = tmp_df$대출건수[ tmp_df$도서명 == book ] - tmp_df_pre$대출건수[ tmp_df_pre$도서명 == book ]
      }
      
      # # check
      # if (idx%%50000==0) {
      #   cat(tmp_lib, month_name, idx, '번째 도서 대출건수 계산 완료\n')
      # }
      # # check
    }
    
    tmp_monthly_df = data.frame( 도서명 = books, 대출건수 = cnts, stringsAsFactors=F )
    res_lib_list[[month_name]] = tmp_monthly_df
    
    # check
    flag1 = identical( tmp_monthly_df$도서명, tmp_df$도서명 )  # 정상이면 TRUE
    flag2 = any( tmp_monthly_df$대출건수 > tmp_df$대출건수 )  # 정상이면 FALSE
    cat(tmp_lib, month_name, '완료', flag1, flag2, '\n')
    # check
  }
  
  
  final_list[[tmp_lib]] = res_lib_list
  cat(tmp_lib, '통합 완료\n')
}

View(final_list)



# 2. RDS 파일로 저장 & 불러와서 확인
new_file_name = paste('2018', gu, 'library monthly data.rds')
saveRDS(final_list, file = new_file_name)

final_list_check = readRDS(new_file_name)
identical(final_list, final_list_check)  # 정상이면 TRUE
View(final_list_check)

