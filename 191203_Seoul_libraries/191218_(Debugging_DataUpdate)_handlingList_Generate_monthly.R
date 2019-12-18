# < 대목표 : 이중 리스트(도서관-연월)에서, 각 도서관의 월별 df(누적 대출건수)로부터, 각 도서관의 월별 df(월간 대출건수) 생성
## 1차 작업(현재 파일) : (K월 건수 - K-1월 건수)를 통해 월간 건수를 담은 이중 리스트 생성
## < 위 작업에서 성북구 디버깅&데이터 업데이트 - 결측치(NA) 채우기

setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')

rm(list = ls())
file_name = '2018 seongbook library monthly data (성북정보, 아리랑정보 제외할 것).rds'
sb_monthly_list = readRDS(file_name)  # 수정할 데이터

file_name2 = '2018 seongbook library cumsum data (updated 191218).rds'
sb_cumsum_list = readRDS(file_name2)


# 성북정보도서관, 아리랑정보도서관에서 수정된 sb_cumsum_list 활용해 sb_monthly_list 수정

for (tmp_lib in c("성북정보도서관", "아리랑정보도서관")) {
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
      if (idx%%50000==0) {
        cat(tmp_lib, month_name, idx, '번째 도서 대출건수 계산 완료\n')
      }
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
  
  
  sb_monthly_list[[tmp_lib]] = res_lib_list
  cat(tmp_lib, '통합 완료\n')
}

View(sb_monthly_list)



# 2. RDS 파일로 저장 & 불러와서 확인
new_file_name = '2018 seongbook library monthly data (updated 191218).rds'
saveRDS(sb_monthly_list, file = new_file_name)

sb_monthly_list_check = readRDS(new_file_name)
identical(sb_monthly_list, sb_monthly_list_check)  # 정상이면 TRUE
View(sb_monthly_list_check)




# ----------------- 지워 -------------------





list_len = length(tmp_list)
str(tmp_list[[ 12-list_len+2 ]])
str(tmp_list[[ 12 ]])

# 1. 
## 2월~12월 혹은 8월~12월 동안 반복
for (idx in c( (12-list_len+2) : 12)) {
  
}




