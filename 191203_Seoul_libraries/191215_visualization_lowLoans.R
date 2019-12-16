# 1. 성북구 도서관 시각화
load('//70.12.116.50/교육자료/R자료/6조/2018년 성북구 도서관별 대출목록.RData')

str(sb_lib_list$달빛마루도서관)
# temp_list = sb_lib_list$달빛마루도서관
# temp_df_begin = temp_list[[1]]
# temp_df_end = temp_list[[ length(temp_list) ]]
# length(temp_df_begin[ temp_df_begin['대출건수']<1 ]); length(temp_df_begin$도서명)
# length(temp_df_end[ temp_df_end['대출건수']<1 ]); length(temp_df_end$도서명)

ratio_vec = c()
for (lib in names(sb_lib_list)) {
  temp_list = sb_lib_list[[lib]]
  temp_df = temp_list[[ length(temp_list) ]]
  
  temp_ratio = length( temp_df$도서명[ temp_df['대출건수'] < 3 ] ) / length(temp_df$도서명) * 100
  ratio_vec = c(ratio_vec, temp_ratio)
}
ratio_vec; names(sb_lib_list)

big_ratio_vec = ratio_vec[ratio_vec > 20]
names = names(sb_lib_list)[ratio_vec > 20]

# names = substr( names, 1, 5 ); length(unique(names)) == length(names)
big_ratio_vec; names

barplot(big_ratio_vec ~ names,
        main = "성북구 도서관별 버려진 도서 비율 (20% 이상)",
        xlab = "도서관", ylab = "버려진 도서 비율")




# 2. 도봉구 도서관 시각화
load('//70.12.116.50/교육자료/R자료/6조/2018년 도봉구 도서관별 대출목록.RData')

ratio_vec = c()
for (lib in names(gu_lib_list)) {
  temp_list = gu_lib_list[[lib]]
  temp_df = temp_list[[ length(temp_list) ]]
  
  temp_ratio = length( temp_df$도서명[ temp_df['대출건수'] < 3 ] ) / length(temp_df$도서명) * 100
  ratio_vec = c(ratio_vec, temp_ratio)
}
ratio_vec
names(gu_lib_list)

big_ratio_vec = ratio_vec[ratio_vec > 45]
names = names(gu_lib_list)[ratio_vec > 45]
# names = substr( names, 1, 7 ); length(unique(names)) == length(names)
big_ratio_vec; names

barplot(big_ratio_vec ~ names,
        main = "도봉구 도서관별 버려진 도서 비율 (45% 이상)",
        xlab = "도서관", ylab = "버려진 도서 비율")




# 3. 강북구 도서관 시각화
load('//70.12.116.50/교육자료/R자료/6조/2018년 강북구 도서관별 대출목록.RData')

ratio_vec = c()
for (lib in names(gu_lib_list)) {
  temp_list = gu_lib_list[[lib]]
  temp_df = temp_list[[ length(temp_list) ]]
  
  temp_ratio = length( temp_df$도서명[ temp_df['대출건수'] < 3 ] ) / length(temp_df$도서명) * 100
  ratio_vec = c(ratio_vec, temp_ratio)
}
ratio_vec; names(gu_lib_list)

big_ratio_vec = ratio_vec[ratio_vec > 20]
names = names(gu_lib_list)[ratio_vec > 20]

# names = substr( names(gu_lib_list), 1, 6 ); length(unique(names)) == length(names)
# big_ratio_vec; names

barplot(big_ratio_vec ~ names,
        main = "강북구 도서관별 버려진 도서 비율 (20% 이상)",
        xlab = "도서관", ylab = "버려진 도서 비율")
