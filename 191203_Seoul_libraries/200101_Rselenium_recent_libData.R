# 패키지 로드, 기본 변수 생성
library(rvest)
library(RSelenium)
library(httr)

gu_vec = c('성북구')
remDr = remoteDriver(port=4445, browserName='chrome')

# 사용할 함수 2개 정의
## 함수(1) '장서/대출 데이터' 이동 > '지역' 서울, '세부지역(구)' gu_name 선택
reset_page_gu = function(gu_name='성북구') {
  url_main = 'https://www.data4library.kr/openDataL'
  remDr$navigate(url_main)
  Sys.sleep(1)
  
  tmp_options = remDr$findElement('css', '#p_region')
  tmp_options$sendKeysToElement(list('서울'))
  Sys.sleep(1)
  
  tmp_options = remDr$findElement('css', '#dtl_region')
  tmp_options$sendKeysToElement(list(gu_name))
  Sys.sleep(1)
}

## 함수(2) 최근 대출 데이터 다운로드 (csv)
download_recent_file = function() {
  remDr$executeScript(script = "window.scrollTo(0, 400);")
  Sys.sleep(0.5)
  
  tmp_btn = remDr$findElement('css','#sb-site > section > div.sub_container > div.notice_wrap.mgb_20 > table > tbody > tr:nth-child(1) > td.data_type.br_none > a.download_link.text_type')
  tmp_btn$clickElement()
}




# 1. 지역, 구 선택(반복문) > 각 도서관 방문(반복문) >  최근 파일 다운로드(함수)
remDr$open()
Sys.sleep(1)

for (gu in gu_vec) {
  reset_page_gu(gu)
  
  ## 해당 세부지역(구) 마지막 페이지로 이동
  remDr$executeScript(script = "window.scrollBy(0, 600);")
  tmp_btn = remDr$findElement('css','#pagef > div.paging_nav > a.page_select.last_page')
  tmp_btn$clickElement()
  
  ## 해당 세부지역(구) 페이지 수 수집 (= 마지막 페이지 숫자)
  remDr$executeScript(script = "window.scrollBy(0, 600);")
  tmp_el = remDr$findElement('css', '#pagef > div.paging_nav > span > strong')
  tmp_last_pg = as.numeric(tmp_el$getElementText())
  
  reset_page_gu(gu)
  
  
  ## 해당 세부지역(구)의 모든 도서관 대출 데이터 수집
  pg_cnt <- 1
  while (T) {
    tmp_list <- remDr$findElements('css', '#pagef > div.tbl_scroll_box.mgb_20 > table > tbody a')
    rep_num <- length(tmp_list)
    
    
    for (j in 1:rep_num) {
      
      # # 테스트용 코드 (접근 수 줄이기)
      #   if ((pg_cnt==1 & j<19) | (pg_cnt==2 & j<6)) {
      #     next
      #   }
      
      
      if (j>5) {
        for (rep_i in 1:j%/%5) {
          remDr$executeScript(script = "window.scrollBy(0, 200);")
        }
        Sys.sleep(1)
      }
      
      error_obj <- tryCatch(
        {
          tmp_selector <- paste0('#pagef > div.tbl_scroll_box.mgb_20 > table > tbody > tr:nth-child(', j, ') > td.link_td > a')
          tmp_btn <- remDr$findElement('css', tmp_selector)
          tmp_btn$clickElement()
          Sys.sleep(1)
          
          download_recent_file()
          print(paste(gu, pg_cnt, '번째 페이지의', j, '번째 도서관 방문 완료'))
        },
        error = function(e) {e}
      )
      if(inherits(error_obj, "error")) {
        print(error_obj)
        break
      }
      
      reset_page_gu(gu)
      if (pg_cnt>1) {
        for (i in 1:pg_cnt-1) {
          remDr$executeScript(script = "window.scrollTo(0, 500);")
          tmp_btn <- remDr$findElement('css', '#pagef > div.paging_nav > a.page_select.next_page')
          tmp_btn$clickElement()
          Sys.sleep(1)
        }
      }
    }
    
    pg_cnt <- pg_cnt + 1
    if (pg_cnt > tmp_last_pg) { break }
    
    reset_page_gu(gu)
    for (i in 1:pg_cnt-1) {
      remDr$executeScript(script = "window.scrollTo(0, 500);")
      tmp_btn <- remDr$findElement('css', '#pagef > div.paging_nav > a.page_select.next_page')
      tmp_btn$clickElement()
      Sys.sleep(1)
    }
  }
}
