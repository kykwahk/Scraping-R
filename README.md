# R을 이용한 웹스크레이핑과 데이터분석
『R을 이용한 웹스크레이핑과 데이터분석』(곽기영, 도서출판 청람), https://product.kyobobook.co.kr/detail/S000000907353.   

『곽기영』 채널(유튜브 동영상 강의), https://www.youtube.com/곽기영.   

Chapter 01 웹스크레이핑 개요   

1.1 웹 데이터 수집   
1.2 웹스크레이핑 기술 및 기법   

Chapter 02 파일 다운로드   

2.1 파일 읽기   
CSV   
테이블   
비정형 텍스트   
스프레드시트   

2.2 사례   
아이리스 @UCI   
원자력 발전소 @NRC   
인공강우 @StatLib   
이상한 나라의 앨리스 @프로젝트 구텐베르크   

Chapter 03 텍스트 패턴매칭   

3.1 정규표현식   
문자열 매칭   
문자 클래스   
수량자   
그리디 매칭 vs. 레이지 매칭   
이스케이프 시퀀스   
문자 클래스 시퀀스   
백레퍼런스   
예: 전화번호부   

3.2 텍스트 함수   
base 패키지   
stringr 패키지   

3.3 인코딩   

3.4 사례   
게티스버그 연설 @에이브러햄 링컨 온라인   
국가코드 @UN   
직업별 연봉 @CNBC   

Chapter 04 XPath   

4.1 HTML 구조   
헤딩   
패러그래프   
포맷   
CSS   
링크   
테이블   
리스트   
그룹   
폼   

4.2 XML 구조   

4.3 HTML/XML 파싱과 노드셋 추출   

4.4 XPath와 노드셋/데이터 추출   
노드셋 추출   
절대경로와 상대경로   
XPath 액시스   
XPath 프레디킷   
데이터 추출   

4.5 사례   
노벨상 소개 @노벨재단   
911 테러 @뉴욕타임즈   
올해의 영화 100 @IMDb   
레스토랑 리뷰 @오픈테이블   
영화 리뷰 @네이버   
웹소설 @네이버   
국가별 가처분 소득 @NationMaster   
국회의원 경비 내역 @BBC   
동의어 사전 @워드넷   
가독성 테스트 @웹FX   
이미지 포맷 변환 @픽스픽처   
경제통계 @미국노동통계국   

Chapter 05 CSS 선택자   

5.1 노드 선택   

5.2 SelectorGadget   

5.3 사례   
인기 영화 250 @IMDb   
노벨상 수상 @노벨재단   
세계인구 @위키피디아   
올림픽 메달 @위키피디아   
출생아수 @미국사회보장국   
위험에 처한 세계유산 @위키피디아/UNESCO   
동의어 사전 @워드넷   
가독성 테스트 @웹FX   

Chapter 06 셀레니움   

6.1 셀레니움 개요   

6.2 셀레니움 환경 구축   
자바 바이너리   
rsDriver()   
도커   

6.3 RSelenium 패키지   
시작   
이동   
요소 식별   
요소 구동   
자바스크립트 실행   
프레임   

6.4 사례   
영화 리뷰 @IMDb   
동영상 정보 @유튜브   
글로벌 대학 순위 @US뉴스   
프로축구 통계 @프리미어리그   

Chapter 07 JSON   

7.1 JSON 구조   

7.2 JSON 파싱   
RJSONIO 패키지   
jsonlite 패키지   

7.3 사례   
국가/지역 코드 @UN   
색상 코드 @GitHub   
레미제라블 @GitHub   
연구 키워드 @NASA   

Chapter 08 API   

8.1 API 개요   
REST   
GraphQL   

8.2 OAuth 인증   

8.3 API 래퍼 소프트웨어   

8.4 사례   
특일 정보 @공공데이터포털/천문연구원   
대기오염 정보 @공공데이터포털/한국환경공단   
아파트 매매 실거래 정보 @공공데이터포털/국토교통부   
소액대출 @Kiva   
트윗 @트위터   
실업률 @세계은행   
eBook @프로젝트 구텐베르크   

Appendix Tidyverse   

A.1 팩터와 데이터프레임   
forcats 패키지   
tibble 패키지   

A.2 텍스트와 날짜   
stringr 패키지   
lubridate 패키지   

A.3 입력 및 출력: readr 패키지   

A.4 데이터 변환   
dplyr 패키지   
purrr 패키지   
tidyr 패키지   

A.5 그래픽: ggplot2 패키지   
그래프 생성   
geom 객체 옵션   
집단별 그래프   
그래프 옵션   
테마   
그래프 배치 및 저장   
