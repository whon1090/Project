import os
import cv2

os.chdir("D:\code\opencv")

img_file = "./img/girl.jpg" # 표시할 이미지 경로            ---①
img = cv2.imread(img_file)  # 이미지를 읽어서 img 변수에 할당 ---②

if img is not None:
  cv2.imshow('IMG', img)   # 읽은 이미지를 화면에 표시      --- ③
  cv2.waitKey()           # 키가 입력될 때 까지 대기      --- ④
  cv2.destroyAllWindows()  # 창 모두 닫기            --- ⑤
else:
    print('No image file.')

# image grayscale

img = cv2.imread(img_file, cv2.IMREAD_GRAYSCALE) # 그레이 스케일로 읽기

# image load

if img is not None:
    cv2.imshow('IMG',img)
    cv2.waitKey()
    cv2.destroyAllWindows()
else:
    print('No image file.')


# image save

save_file = './img/girl_gray.jpg'

img = cv2.imread(img_file,cv2.IMREAD_GRAYSCALE)
cv2.imshow(img_file, img) #파일로 저장, 포맷은 확장에 따름
cv2.imwrite(save_file, img)
cv2.waitKey()
cv2.destroyAllWindows()

# video read

video_file = "./img/big_buck.avi" # 동영상 파일 경로

cap = cv2.VideoCapture(video_file) # 동영상 캡쳐 객체 생성  ---①
if cap.isOpened():                 # 캡쳐 객체 초기화 확인
    while True:
        ret, img = cap.read()      # 다음 프레임 읽기      --- ②
        if ret:                     # 프레임 읽기 정상
            cv2.imshow(video_file, img) # 화면에 표시  --- ③
            cv2.waitKey(25)            # 25ms 지연(40fps로 가정)   --- ④
        else:                       # 다음 프레임 읽을 수 없슴,
            break                   # 재생 완료
else:
    print("can't open video.")      # 캡쳐 객체 초기화 실패
cap.release()                       # 캡쳐 자원 반납
cv2.destroyAllWindows()


# read frame from camera

cap = cv2.VideoCapture(0)               # 0번 카메라 장치 연결 ---①
if cap.isOpened():                      # 캡쳐 객체 연결 확인
    while True:
        ret, img = cap.read()           # 다음 프레임 읽기
        if ret:
            cv2.imshow('camera', img)   # 다음 프레임 이미지 표시
            if cv2.waitKey(1) != -1:    # 1ms 동안 키 입력 대기 ---②
                break                   # 아무 키라도 입력이 있으면 중지
        else:
            print('no frame')
            break
else:
    print("can't open camera.")
cap.release()                           # 자원 반납
cv2.destroyAllWindows()

# video play & set FPS

cap = cv2.VideoCapture(video_file) # 동영상 캡쳐 객체 생성
if cap.isOpened():                 # 캡쳐 객체 초기화 확인
    fps = cap.get(cv2.CAP_PROP_FPS) # 프레임 수 구하기
    delay = int(1000/fps)
    print("FPS: %f, Delay: %dms" %(fps, delay))

    while True:
        ret, img = cap.read()      # 다음 프레임 읽기
        if ret:                     # 프레임 읽기 정상
            cv2.imshow(video_file, img) # 화면에 표시
            cv2.waitKey(delay)            # fps에 맞게 시간 지연
        else:
            break                   # 다음 프레임 읽을 수 없슴, 재생 완료
else:
    print("can't open video.")      # 캡쳐 객체 초기화 실패
cap.release()                       # 캡쳐 자원 반납
cv2.destroyAllWindows()

# video cam resize

cap = cv2.VideoCapture(0)                   # 카메라 0번 장치 연결
width = cap.get(cv2.CAP_PROP_FRAME_WIDTH)   # 프레임 폭 값 구하기
height = cap.get(cv2.CAP_PROP_FRAME_HEIGHT) # 프레임 높이 값 구하기
print("Original width: %d, height:%d" % (width, height) ) 

cap.set(cv2.CAP_PROP_FRAME_WIDTH, 320)      # 프레임 폭을 320으로 설정 
cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 240)     # 프레임 높이를 240으로 설정
width = cap.get(cv2.CAP_PROP_FRAME_WIDTH)   # 재지정한 프레임 폭 값 구하기
height = cap.get(cv2.CAP_PROP_FRAME_HEIGHT) # 재지정한 프레임 폭 값 구하기


print("Resized width: %d, height:%d" % (width,height))
if cap.isOpened():
    while True:
        ret, img = cap.read()
        if ret:
            cv2.imshow('camera',img)
            if cv2.waitKey(1) != -1:
                break
        else :
            print('no frame!')
            break
else:
    print('cant open camera!')
cap.release()
cv2.destroyAllWindows()

# video cam take pic

cap = cv2.VideoCapture(0)                       # 0번 카메라 연결
if cap.isOpened() :
    while True:
        ret, frame = cap.read()                 # 카메라 프레임 읽기
        if ret:
            cv2.imshow('camera',frame)          # 프레임 화면에 표시
            if cv2.waitKey(1) != -1:            # 아무 키나 누르면
                cv2.imwrite('photo.jpg', frame) # 프레임을 'photo.jpg'에 저장
                break
        else:
            print('no frame!')
            break
else:
    print('no camera!')

cap.release()
cv2.destroyAllWindows()

# video cam rec

cap = cv2.VideoCapture(0)
if cap.isopened() :
    file_path = './record.avi'
    fps = 25.40
    fourcc = cv2.VideoWriter_fourcc(*'DIVX')
    width = cap.get(cv2.CAP_PROP_FRAME_WIDTH)
    height = cap.get(cv2.CAP_PROP_FRAME_HEIGHT)
    size = (int(width), int(height))
    out = cv2.VideoWriter(file_path, fourcc, fps, size) 
    while True:
        ret, frame = cap.read()
        if ret:
            cv2.imshow('camera-recording', frame)
            out.write(frame)
            if cv2.waitKey(int(1000/fps)) != -1 :
                break
        else :
            print("no frame!")
            break
        out.release()
else :
    print('cant open camera')

cap.release()
cv2.destroyAllWindows()

# draw img
import numpy as np

img = cv2.imread('./img/blank_500.jpg')

cv2.line(img, (50, 50), (150, 50), (255,0,0))   # 파란색 1픽셀 선
cv2.line(img, (200, 50), (300, 50), (0,255,0))  # 초록색 1픽셀 선
cv2.line(img, (350, 50), (450, 50), (0,0,255))  # 빨간색 1픽셀 선

# 하늘색(파랑+초록) 10픽셀 선      
cv2.line(img, (100, 100), (400, 100), (255,255,0), 10)          
# 분홍(파랑+빨강) 10픽셀 선      
cv2.line(img, (100, 150), (400, 150), (255,0,255), 10)          
# 노랑(초록+빨강) 10픽셀 선      
cv2.line(img, (100, 200), (400, 200), (0,255,255), 10)          
# 회색(파랑+초록+빨강) 10픽셀 선  
cv2.line(img, (100, 250), (400, 250), (200,200,200), 10)        
# 검정 10픽셀 선    
cv2.line(img, (100, 300), (400, 300), (0,0,0), 10)                    

# 4연결 선
cv2.line(img, (100, 350), (400, 400), (0,0,255), 20, cv2.LINE_4)   
# 8연결 선
cv2.line(img, (100, 400), (400, 450), (0,0,255), 20, cv2.LINE_8)    
# 안티에일리어싱 선 
cv2.line(img, (100, 450), (400, 500), (0,0,255), 20, cv2.LINE_AA)   
# 이미지 전체에 대각선 
cv2.line(img, (0,0), (500,500), (0,0,255))  

cv2.imshow('lines',img)
cv2.waitKey(0)
cv2.destroyAllWindows()

# draw rect

img = cv2.imread('./img/blank_500.jpg')

# 좌상, 우하 좌표로 사각형 그리기
cv2.rectangle(img, (50, 50), (150, 150), (255,0,0) )        
# 우하, 좌상 좌표로 사각형 그리기
cv2.rectangle(img, (300, 300), (100, 100), (0,255,0), 10 )  
# 우상, 좌하 좌표로 사각형 채워 그리기 ---①
cv2.rectangle(img, (450, 200), (200, 450), (0,0,255), -1 )  

cv2.imshow('rectangel',img)
cv2.waitKey(0)
cv2.destroyAllWindows()

# draw polylines

img = cv2.imread('./img/blank_500.jpg')

# Numpy array로 좌표 생성 ---②
# 번개 모양 선 좌표
pts1 = np.array([[50,50], [150,150], [100,140],[200,240]], dtype=np.int32) 
# 삼각형 좌표
pts2 = np.array([[350,50], [250,200], [450,200]], dtype=np.int32) 
# 삼각형 좌표
pts3 = np.array([[150,300], [50,450], [250,450]], dtype=np.int32) 
# 5각형 좌표
pts4 = np.array([[350,250], [450,350], [400,450], [300,450], [250,350]],\
                 dtype=np.int32) 

# 다각형 그리기 ---③
cv2.polylines(img, [pts1], False, (255,0,0))       # 번개 모양 선 그리기
cv2.polylines(img, [pts2], False, (0,0,0), 10)     # 3각형 열린 선 그리기 ---④
cv2.polylines(img, [pts3], True, (0,0,255), 10)    # 3각형 닫힌 도형 그리기 ---⑤
cv2.polylines(img, [pts4], True, (0,0,0))          # 5각형 닫힌 도형 그리기

cv2.imshow('polyline',img)
cv2.waitKey(0)
cv2.destroyAllWindows()

# draw circle

img = cv2.imread('./img/blank_500.jpg')

# 원점(150,150), 반지름 100 ---①
cv2.circle(img, (150, 150), 100, (255,0,0))     
# 원점(300,150), 반지름 70 ---②
cv2.circle(img, (300, 150), 70, (0,255,0), 5)   
# 원점(400,150), 반지름 50, 채우기 ---③
cv2.circle(img, (400, 150), 50, (0,0,255), -1)  

# 원점(50,300), 반지름(50), 회전 0, 0도 부터 360도 그리기 ---④
cv2.ellipse(img, (50, 300), (50, 50), 0, 0, 360, (0,0,255))    
# 원점(150, 300), 아래 반원 그리기 ---⑤
cv2.ellipse(img, (150, 300), (50, 50), 0, 0, 180, (255,0,0))    
#원점(200, 300), 윗 반원 그리기 ---⑥
cv2.ellipse(img, (200, 300), (50, 50), 0, 181, 360, (0,0,255))    

# 원점(325, 300), 반지름(75,50) 납작한 타원 그리기 ---⑦
cv2.ellipse(img, (325, 300), (75, 50), 0, 0, 360, (0,255,0))    
# 원점(450,300), 반지름(50,75) 홀쭉한 타원 그리기 ---⑧
cv2.ellipse(img, (450, 300), (50, 75), 0, 0, 360, (255,0,255))    

# 원점(50, 425), 반지름(50,75), 회전 15도 ---⑨
cv2.ellipse(img, (50, 425), (50, 75), 15, 0, 360, (0,0,0))    
# 원점(200,425), 반지름(50,75), 회전 45도 ---⑩
cv2.ellipse(img, (200, 425), (50, 75), 45, 0, 360, (0,0,0))    

# 원점(350,425), 홀쭉한 타원 45도 회전 후 아랫 반원 그리기 ---⑪
cv2.ellipse(img, (350, 425), (50, 75), 45, 0, 180, (0,0,255))    
# 원점(400,425), 홀쭉한 타원 45도 회전 후 윗 반원 그리기 ---⑫
cv2.ellipse(img, (400, 425), (50, 75), 45, 181, 360, (255,0,0))    

cv2.imshow('circle',img)
cv2.waitKey(0)
cv2.destroyAllWindows()

# draw text

img = cv2.imread('../img/blank_500.jpg')

# sans-serif small
cv2.putText(img, "Plain", (50, 30), cv2.FONT_HERSHEY_PLAIN, 1, (0, 0,0))            
# sans-serif normal
cv2.putText(img, "Simplex", (50, 70), cv2.FONT_HERSHEY_SIMPLEX, 1, (0, 0,0))        
# sans-serif bold
cv2.putText(img, "Duplex", (50, 110), cv2.FONT_HERSHEY_DUPLEX, 1, (0, 0,0))         
# sans-serif normall X2  ---①
cv2.putText(img, "Simplex", (200, 110), cv2.FONT_HERSHEY_SIMPLEX, 2, (0,0,250)) 

# serif small
cv2.putText(img, "Complex Small", (50, 180), cv2.FONT_HERSHEY_COMPLEX_SMALL, \
            1, (0, 0,0))   
# serif normal
cv2.putText(img, "Complex", (50, 220), cv2.FONT_HERSHEY_COMPLEX, 1, (0, 0,0))
# serif bold
cv2.putText(img, "Triplex", (50, 260), cv2.FONT_HERSHEY_TRIPLEX, 1, (0, 0,0))               
# serif normal X2  ---②
cv2.putText(img, "Complex", (200, 260), cv2.FONT_HERSHEY_TRIPLEX, 2, (0,0,255))               

# hand-wringing sans-serif
cv2.putText(img, "Script Simplex", (50, 330), cv2.FONT_HERSHEY_SCRIPT_SIMPLEX, \
            1, (0, 0,0)) 
# hand-wringing serif
cv2.putText(img, "Script Complex", (50, 370), cv2.FONT_HERSHEY_SCRIPT_COMPLEX, \
            1, (0, 0,0)) 

# sans-serif + italic ---③
cv2.putText(img, "Plain Italic", (50, 430), \
            cv2.FONT_HERSHEY_PLAIN | cv2.FONT_ITALIC, 1, (0, 0,0)) 
# sarif + italic
cv2.putText(img, "Complex Italic", (50, 470), \
            cv2.FONT_HERSHEY_COMPLEX | cv2.FONT_ITALIC, 1, (0, 0,0)) 

cv2.imshow('draw text', img)
cv2.waitKey()
cv2.destroyAllWindows()

# win

file_path = '../img/girl.jpg'
img = cv2.imread(file_path)                            # 이미지를 기본 값으로 읽기
img_gray = cv2.imread(file_path, cv2.IMREAD_GRAYSCALE) # 이미지를 그레이 스케일로 읽기

cv2.namedWindow('origin')                               # origin 이름으로 창 생성
cv2.namedWindow('gray', cv2.WINDOW_NORMAL)              # gray 이름으로 창 생성
cv2.imshow('origin', img)                               # origin 창에 이미지 표시
cv2.imshow('gray', img_gray)                            # gray 창에 이미지 표시

cv2.moveWindow('origin', 0, 0)                          # 창 위치 변경
cv2.moveWindow('gray', 100, 100)                        # 창 위치 변경

cv2.waitKey(0)                                          # 아무키나 누르면
cv2.resizeWindow('origin', 200, 200)                    # 창 크기 변경 (변경 안됨)
cv2.resizeWindow('gray', 100, 100)                      # 창 크기 변경 (변경 됨))

cv2.waitKey(0)                                          # 아무키나 누르면
cv2.destroyWindow("gray")                               # gray 창 닫기

cv2.waitKey(0)                                          # 아무키나 누르면
cv2.destroyAllWindows()                                 # 모든 창 닫기

img_file = "../img/girl.jpg" 
img = cv2.imread(img_file) 
title = 'IMG'                   # 창 이름 
x, y = 100, 100                 # 최초 좌표

while True:
    cv2.imshow(title, img)
    cv2.moveWindow(title, x, y)
    key = cv2.waitKey(0) & 0xFF # 키보드 입력을 무한 대기, 8비트 마스크처리
    print(key, chr(key))        # 키보드 입력 값,  문자 값 출력
    if key == ord('h'):         # 'h' 키 이면 좌로 이동
        x -= 10
    elif key == ord('j'):       # 'j' 키 이면 아래로 이동
        y += 10
    elif key == ord('k'):       # 'k' 키 이면 위로 이동
        y -= 10
    elif key == ord('l'):       # 'l' 키 이면 오른쪽으로 이동
        x += 10
    elif key == ord('q') or key == 27: # 'q' 이거나 'esc' 이면 종료
        break
        cv2.destroyAllWindows()
    cv2.moveWindow(title, x, y )   # 새로운 좌표로 창 이동
        
# mouse event
title = 'mouse event'                   # 창 제목
img = cv2.imread('../img/blank_500.jpg') # 백색 이미지 읽기
cv2.imshow(title, img)                  # 백색 이미지 표시

def onMouse(event, x, y, flags, param): # 아무스 콜백 함수 구현 ---①
    print(event, x, y, )                # 파라미터 출력
    if event == cv2.EVENT_LBUTTONDOWN:  # 왼쪽 버튼 누름인 경우 ---②
        cv2.circle(img, (x,y), 30, (0,0,0), -1) # 지름 30 크기의 검은색 원을 해당 좌표에 그림
        cv2.imshow(title, img)          # 그려진 이미지를 다시 표시 ---③

cv2.setMouseCallback(title, onMouse)    # 마우스 콜백 함수를 GUI 윈도우에 등록 ---④

while True:
    if cv2.waitKey(0) & 0xFF == 27:     # esc로 종료
        break
cv2.destroyAllWindows()

# mouse event flags

title = 'mouse event'                   # 창 제목
img = cv2.imread('../img/blank_500.jpg') # 백색 이미지 읽기
cv2.imshow(title, img)                  # 백색 이미지 표시

colors = {'black':(0,0,0),
         'red' : (0,0,255),
         'blue':(255,0,0),
         'green': (0,255,0) } # 색상 미리 정의

def onMouse(event, x, y, flags, param): # 아무스 콜백 함수 구현 ---①
    print(event, x, y, flags)                # 파라미터 출력
    color = colors['black']
    if event == cv2.EVENT_LBUTTONDOWN:  # 왼쪽 버튼 누름인 경우 ---②
        # 컨트롤키와 쉬프트 키를 모두 누른 경우
        if flags & cv2.EVENT_FLAG_CTRLKEY and flags & cv2.EVENT_FLAG_SHIFTKEY : 
            color = colors['green']
        elif flags & cv2.EVENT_FLAG_SHIFTKEY : # 쉬프트 키를 누른 경우
            color = colors['blue']
        elif flags & cv2.EVENT_FLAG_CTRLKEY : # 컨트롤 키를 누른 경우
            color = colors['red']
        # 지름 30 크기의 검은색 원을 해당 좌표에 그림
        cv2.circle(img, (x,y), 30, color, -1) 
        cv2.imshow(title, img)          # 그려진 이미지를 다시 표시 ---③

cv2.setMouseCallback(title, onMouse)    # 마우스 콜백 함수를 GUI 윈도우에 등록 ---④

while True:
    if cv2.waitKey(0) & 0xFF == 27:     # esc로 종료
        break
cv2.destroyAllWindows()


# trackbar
win_name = 'Trackbar'                                   # 창 이름

img = cv2.imread('../img/blank_500.jpg')
cv2.imshow(win_name,img)                                # 초기 이미지를 창에 표시

# 트랙바 이벤트 처리 함수 선언 ---①
def onChange(x):                                        
    print(x)                                            # 트랙바 새로운 위치 값 --- ②
    # 'R', 'G', 'B' 각 트랙바 위치 값    --- ③
    r = cv2.getTrackbarPos('R',win_name)               
    g = cv2.getTrackbarPos('G',win_name)               
    b = cv2.getTrackbarPos('B',win_name)               
    print(r, g, b)
    img[:] = [b,g,r]                                    # 기존 이미지에 새로운 픽셀 값 적용 --- ④
    cv2.imshow(win_name, img)                           # 새 이미지 창에 표시

# 트랙바 생성    --- ⑤
cv2.createTrackbar('R', win_name, 255, 255, onChange)  
cv2.createTrackbar('G', win_name, 255, 255, onChange)
cv2.createTrackbar('B', win_name, 255, 255, onChange)

while True:
    if cv2.waitKey(1) & 0xFF == 27:
        break
cv2.destroyAllWindows()        