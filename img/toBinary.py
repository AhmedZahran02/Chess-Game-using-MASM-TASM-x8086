
import cv2
import sys
img = cv2.imread(sys.argv[1])
ima1 = cv2.cvtColor(img,cv2.COLOR_BGR2RGB)
f=open(sys.argv[2],'wb')
f.write(ima1)
f.close()