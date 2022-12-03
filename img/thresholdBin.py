import matplotlib.pyplot as plt
import cv2
import sys

image= cv2.imread(sys.argv[1])
gray_image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
ret, thresh = cv2.threshold(gray_image, 120, 255, cv2.THRESH_BINARY)
cv2.imshow("image", thresh)
cv2.waitKey(0) # waits until a key is pressed
cv2.destroyAllWindows() # destroys the window showing image

print(thresh)
thresh.tofile(sys.argv[2])