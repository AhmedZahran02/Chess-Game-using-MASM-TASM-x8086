imageWidth = 526;
imageHeight = 935;
numColor = 3;

import numpy
import cv2
import sys
fd = open(sys.argv[1], 'rb')
f = numpy.fromfile(fd, dtype=numpy.uint8,count=imageHeight*imageWidth*numColor)
img = f.reshape((imageHeight, imageWidth, numColor))
ima1 = cv2.cvtColor(img,cv2.COLOR_BGR2RGB)
fd.close()
cv2.imshow('', ima1)
cv2.waitKey()
cv2.destroyAllWindows()