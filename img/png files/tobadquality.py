import binascii
import sys
filename = sys.argv[1]
with open(filename, 'rb') as f:
    content = f.read()
print(binascii.hexlify(content))

f=open(sys.argv[2],'wb')
f.write(binascii.hexlify(content))