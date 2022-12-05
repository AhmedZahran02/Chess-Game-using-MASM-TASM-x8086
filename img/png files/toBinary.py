
from PIL import Image
from io import BytesIO
import sys

out = BytesIO()

with Image.open(sys.argv[1]) as img:
     img.save(out, format="png")

image_in_bytes = out.getvalue()

encoded_b2 = "".join([format(n, '08b') for n in image_in_bytes])
#print(encoded_b2)

decoded_b2 = [int(encoded_b2[i:i + 8], 2) for i in range(0, len(encoded_b2), 8)]

with open(sys.argv[2], 'wb') as f:
     f.write(bytes(decoded_b2))