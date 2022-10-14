from PIL import Image
width = 8
height = 16
import sys
ascii_char = list("$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\|()1{}[]?-_+~<>i!lI;:,\"^`'. ")
def get_char(r,g,b,alpha = 256):
    if alpha == 0:
        return ' '
    length = len(ascii_char)
    gray = int(0.2126 * r + 0.7152 * g + 0.0722 * b)
    unit = (255.0 + 1)/length
    return ascii_char[int(gray/unit)]
if __name__ == '__main__':
    IMG = sys.argv[1]
    im = Image.open(IMG)
    im = im.resize((width,heigh), Image.NEAREST)
    txt = ""
    printable_text = ""
    for i in range(height):
        txt += "%"
        for j in range(width):
            temp_char = get_char(*im.getpixel((j,i)))
            if temp_char == " ":
                temp_char0 = " "
                temp_char1 = 0
            else:
                temp_char0 = "▆"
                temp_char1 = 1
            printable_text += str(temp_char0)
            txt += str(temp_char1)
        printable_text += '\n'
        txt += '\n'
    
    print(printable_text)
    print(txt)