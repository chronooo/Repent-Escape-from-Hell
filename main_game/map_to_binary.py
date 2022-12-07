import sys

filepath = sys.argv[1]

# file = open(filepath, 'r')
# lines = file.readlines()

count = 0


# define function for numerical breakpoints
def increment_count():
    global count
    count += 1
    # if count % 256 == 0:
    if count % 64 == 0:
        print(";   Bytes: " + str(count))
    if count % 16 == 0:
        print("")
        print("HEX ", end='')


print("HEX ", end='')
y = 0
x = 0
with open(filepath) as file:
    for line in file:
        # print(line.rstrip())

        print("00 ", end='')
        increment_count()
        y = 0
        for char in line.rstrip()[::-1]:

            if char == 'P':
                print(f"{1 + (32 * y):02x}", end=' ')
                increment_count()
            elif char == '-':
                pass
                # skip over this thing
                # print(2, end='')
            elif char == 'L':
                print(f"{6 + (32 * y):02x}", end=' ')  # ladder char is 6
                increment_count()

            elif char == 'E':
                print(f"{3 + (32 * y):02x}", end=' ')  # ladder char is 3
                increment_count()
            elif char == "S":
                print(f"{17 + (32 * y):02x}", end=' ')  # SCROLL
                increment_count()
            elif char == "F":
                print(f"{18 + (32 * y):02x}", end=' ')  # FLOOR FILL
                increment_count()
            elif char == "T":
                print(f"{19 + (32 * y):02x}", end=' ')  # TELEPORTER
                increment_count()
            else:
                # print("SUS SUS SUS SUS SUS CHARACTER IN MAP SUS !!")
                quit()

            if y < 0:
                print("BAD MAP BAD MAP TOO COLUMN TOO TALL!!")
                quit()
            y = y + 1

print("FF")
print("                                                    ;   Total map size: " + str(count) + " Bytes", end="")
