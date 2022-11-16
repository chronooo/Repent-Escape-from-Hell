import sys

filepath = sys.argv[1]

# file = open(filepath, 'r')
# lines = file.readlines()
count = 0
print("HEX ", end='')
y = 0
x = 0
with open(filepath) as file:
    for line in file:
        # print(line.rstrip())
        print("00 ", end='')
        count += 1
        if count % 16 == 0:
            print("")
            print("HEX ", end='')
        y = 7
        for char in line.rstrip()[::-1]:
            if char == 'P':
                x = hex(1 + (32 * y))  # 01 is platform tile
                print(f"{1 + (32 * y):02x}", end=' ')
                count += 1
                if count % 16 == 0:
                    print("")
                    print("HEX ", end='')
            elif char == '-':
                pass
                # skip over this thing
                # print(2, end='')
            elif char == 'L':
                x = hex(6 + (32 * y))  # 01 is platform tile
                print(f"{6 + (32 * y):02x}", end=' ')  # ladder char is 6
                count += 1
                if count % 16 == 0:
                    print("")
                    print("HEX ", end='')
            elif char == 'E':
                x = hex(3 + (32 * y))  # 01 is platform tile
                print(f"{3 + (32 * y):02x}", end=' ')  # ladder char is 3
                count += 1
                if count % 16 == 0:
                    print("")
                    print("HEX ", end='')
            else:
                # print("SUS SUS SUS SUS SUS CHARACTER IN MAP SUS !!")
                quit()

            if y < 0:
                print("BAD MAP BAD MAP TOO COLUMN TOO TALL!!")
                quit()
            y = y - 1

            # print(" ")
            # print(count)
            # print(" ")
