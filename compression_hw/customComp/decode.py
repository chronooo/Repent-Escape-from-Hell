list = [0x20, 0x60, 0xec, 0xbc, 0x20, 0x1e, 0xd6, 0xe7, 
        0xdf, 0xe6, 0x7d, 0x20, 0xba, 0xfa, 0x8f, 0x20, 
        0xa7, 0x00]

newlist = []

print("\n----input:-------")

i = 0
for x in list:  
    print(f'{x:02x}', end = ' ')
#     if (i % 16) == 15:
#         print("")
    i = i + 1

print("\n------------------")


print("\n----output:-------")

i = 0
twenty_last = False

for x in newlist: 
    if x == 0:
        print(f'{x:02x}', end = ' ')
        break
    if twenty_last == True:
        print(f'{x:02x}', end = ' ')
        twenty_last = False
        continue
    if x == 0x20:
        print(f'{x:02x}', end = ' ')
        twenty_last = True
        continue

    for i in range(4):
        if (x - ) != 0:
        first = 
        print(f'{x:02x}', end = ' ')


    
    print(f'{x:02x}', end = ' ')
    if (i % 16) == 15:
        print("")
    i = i + 1

print("\n------------------")