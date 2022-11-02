   # HEX 20 60 12 05 10 05 0E 14 3A 20 1E 01 0E 60 05 #;[]*$96,REPENT:[]*$1E[]AN[]E
   # HEX 13 03 01 10 05 60 06 12 0F 0D 60 08 05 0C 0C #;SCAPE[]FROM[]HELL
   # HEX 20 BA 10 12 05 13 13 60 1A 20 A7 00 #;[]*$BA,PRESS[]Z[]*$A7
import codecs

oldlist = [0x20, 0x02, 0x12, 0x05, 0x10, 0x05, 0x0E, 0x14, 0x3A, 0x20, 0x1E, 0x01, 0x0E, 0x02, 0x05, 0x13, 
           0x03, 0x01, 0x10, 0x05, 0x02, 0x06, 0x12, 0x0F, 0x0D, 0x02, 0x08, 0x05, 0x0C, 0x0C, 0x20, 0xBA, 
           0x10, 0x12, 0x05, 0x13, 0x13, 0x02, 0x1A, 0x20, 0xA7, 0x00]

list =   [0x20, 0x60, 0x12, 0x05, 0x10, 0x05, 0x0E, 0x14, 0x3A, 0x20, 0x1E, 0x01, 0x0E, 0x02, 0x05, 0x13, 
          0x03, 0x01, 0x10, 0x05, 0x02, 0x06, 0x12, 0x0F, 0x0D, 0x02, 0x08, 0x05, 0x0C, 0x0C, 0x20, 0xBA, 
          0x10, 0x12, 0x05, 0x13, 0x13, 0x02, 0x1A, 0x20, 0xA7, 0x00]

print("\n---startinglist-------")

i = 0
for x in list:  
    print(f'{x:02x}', end = ' ')
    if (i % 16) == 15:
        print("")
    i = i + 1

print("\n------------------")

# i = 0
# for x in list:  
#     if x > 63:
#         print(" X IS MORE THAN 64, 0x", end = '')
#         print(f'{x:02x}', end = ' ')

#         print(i, end = '')
#         print("")

    # print(f'{x:02x}', end = ' ')
    # if (i % 16) == 15:
        # print("")
    # i = i + 1

newlist = []
newchar = 0


for x in list:
    if x == 0x20:
        if newchar != 0:
            newlist.append(newchar)
        newlist.append(0x20)
        continue
    if newlist[-1] == 0x20:
        newlist.append(x)
        continue      

    # termination
    if x == 0x00:
        if newchar == 0:
            newlist.append(0x00)
        else:
            newlist.append(newchar)
            newlist.append(0x00)
        break
            

    #else, assemble the hex of the next two numbers

    if newchar == 0: # if we are at first half of byte
        newchar = x
    else:
        newchar = newchar + (x + 0x80)
        newlist.append(newchar)
        newchar = 0
    # loop end


i = 0
for x in newlist:  
    print(f'{x:02x}', end = ' ')
    if (i % 16) == 15:
        print("")
    i = i + 1

print("\n------------------")

newlist = []
newchar = 0
subcounter = 0
i = 0

while i < len(list):
    x = list[i]
    if x == 0x20:
        if subcounter != 0:
            newlist.append(newchar)
            newchar = 0 
            subcounter = 0
        newlist.append(0x20)
        i = i + 1
        continue
    if newlist[-1] == 0x20:
        newlist.append(x)
        i = i + 1
        continue   
    # termination
    if x == 0x00:
        if subcounter == 0:
            newlist.append(0x00)
        else:
            newlist.append(newchar)
            newlist.append(0x00)
        break
    #else, assemble 


    newchar = newchar + (x + (0x20 * subcounter))
    if subcounter == 3:
        newlist.append(newchar)
        newchar = 0
    subcounter = (subcounter + 1) % 4
    i = i + 1
    # print(i, end = ' ')
    # print(f'{x:02x}', end = ' ')
    # print(f'{newchar:02x}')
    #end loop




print("\n------------------")

i = 0
for x in newlist:  
    print(f'{x:02x}', end = ' ')
    if (i % 16) == 15:
        print("")
    i = i + 1

print("\n------------------")