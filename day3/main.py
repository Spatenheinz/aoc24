#!/usr/bin/env python3

import re

with open("input.txt", "r") as f:
    text = f.read()

# Find all matches
pattern = r"mul\((\d{1,3}),(\d{1,3})\)"
matches = re.findall(pattern, text)
sum = 0
[sum := sum + int(x) * int(y) for x, y in matches]
print(sum)

pattern = r"mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don\'t\(\))"
matches = re.findall(pattern, text)

sum = 0
mul = 1
for x, y, do, dont in matches:
    if dont:
        mul = 0
    elif do:
        mul = 1
    elif x and y:
        sum += int(x) * int(y) * mul

print(sum)
