import math

p = float(input ("enter the least count for ur sin to be calculated"))
x = float(input("enter the angle"))
d = 100 * p
sum = 0
i = 0
while abs(d)>p:
    d = x**(2*i + 1)/math.factorial(2*i +1)*(-1)**i
    sum += d
    i += 1
print("Sine of the angle is", sum, "+/-", p)
