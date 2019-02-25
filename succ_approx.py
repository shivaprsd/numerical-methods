from math import sqrt

def f(x):
    return x**2 + x - 0.5

xi = input()
while True:
    xinext = f(xi)
    print(xinext)
    if(abs(xinext - xi) < 0.0005):
        break
    xi = xinext

print(xinext)
