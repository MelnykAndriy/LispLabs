

from math import sin,cos,sqrt,exp 

def func1(n):
    if n == 1: 
        return 2
    else:
        prev = func1(n - 1)
        sin_res = sin(prev)
        cos_res = cos(prev)
        return sin_res*sin_res - cos_res * cos_res
        
    
def func2(n):
    if n == 9:
        return 1
    else:
        return sqrt(func2(n-1)) + exp(n)/100

def func(n):
    if 0 < n < 9:
        return func1(n)
    elif 8 < n < 15:
        return func2(n)
    else: 
        raise Exception()
        
def test():
    for x in [x for x in range(15)][1:]:
        print( "for i = %f : result is %f" % (x,func(x)))


    