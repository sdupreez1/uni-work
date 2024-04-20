from math import sqrt
import urllib.request
import re


# Useful functions
def prime(n):
    return all([1 if (n % 2 and n % i) else 0 for i in range(3, sqrt(n), 2)])     # returns True or False if n is prime or not


def product(n):
    a = 1
    for i in n:
        a *= int(i)
    return a


# Problem 1: find the sum of all factors of n divisible by 3 or 5
def p1(n):
    return sum([k for k in range(n) if (k % 3 == 0 or k % 5 == 0)])


# Problem 2: find the sum of all the Fibonacci numbers less than n
def p2(n):
    fn1, fn, fo, sn = 0, 1, 0, 0
    while fn < n:
        a = fn + fn1
        fn1, fn, sn = fn, a, sn + a
        if a % 2:
            fo += fn
    print(sn - fo)


# Problem 3: find the largest prime factor of n
def p3(n):
    return max([k for k in range(1, round(sqrt(n))) if (n % k == 0 and prime(k))])


# Problem 4: find the largest palindrome made from the product of two 3-digit numbers
def p4():
    return max([k*j for k in range(100, 1000) for j in range(100, 1000) if
                (all(
                    [str(k*j)[i-1] == str(k*j)[-i] for i in range(len(str(k*j)))])
                )])


# Problem 5: Find the smallest positive number that is evenly divisible by all numbers from 1 to 20


# Problem 6: Find the difference between the sum of the squares of the first n natural
# numbers and the square of the sum.
def p6(n):
    return (sum([k for k in range(n+1)]))**2 - sum([k**2 for k in range(n+1)])

# Problem 7: find the nth prime number
def p7(n):
    primes = [2, 3]
    p = 3
    while len(primes) < n:
        p += 2
        if all([p%i for i in primes]):
            if prime(p):
                primes.append(p)
    return max(primes)


# Problem 8: Find the k adjacent digits in the 1000-digit number that have the greatest product.
# What is the value of this product?
def p8(k):
    p8link = 'https://projecteuler.net/problem=8'
    p8 = urllib.request.urlopen(p8link)
    p8t = str(p8.read())
    nlst = [re.sub(r'\D','', x) for x in re.split(r'\\n', p8t)
            if (bool(re.search(r'\d', x[0]) and re.search(r'\d', x[-11])))]
    n = ''.join(nlst)
    return max([product(n[i:i+k]) for i in range(len(n))])


# Problem 9: There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.
def p9():
    for p in [(a,b,c) for a in range(0, 900, 2) for b in range(1, 900, 2) for c in range(1, 900)]:
        if (p[0]**2 + p[1]**2) == p[2]**2:
            if p[0] + p[1] + p[2] == 1000:
                return p[0]*p[1]*p[2]


# Find the sum of all the primes below n
def p10(n):
    return sum([p for p in range(1, n, 2) if prime(p)]) + 2

print(p10(2000000))