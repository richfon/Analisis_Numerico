# -*- coding: utf-8 -*-
"""
Created on Mon Oct 11 20:20:50 2021

@author: david
"""

import sympy as sp
def intLagrange(x,y,u=None):
  n=len(x)
  if u==None:
    t=sp.Symbol('x')

  else:
    t=u
  p=0
  for i in range(0,n):
    l=1
    for j in range (0,n):
      if j!=i:
        l=l*(t-x[j])/(x[i]-x[j])
    p=p+y[i]*l
    p=sp.expand(p)
  return p

x=[1,2.5,3.8,4,5.25,6]
y=[5,-2,1.5,6.1,-4.8,10]
p=intLagrange(x,y)
print ("POLINOMIO:")
print (p) #Polinomio de interpolacion
a=intLagrange(x,y,4) #Evaluar el polinomio en otro punto
print("EVALUACION DEL POLINOMIO:")
print(a)
