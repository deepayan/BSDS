
def aliquot_sum(n):
  sumdiv = 1
  for i in range(2, 1 + n // 2):
    if n % i == 0:
      sumdiv += i
  return sumdiv

n = 1
while True:
  if aliquot_sum(n) == n: 
    print(n)
  n += 1

