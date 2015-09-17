from itertools import *

# observe that
# [f(x%m)]%m == [f(x)]%m
# [f(x%m)%m + f(y%m)%m +...]%m == [f(x%m)+f(y%m)+...]%m == [f(x)+f(y)+...]%m
# by experiment #2 is fastest

(k,m) = tuple(map(int, raw_input().split()))
lists = [set(map(lambda x: int(x)%m,raw_input().split()[1:])) for _ in range(k)]
print max([sum(map(lambda x:x*x,comb))%m for comb in product(*lists)])
