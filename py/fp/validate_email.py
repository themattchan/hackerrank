import re

def isValid(email):
    e = email.replace('@','.').split('.')
    if len(e) != 3 or len(filter(lambda s: s, e)) != 3: return False
    u = re.search(r'[^\-\_\A-Za-z0-9]+',e[0]) == None
    w = re.search(r'[^\A-Za-z0-9]+',e[1]) == None
    x = len(e[2]) >= 1 and len(e[2]) <= 3 and re.search(r'[^\A-Za-z]+',e[2]) == None
    return u and w and x

emails = [raw_input() for i in range(input())]
print sorted(filter(isValid,emails))
