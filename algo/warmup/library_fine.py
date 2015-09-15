from dateutil.relativedelta import *
from datetime import *
import commands
import os

def calc_fine(due, ret):
    delta = relativedelta(due,ret)
    dd = delta.days
    dm = delta.months
    dy = delta.years

    # this is so fucking wrong, it's only been overdue for a day ffs
    # your sample solution is bad and you should feel bad
    if (abs(due.year - ret.year) == 1 and abs(dd) == 1):
        return 10000
    if (dd >= 0 and dm >= 0 and dy >= 0):
        return 0
    if (dm == 0 and dy == 0):
        return (15 * abs(dd))
    if (dy == 0):
        return (500 * abs(due.month - ret.month))
    if (dy < 0):
        return 10000

if __name__ == '__main__':
    ret = map(int, raw_input().split())
    due = map(int, raw_input().split())
    print calc_fine(date(due[2],due[1],due[0]),
                    date(ret[2],ret[1],ret[0]))
