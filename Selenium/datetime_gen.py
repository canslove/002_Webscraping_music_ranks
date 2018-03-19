import datetime

max=datetime.date(2018,2,1)
min=datetime.date(2017,9,15)
print(min,max)

n= int((max-min+datetime.timedelta(days=1))/datetime.timedelta(days=7))
print(n)

start_date = min
end_date = start_date + datetime.timedelta(days=6)

datelist_sta = [str(min)]
datelist_end = [str(end_date)]

tmp = min
tmp2 = end_date

for i in range(n-1):
    #tmp = list_[-1] + datetime.timedelta(days=6)
    tmp += datetime.timedelta(days=7)
    datelist_sta.append(str(tmp))
    
    tmp2 += datetime.timedelta(days=7)
    datelist_end.append(str(tmp2))
print(datelist_sta, datelist_end)


a = list(map(lambda s: "".join(s.split('-')), datelist_sta))
b = list(map(lambda s: "".join(s.split('-')), datelist_end))

# %3A20180119%3A20180125%3Aus
c = list(map(lambda x, y, z, r, q: x+y+z+r+q, ['%3A']*20, a, ['%3A']*20, b, ['%3Aus']*20)) 
c


