#!/usr/bin/env python

import re
import os
import time
from collections import defaultdict

ipre = re.compile('[0-9]+\.[0-9]+\.[0-9]+\.[0-9]')

log_files = os.listdir('logs/')

ip_hits = defaultdict(int)
date_hits = defaultdict(int)
res_hits = defaultdict(int)

for lf in log_files:
    with file(os.path.join('logs',lf)) as l:
        first_in_file = True
        for line in l.readlines():
            if 'HEAD' in line:
                continue

            p = line.split(' - ')

            p0 = p[0].split(' ')
            p1 = p[1].split(' ')

            t = time.strptime((p0[2] + p0[3])[1:-1], '%d/%b/%Y:%H:%M:%S+0000')
            maybe_ip = p0[-1]
            if len(p1) >= 3 and not p1[0] in ['-', '"GET', '"PUT']:
                res_hits[p1[2]] += 1

            if ipre.match(maybe_ip):
                if first_in_file:
                    ip_hits[maybe_ip] += 1
                    date_hits[time.strftime("%Y-%m-%d", t)] += 1
                    
                    first_in_file = False

print sorted([(ip_hits[k],k) for k in ip_hits], reverse = True)
print sorted([(k, date_hits[k]) for k in date_hits])
print sorted([(res_hits[k],k) for k in res_hits], reverse = True)
