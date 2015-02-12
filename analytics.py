#!/usr/bin/env python

import re
import os
import time
from collections import defaultdict

ipre = re.compile('[0-9]+\.[0-9]+\.[0-9]+\.[0-9]')

log_files = os.listdir('logs/')

ip_hits = defaultdict(int)
date_hits = defaultdict(int)

for lf in log_files:
    with file(os.path.join('logs',lf)) as l:
        first_in_file = True
        for line in l.readlines():
            p = line.split(' - ')
            ps = p[0].split(' ')

            t = time.strptime((ps[2] + ps[3])[1:-1], '%d/%b/%Y:%H:%M:%S+0000')
            maybe_ip = ps[-1]

            if ipre.match(maybe_ip):
                if first_in_file:
                    ip_hits[maybe_ip] += 1
                    date_hits[time.strftime("%Y-%m-%d", t)] += 1
                    
                    first_in_file = False

print sorted([(ip_hits[k],k) for k in ip_hits], reverse = True)
print sorted([(k, date_hits[k]) for k in date_hits])
