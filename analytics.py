#!/usr/bin/env python

import re
import os
from collections import defaultdict

ipre = re.compile('[0-9]+\.[0-9]+\.[0-9]+\.[0-9]')

log_files = os.listdir('logs/')

ip_hits = defaultdict(int)

for lf in log_files:
    with file(os.path.join('logs',lf)) as l:
        first_in_file = True
        for line in l.readlines():
            p = line.split(' - ')
            maybe_ip = p[0].split(' ')[-1]
            if ipre.match(maybe_ip):
                if first_in_file:
                    ip_hits[maybe_ip] += 1
                    first_in_file = False

print sorted([(ip_hits[k],k) for k in ip_hits], reverse = True)
