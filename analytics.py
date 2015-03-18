#!/usr/bin/env python

from __future__ import print_function

import re
import os
import time
from collections import Counter, defaultdict

ipre = re.compile('(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})')

# Ignore Googlebot, Baidu, Yandex, Trend Micro
ignore_prefix = [tuple(p.split('.')) for p in ['66.249', '180.76', '100.43',
                                               '150.70', '202.46',
                                               '199.21.99']]

log_files = os.listdir('logs/')

ip_hits = Counter()
prefix_hits = defaultdict(set)
date_hits = Counter()
res_hits = Counter()

for lf in log_files:
    with open(os.path.join('logs',lf), 'r') as l:
        first_in_file = True
        for line in l.readlines():
            if 'HEAD' in line:
                continue

            # Broadly search for prefixes to ignore
            maybe_ip = ipre.search(line)
            if not maybe_ip is None:
                octets = maybe_ip.groups()
                match = False
                for prefix in ignore_prefix:
                    l = len(prefix)
                    if octets[0:l] == prefix:
                        match = True
                if match:
                    continue

            p = line.split(' - ')

            p0 = p[0].split(' ')
            p1 = p[1].split(' ')

            t = time.strptime((p0[2] + p0[3])[1:-1], '%d/%b/%Y:%H:%M:%S+0000')
            maybe_ip_loc = ipre.match(p0[-1])
            if len(p1) >= 3 and not p1[0] in ['-', '"GET', '"PUT']:
                res_hits[p1[2]] += 1

            if not maybe_ip_loc is None:
                if first_in_file:
                    ip_hits[maybe_ip_loc.group()] += 1

                    # Split IP into prefix and suffix
                    octets = maybe_ip_loc.groups()
                    prefix = '.'.join(octets[0:2])
                    suffix = '.'.join(octets[2:4])
                    prefix_hits[prefix].add(suffix)
                    date_hits[time.strftime("%Y-%m-%d", t)] += 1
                    
                    first_in_file = False

print('IPs: ', ip_hits.most_common())
print('IP prefixes: ', sorted([(k, prefix_hits[k]) for k in prefix_hits],
                              key = lambda p: len(p[1]),
                              reverse = True))
print('Dates: ', sorted([(k, date_hits[k]) for k in date_hits]))
print('Resources: ', res_hits.most_common())
