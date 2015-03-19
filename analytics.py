#!/usr/bin/env python

# Breaking Python 2.x compatibility to use redesigned HTTP libraries

import re
import os
import time
from collections import Counter, defaultdict
import http.client
import json

ipre = re.compile('(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})')

# Ignore Google, Baidu, Yandex, Trend Micro, Bing, Yahoo, etc.
ignore_prefix = [tuple(p.split('.')) for p in ['66.249', '180.76', '100.43',
                                               '150.70', '202.46', '104.154',
                                               '199.21.99', '157.55',
                                               '74.6.254.111',
                                               '96.238.59.67',
                                               '75.130.244.15',
                                               '199.16.156.125',
                                               '199.16']]

log_files = os.listdir('logs/')

ip_hits = Counter()
prefix_hits = defaultdict(set)
country_hits = Counter()
region_hits = Counter()
date_hits = Counter()
hour_hits = Counter()
res_hits = Counter()

class GeoIP():
    def __init__(self):
        self.conn = http.client.HTTPConnection('freegeoip.net')

        try:
            with open('cache/geoip.json', 'r') as infile:
                self.cache = json.load(infile)
        except:
            self.cache = {}

    def fetch(self, ip):
        if ip in self.cache:
            return self.cache[ip]
        
        self.conn.request('GET', '/json/' + ip)

        response = self.conn.getresponse()
        print('Querying GeoIP:', response.status, response.reason)

        data_serialized = response.read().decode('utf-8')
        data = json.loads(data_serialized)

        self.cache[ip] = data
        
        return data

    def dump(self):
        with open('cache/geoip.json', 'w') as outfile:
            json.dump(self.cache, outfile)

geoip = GeoIP()

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

                    report = geoip.fetch(maybe_ip_loc.group())

                    if 'country_name' in report:
                        country_name = report['country_name']
                        if not country_name == '':
                            country_hits[report['country_name']] += 1
                    if 'region_name' in report:
                        region_name = report['region_name']
                        if not region_name == '':
                            region_hits[report['region_name']] += 1

                    # Split IP into prefix and suffix
                    octets = maybe_ip_loc.groups()
                    prefix = '.'.join(octets[0:2])
                    suffix = '.'.join(octets[2:4])
                    prefix_hits[prefix].add(suffix)
                    date_hits[time.strftime("%Y-%m-%d", t)] += 1
                    hour_hits[time.strftime("%H+0000'", t)] += 1
                    
                    first_in_file = False

# Cache GeoIP data
geoip.dump()
                    
print('IPs:', ip_hits.most_common())
print('IP prefixes:', sorted([(k, prefix_hits[k]) for k in prefix_hits
                              if len(prefix_hits[k]) > 1],
                             key = lambda p: len(p[1]),
                             reverse = True))
print('Countries:', country_hits.most_common())
print('Regions:', region_hits.most_common())
print('Dates:', sorted([(k, date_hits[k]) for k in date_hits]))
print('Hours:', sorted([(k, hour_hits[k]) for k in hour_hits]))
print('Resources:', res_hits.most_common())
