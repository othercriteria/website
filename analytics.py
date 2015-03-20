#!/usr/bin/env python

# Breaking Python 2.x compatibility to use redesigned HTTP libraries

import csv
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
                                               '199.16']]

log_files = os.listdir('logs/')

ip_hits = Counter()
prefix_hits = defaultdict(set)
country_hits = Counter()
region_hits = Counter()
date_hits = Counter()
hour_hits = Counter()
res_hits = Counter()
referrer_hits = Counter()
agent_hits = Counter()

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
        log_reader = csv.reader(l, delimiter = ' ', quotechar = '"')

        first_in_file = True
        for line in log_reader:
            # Only analyze GET requests
            if not line[7] == 'WEBSITE.GET.OBJECT':
                continue

            # Only analyze responses that actually served a real page
            if not line[10][0] == '2':
                continue

            remote_ip_str = line[4]
            remote_ip = ipre.search(remote_ip_str)
            octets = ipre.search(remote_ip_str).groups()

            # Check IP against prefixes to exclude from analysis
            match = False
            for prefix in ignore_prefix:
                l = len(prefix)
                if octets[0:l] == prefix:
                    match = True
            if match:
                continue

            key = line[8]
            if key == '-':
                continue
            res_hits[key] += 1

            if first_in_file:
                ip_hits[remote_ip_str] += 1

                referrer = line[16]
                if not referrer == '-':
                    referrer_hits[referrer] += 1
                agent = line[17]
                if not agent == '-':
                    agent_hits[agent.split(' ')[0]] += 1

                report = geoip.fetch(remote_ip_str)

                if 'country_name' in report:
                    country_name = report['country_name']
                    if not country_name == '':
                        country_hits[report['country_name']] += 1
                if 'region_name' in report:
                    region_name = report['region_name']
                    if not region_name == '':
                        region_hits[report['region_name']] += 1

                prefix_hits[octets[0:2]].add(octets[2:4])

                t = time.strptime(line[2][1:], '%d/%b/%Y:%H:%M:%S')
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
print('Referrers:', referrer_hits.most_common())
print('User-Agents:', agent_hits.most_common())
print('Resources:', res_hits.most_common())
