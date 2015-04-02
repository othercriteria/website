#!/usr/bin/env python

import csv
import re
import os
import time
from collections import Counter, defaultdict
import http.client
import json

ipre = re.compile('(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})')

# IP ranges to exclude that aren't caught automatically for bot-like
# behavior
ignore_prefix = [tuple(p.split('.'))
                 for p in ['150.70', '202.46', '75.130.244.15', '64.233']]

# Tests for bot-like behavior
def suspicious_key(key):
    return (key == 'robots.txt' or 'googled' in key)
def suspicious_referrer(referrer):
    return ('semalt' in referrer or 'buttons' in referrer)
def suspicious_agent(agent):
    return ('bot' in agent or 'Bot' in agent or 'spider' in agent or
            'Google favicon' in agent or 'Disqus' in agent or
            'prismatic' in agent)

log_files = os.listdir('logs/')

ip_hits = Counter()
country_hits = Counter()
region_hits = Counter()
date_hits = Counter()
hour_hits = Counter()
res_hits = Counter()
referrer_hits = Counter()
agent_hits = Counter()
possible_robots = set()
excluded = set()

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

# First pass to detect non-human access
for infile in log_files:
    with open(os.path.join('logs', infile), 'r') as log:
        log_reader = csv.reader(log, delimiter = ' ', quotechar = '"')

        for line in log_reader:
            remote_ip_str = line[4]

            if suspicious_key(line[8]):
                possible_robots.add(remote_ip_str)
            if suspicious_referrer(line[16]):
                possible_robots.add(remote_ip_str)
            if suspicious_agent(line[17]):
                possible_robots.add(remote_ip_str)

for infile in log_files:
    with open(os.path.join('logs', infile), 'r') as log:
        log_reader = csv.reader(log, delimiter = ' ', quotechar = '"')

        first_in_file = True
        for line in log_reader:
            # Parse remote IP so it can be checked against prefixes
            remote_ip_str = line[4]
            remote_ip = ipre.search(remote_ip_str)
            octets = ipre.search(remote_ip_str).groups()

            # Check IP against manual prefixes to exclude from analysis
            match = False
            for prefix in ignore_prefix:
                l = len(prefix)
                if octets[0:l] == prefix:
                    match = True
            if match:
                excluded.add(remote_ip_str)
                break

            # Check IP against dynamic possible robots to exclude from analysis
            if remote_ip_str in possible_robots:
                break

            # Only analyze GET requests
            if not line[7] == 'WEBSITE.GET.OBJECT':
                continue

            # Only analyze responses that actually served a real page
            if not line[10][0] == '2':
                continue

            key = line[8]
            if key == '-':
                continue
            res_hits[key] += 1

            if first_in_file:
                # As properties of the client, these only get recorded
                # once per connection

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

                t = time.strptime(line[2][1:], '%d/%b/%Y:%H:%M:%S')
                date_hits[time.strftime("%Y-%m-%d", t)] += 1
                hour_hits[time.strftime("%H+0000'", t)] += 1

                first_in_file = False

# Cache GeoIP data
geoip.dump()

print('IPs:', ip_hits.most_common(), '\n')
print('Countries:', country_hits.most_common(), '\n')
print('Regions:', region_hits.most_common(), '\n')
print('Dates:', sorted([(k, date_hits[k]) for k in date_hits]), '\n')
print('Hours:', sorted([(k, hour_hits[k]) for k in hour_hits]), '\n')
print('Referrers:', referrer_hits.most_common(), '\n')
print('User-Agents:', agent_hits.most_common(), '\n')
print('Resources:', res_hits.most_common(), '\n')
print('Redundant manual exclusions:', possible_robots.intersection(excluded))
