#!/usr/bin/env python

import csv
import re
import os
from datetime import datetime
from collections import Counter, defaultdict
import http.client
import json

# Interface to R for analysis and visualization
import rpy2.robjects as robjects
from rpy2.robjects import FloatVector
from rpy2.robjects.packages import importr
base = importr('base')
graphics = importr('graphics')
grdevices = importr('grDevices')
circular = importr('circular')

ipre = re.compile('(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})')

# IP ranges to exclude that aren't caught automatically for bot-like
# behavior
ignore_prefix = [tuple(p.split('.'))
                 for p in ['150.70', '202.46', '75.130.244.15', '64.233',
                           '96.47.226.21', '178.63.97.34', '89.31.57.5',
                           '85.159.113.228', '148.251.77.34',
                           '178.217.187.39']]

# Tests for bot-like behavior
def suspicious_key(key):
    return (key == 'robots.txt' or 'googled' in key)
def suspicious_referrer(referrer):
    return ('semalt' in referrer or 'buttons' in referrer or
            'best-seo' in referrer)
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
time_hits = []
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

                t = datetime.strptime(line[2][1:], '%d/%b/%Y:%H:%M:%S')
                date_hits[t.strftime("%Y-%m-%d")] += 1
                hour_hits[t.strftime("%H+0000'")] += 1
                time_hits.append(t.hour + t.minute / 60 + t.second / 3600)

                first_in_file = False

# Cache GeoIP data
geoip.dump()

# Basic report to console
print('IPs:', ip_hits.most_common(), '\n')
print('Countries:', country_hits.most_common(), '\n')
print('Regions:', region_hits.most_common(), '\n')
print('Dates:', sorted([(k, date_hits[k]) for k in date_hits]), '\n')
print('Hours:', sorted([(k, hour_hits[k]) for k in hour_hits]), '\n')
print('Referrers:', referrer_hits.most_common(), '\n')
print('User-Agents:', agent_hits.most_common(), '\n')
print('Resources:', res_hits.most_common(), '\n')
print('Redundant manual exclusions:', possible_robots.intersection(excluded))
print('\n')

# Generate figures
time_hits_vec = FloatVector(time_hits)
grdevices.png('analytics_out/hits_by_time.png')
graphics.par(mar = [1,1,1,1])
graphics.hist(time_hits_vec, main = 'Hits by time', breaks = 24,
              xlab = 'time (hours past 00:00 UTC)', ylab = 'hits')
grdevices.dev_off()

hits_circ = circular.circular(time_hits_vec,
                              units = 'hours', template = 'clock24')
hits_density = circular.density_circular(hits_circ, bw = 100)
hits_bs = circular.mle_vonmises_bootstrap_ci(hits_circ)
print('Von Mises fit for hits by time (hours past 00:00 UTC)')
print(hits_bs)

grdevices.png('analytics_out/hits_by_time_von_mises_point.png')
graphics.par(mar = [1,1,1,1])
circular.plot_circular(hits_circ, stack = True,
                       main = 'Hits by time (hours past 00:00 UTC)')
for reps in range(100):
    hits_mle = circular.mle_vonmises(hits_circ)
    mle_samp = circular.rvonmises(n = len(time_hits),
                                  mu = hits_mle.rx2('mu'),
                                  kappa = hits_mle.rx2('kappa'))
    mle_density = circular.density_circular(mle_samp, bw = 100)
    graphics.lines(mle_density, offset = 0.5, shrink = 0.5,
                   col = grdevices.rgb(1, 0, 0, 0.1))
graphics.lines(hits_density, offset = 0.5, shrink = 0.5, col = 'blue')
grdevices.dev_off()

grdevices.png('analytics_out/hits_by_time_von_mises_bs.png')
graphics.par(mar = [1,1,1,1])
circular.rose_diag(hits_bs.rx('mu'), bins = 24,
                   main = 'Hits by time (hours past 00:00 UTC)')
for reps in range(100):
    hits_circ_bs = base.sample(hits_circ, replace = True)
    hits_mle = circular.mle_vonmises(hits_circ_bs)
    mle_samp = circular.rvonmises(n = len(time_hits),
                                  mu = hits_mle.rx2('mu'),
                                  kappa = hits_mle.rx2('kappa'))
    mle_density = circular.density_circular(mle_samp, bw = 100)
    graphics.lines(mle_density, offset = 0.5, shrink = 0.5,
                   col = grdevices.rgb(1, 0, 0, 0.1))
graphics.lines(hits_density, offset = 0.5, shrink = 0.5, col = 'blue')
grdevices.dev_off()
