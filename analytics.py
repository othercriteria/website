#!/usr/bin/env python

import csv
from io import StringIO
import re
import os
from datetime import datetime, timedelta
from collections import Counter, defaultdict
import http.client
import json
from math import cos, sin, pi

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

ip_hits = Counter()
ip_hits_all = Counter()
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

class LogDB():
    def __init__(self):
        try:
            with open('cache/logs.json', 'r') as infile:
                self.db = json.load(infile)
        except:
            self.db = []

    def add_new(self):
        new_log_files = os.listdir('logs/')
        for new_log_file in new_log_files:
            f = os.path.join('logs', new_log_file)
            with open(f, 'r') as log:
                self.db.append(log.read())
            os.remove(f)

    def dump(self):
        with open('cache/logs.json', 'w') as outfile:
            json.dump(self.db, outfile)

class GeoIP():
    def __init__(self):
        self.conn = http.client.HTTPConnection('freegeoip.net', timeout = 10)

        try:
            with open('cache/geoip.json', 'r') as infile:
                self.cache = json.load(infile)
        except:
            self.cache = {}

    def fetch(self, ip):
        if ip in self.cache:
            return self.cache[ip]

        try:
            self.conn.request('GET', '/json/' + ip)

            response = self.conn.getresponse()
            print('Querying GeoIP:', response.status, response.reason)

            data_serialized = response.read().decode('utf-8')
            data = json.loads(data_serialized)

            self.cache[ip] = data

            return data
        except:
            return None

    def dump(self):
        with open('cache/geoip.json', 'w') as outfile:
            json.dump(self.cache, outfile)

# Update logs "database"
logs = LogDB()
logs.add_new()
logs.dump()

# Initialize GeoIP "database"
geoip = GeoIP()

# First pass to detect non-human access
for log in logs.db:
    s = StringIO(log)
    log_reader = csv.reader(s, delimiter = ' ', quotechar = '"')

    for line in log_reader:
        remote_ip_str = line[4]
        ip_hits_all[remote_ip_str] += 1

        if suspicious_key(line[8]):
            possible_robots.add(remote_ip_str)
        if suspicious_referrer(line[16]):
            possible_robots.add(remote_ip_str)
        if suspicious_agent(line[17]):
            possible_robots.add(remote_ip_str)

for log in logs.db:
    s = StringIO(log)
    log_reader = csv.reader(s, delimiter = ' ', quotechar = '"')

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
            if not report is None:
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

# Interface to R for analysis and visualization
import rpy2.robjects as robjects
import rpy2.robjects.lib.ggplot2 as ggplot
from rpy2.robjects import IntVector
from rpy2.robjects import FloatVector
from rpy2.robjects import POSIXct
from rpy2.robjects.packages import importr
base = importr('base')
graphics = importr('graphics')
grdevices = importr('grDevices')
circular = importr('circular')
scales = importr('scales')

def rank_abundance_data(counter):
    n = len(counter)
    ranks = IntVector(range(1, n+1))
    counts = [c for (i, c) in counter.most_common()]
    counts_sum = sum(counts)
    fracs_arr = [(c / counts_sum) for c in counts]
    fracs = FloatVector(fracs_arr)

    return ranks, fracs

def rank_abundance_plot(counter, name):
    grdevices.png('analytics_out/{0}_rank_abundance.png'.format(name))
    ranks, fracs = rank_abundance_data(counter)
    df = robjects.DataFrame({'rank': ranks, 'f': fracs})
    pp = ggplot.ggplot(df) + \
        ggplot.aes_string(x = 'rank', y = 'f') + \
        ggplot.geom_point() + \
        ggplot.scale_y_log10(name = 'fraction of hits')
    pp.plot()
    grdevices.dev_off()

rank_abundance_plot(ip_hits, 'ip_hits')
rank_abundance_plot(ip_hits_all, 'ip_hits_all')
rank_abundance_plot(country_hits, 'country_hits')
rank_abundance_plot(region_hits, 'region_hits')

# Restructure date data
# TODO: make this less hacky
start = base.as_Date(min(date_hits.elements()))
end = base.as_Date(max(date_hits.elements()))
dates = base.seq(start, end, by = 'day')
hits_arr = [0] * len(dates)
for date in date_hits:
    hits_arr[int(base.as_Date(date)[0]) - int(start[0])] = date_hits[date]
hits = IntVector(hits_arr)

grdevices.png('analytics_out/hits_by_date.png')
df = robjects.DataFrame({'date': dates, 'hits': hits})
pp = ggplot.ggplot(df) + \
    ggplot.aes_string(x = 'date', y = 'hits') + \
    ggplot.geom_point() + \
    ggplot.geom_smooth(method = 'loess')
pp.plot()
grdevices.dev_off()

# Restructure hour data
hour_arr = []
hits_arr = []
for hour in hour_hits:
    hour_arr.append(datetime.min + timedelta(hours = int(hour[0:2])))
    hits_arr.append(hour_hits[hour])
hour = POSIXct(hour_arr)
hits = IntVector(hits_arr)

grdevices.png('analytics_out/hits_by_time.png')
df = robjects.DataFrame({'hour': POSIXct(hour), 'hits': IntVector(hits)})
pp = ggplot.ggplot(df) + \
    ggplot.aes_string(x = 'hour', y = 'hits') + \
    ggplot.scale_x_datetime(labels = scales.date_format('%H:%M UTC')) + \
    ggplot.geom_bar(stat = 'identity')
pp.plot()
grdevices.dev_off()

# Restructure circular hit time datax
time_hits_vec = FloatVector(time_hits)
hits_circ = circular.circular(time_hits_vec,
                              units = 'hours', template = 'clock24')
hits_density = circular.density_circular(hits_circ, bw = 100)

print('Von Mises fit for hits by time (hours past 00:00 UTC)')
hits_mle = circular.mle_vonmises(hits_circ)
mu = base.cbind(hits_mle.rx('mu'))[0][0]
mu_se = hits_mle.rx('se.mu')[0][0]
kappa = hits_mle.rx('kappa')[0][0]
kappa_se = hits_mle.rx('se.kappa')[0][0]
print('MLE: mu = %0.2f (%0.2f)  kappa = %0.2f (%0.2f)' %
      (mu, mu_se, kappa, kappa_se))
hits_bs = circular.mle_vonmises_bootstrap_ci(hits_circ)
print(hits_bs)

grdevices.png('analytics_out/hits_by_time_von_mises_point.png')
graphics.par(mar = [1,1,1,1])
circular.plot_circular(hits_circ, stack = True,
                       main = 'Hits by time (hours past 00:00 UTC)')
hits_mle = circular.mle_vonmises(hits_circ)
for reps in range(100):
    mle_samp = circular.rvonmises(n = len(hits_circ),
                                  mu = hits_mle.rx2('mu'),
                                  kappa = hits_mle.rx2('kappa'))
    mle_density = circular.density_circular(mle_samp, bw = 100)
    graphics.lines(mle_density, offset = 0.5, shrink = 0.5,
                   col = grdevices.rgb(1, 0, 0, 0.1))
graphics.lines(hits_density, offset = 0.5, shrink = 0.5, col = 'blue')
grdevices.dev_off()

def moment_plot(c, col):
    m_clock = circular.mean_circular(c)
    m_circ = circular.conversion_circular(m_clock, units = 'radians',
                                          rotation = 'counter', zero = 0)
    m = base.cbind(m_circ)[0]
    v = base.cbind(circular.var_circular(c))[0]
    graphics.points((1 - v) * cos(m), (1 - v) * sin(m), col = col)

grdevices.png('analytics_out/hits_by_time_von_mises_bs.png')
graphics.par(mar = [1,1,1,1])
circular.rose_diag(hits_bs.rx('mu'), bins = 24,
                   main = 'Hits by time (hours past 00:00 UTC)')
for reps in range(100):
    hits_circ_bs = base.sample(hits_circ, replace = True)
    bs_density = circular.density_circular(hits_circ_bs, bw = 100)
    graphics.lines(bs_density, offset = 0.5, shrink = 0.5,
                   col = grdevices.rgb(0, 0, 0, 0.1))
    moment_plot(hits_circ_bs, grdevices.rgb(1, 0, 0, 0.1))
    hits_mle = circular.mle_vonmises(hits_circ_bs)
    mle_samp = circular.rvonmises(n = len(hits_circ),
                                  mu = hits_mle.rx2('mu'),
                                  kappa = hits_mle.rx2('kappa'))
    mle_density = circular.density_circular(mle_samp, bw = 100)
    graphics.lines(mle_density, offset = 0.5, shrink = 0.5,
                   col = grdevices.rgb(1, 0, 0, 0.1))
graphics.lines(hits_density, offset = 0.5, shrink = 0.5, col = 'blue')
moment_plot(hits_circ, 'blue')
grdevices.dev_off()
