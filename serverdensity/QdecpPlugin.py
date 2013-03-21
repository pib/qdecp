""" QDECP Plugin for Server Density """
import json
import urllib2

class QdecpPlugin(object):
    def __init__(self, agentConfig, checksLogger, rawConfig):
        self.config = agentConfig
        self.log = checksLogger
        self.raw_config = rawConfig

    def run(self):
        stat_url = self.raw_config['Qdecp']['stats_url']

        blacklist = set(filter(bool, [s.strip() for s in
                                      self.raw_config['Qdecp'].get('blacklist', '').split(',')]))
        try:
            res = urllib2.urlopen(stat_url)
            all_stats = json.load(res)
            stats = {}
            def filter_key(key):
                for k in blacklist:
                    if key.endswith(k):
                        return False
                return True
                
            stats = {k: v for k, v in all_stats.iteritems() if filter_key(k)}
            self.log.info('QdecpPlugin: Loaded Qdecp stats OK. Included {} of {} fields.'.format(
                    len(stats), len(all_stats)))
        except Exception, e:
            self.log.error('QdecpPlugin: Failed to load Qdecp stats.')
            self.log.error(e)
            stats = {}

        return stats
