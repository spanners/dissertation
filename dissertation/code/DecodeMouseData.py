#!/usr/bin/python2.7

import json
import sys

class DecodeMouseData(object):

    def decode(self, jsonString):
        return json.loads(jsonString)

    def getNumberOfClicks(self, jsonString):
        return len(self.decode(jsonString))

    def getSessionDuration(self, jsonString):
        decoded = self.decode(jsonString)

        finish = max([x["t"] for x in decoded.values()])
        start = min([x["t"] for x in decoded.values()])

        # compute number of minutes in this number of milliseconds
        return (finish - start) / 60000.0

if __name__ == "__main__":
    dmd = DecodeMouseData()

    with open(sys.argv[1]) as f:
        jsonString = f.read()
        print "Time:", dmd.getSessionDuration(jsonString)
        print "Clicks:", dmd.getNumberOfClicks(jsonString)

