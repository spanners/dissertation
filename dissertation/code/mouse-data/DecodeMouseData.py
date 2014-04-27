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

    def getFilesTimeClickDict(self, files):
        timeAndClicks = dict()
        filename = str()
        for jsonFile in files:
            with open(jsonFile) as f:
                jsonString = f.read()
                filename = f.name
                print filename
            timeAndClicks[filename] = (self.getSessionDuration(jsonString),
                    self.getNumberOfClicks(jsonString))
        return timeAndClicks

    def getDictPrettyPrint(self, timeAndClicks):
        output = ""
        tablelines = "-"*11 + " " + "-"*10
        output += tablelines + "\n"
        output += "Time (min)  Clicks\n"
        output += tablelines + "\n"
        length = len(timeAndClicks)
        for i,filename in enumerate(timeAndClicks):
            output += ("%10f %10d" % (timeAndClicks[filename][0],
                    timeAndClicks[filename][1])) + "\n"
        output += tablelines
        return output


if __name__ == "__main__":
    dmd = DecodeMouseData()
    print dmd.getDictPrettyPrint(dmd.getFilesTimeClickDict(sys.argv[1:]))
    timeAndClicks = dmd.getFilesTimeClickDict(sys.argv[1:])
    total = 0
    for filename in timeAndClicks:
        total += timeAndClicks[filename][1]
    print "TOTAL: ", total
