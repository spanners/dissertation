import ClicksPerCategory
import sys
import json

cpc = ClicksPerCategory.ClicksPerCategory()

def get_all_clicks():
    _all = dict()
    contents = ""
    fname = ""
    region1 = (0,538, 330,556)
    for _file in sys.argv[1:]:
        with open(_file) as f:
            fname, contents = f.name, f.read()
        _all[fname] = cpc.get_clicks_in_region(contents, *region1)
    return _all

_all = get_all_clicks()
total = 0
for fname in _all:
    length = len(_all[fname])
    print fname, length
    total += length
print "TOTAL:", total
