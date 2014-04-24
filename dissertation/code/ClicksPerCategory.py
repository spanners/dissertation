import unittest
import json

class ClicksPerCategory(object):
    def get_clicks_in_region(self, jsonStr, x_min, y_min, x_max, y_max):
        region = {}
        mouse_data = json.loads(jsonStr)
        for click in mouse_data:
            coords = mouse_data[click]
            if (x_min <= coords["x"] <= x_max) and (y_min <= coords["y"] <= y_max):
                region[click] = coords
        return region


class FooTests(unittest.TestCase):

    def setUp(self):
       self.mr = ClickPerCategory()

    def test_get_clicks_in_region(self):
        jsonStr = '{"uniq_id1": {"t": 1, "x":1, "y":2}, "uniq_id2": {"t":2, "x":3, "y":4}}'
        x_min = 0
        x_max = 4
        y_min = 3
        y_max = 4
        expected = {u"uniq_id2": {u"t":2, u"x":3, u"y":4}}
        actual = self.mr.get_clicks_in_region(jsonStr, x_min, y_min, x_max,
                y_max)
        self.assertEquals(actual, expected)

def main():
    unittest.main()

if __name__ == '__main__':
    main()

