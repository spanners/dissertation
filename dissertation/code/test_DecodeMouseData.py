import unittest
import DecodeMouseData as d


class FooTests(unittest.TestCase):

    def setUp(self):
        self.dmd = d.DecodeMouseData()

    def testDecode(self):
        expected = {'1':2, '3':4}
        actual = self.dmd.decode('{"1":2, "3":4}')

        self.assertEquals(actual, expected)

    def testMouseDecode(self):
        expected = {"-JKMBewWrFje3lHT8spD" :
                {"t" : 1397327310399, "y" : 646, "x" : 629}}
        actual = self.dmd.decode(
                '{"-JKMBewWrFje3lHT8spD" : ' +
                '{"t" : 1397327310399, "y" : 646, "x" : 629}}')

        self.assertEquals(actual, expected)

    def testNumClicks(self):
        expected = 1
        actual = self.dmd.getNumberOfClicks(
                '{"-JKMBewWrFje3lHT8spD" : ' +
                '{"t" : 1397327310399, "y" : 646, "x" : 629}}')

        self.assertEquals(actual, expected)

    def testLotsClicks(self):
        expected = 2

        actual = self.dmd.getNumberOfClicks("""{
  "-JKMBewWrFje3lHT8spD" : {
    "t" : 1397327310399,
    "y" : 646,
    "x" : 629
  },
  "-JKMBewawNo6G_Zdfnkk" : {
    "t" : 1397327310465,
    "y" : 646,
    "x" : 629
  }
}""")

        self.assertEquals(actual, expected)

    def testComputeSessionTime(self):
        expected = 0.0011

        actual = self.dmd.getSessionDuration("""{
  "-JKMBewWrFje3lHT8spD" : {
    "t" : 1397327310399,
    "y" : 646,
    "x" : 629
  },
  "-JKMBewawNo6G_Zdfnkk" : {
    "t" : 1397327310465,
    "y" : 646,
    "x" : 629
  }
}""")

        self.assertEquals(actual, expected)

def main():
    unittest.main()

if __name__ == '__main__':
    main()

