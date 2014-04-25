from scipy import stats
import numpy as np

elm = [76,487,105,66]
js = [33,12,69,3]

outcome = stats.ttest_ind(elm,js, equal_var=False)

print("t-value: %4.2f, p-value: %4.2f" % (outcome[0], outcome[1]))
