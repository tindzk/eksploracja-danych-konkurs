# This script visualises the x and y coordinate of all datapoints in the given
# CSV file, colouring the cluster the datapoint was associated to accordingly.

import csv
import numpy as np
import pylab as pl

# See http://stackoverflow.com/questions/11885060/how-to-shade-points-in-scatter-based-on-colormap-in-matplotlib
colors = np.r_[np.linspace(0.1, 1, 5), np.linspace(0.1, 1, 5)] 
mymap = pl.get_cmap("Reds")
my_colors = mymap(colors)

with open('data.csv', 'r') as csvfile:
	rows = list(csv.reader(csvfile, delimiter=','))
		
	for n in range(0,9):
		x = []
		y = []

		for row in rows:
			if (int(row[len(row) - 1]) == n):
				x.append(float(row[0]))
				y.append(float(row[1]))

		if (x == []):
			break

		pl.scatter(x, y, s=40,
		           color=my_colors[n], edgecolors='None',
		           label="cluster %i" % (n + 1))

	pl.legend(scatterpoints=1)
	pl.tight_layout()
	pl.show()