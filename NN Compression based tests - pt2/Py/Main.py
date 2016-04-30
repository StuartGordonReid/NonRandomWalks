import numpy

x = ['window size', 'decimation', 'levels', 'dow mean', 'rand mean', 'rand stdev']

for x1 in numpy.repeat("Sim", 10):
    x.append(str(x1))

print(x)

