#!/usr/bin/python
# -*- coding: utf-8 -*-

from collections import namedtuple
Item = namedtuple("Item", ['index', 'value', 'weight'])

def solve_it(input_data):
    # Modify this code to run your optimization algorithm

    # parse the input
    lines = input_data.split('\n')

    firstLine = lines[0].split()
    item_count = int(firstLine[0])
    capacity = int(firstLine[1])

    items = []

    for i in range(1, item_count+1):
        line = lines[i]
        parts = line.split()
        items.append(Item(i-1, int(parts[0]), int(parts[1])))

    # a trivial greedy algorithm for filling the knapsack
    # it takes items in-order until the knapsack is full
    value = 0
    weight = 0
    taken = [0]*len(items)

# Input:
# Values (stored in array v)
# Weights (stored in array w)
# Number of distinct items (n)
# Knapsack capacity (W)
    print "Cap: " + str(capacity) + ", N: " + str(item_count)
    W = capacity
    N = item_count  

    V = {}
    keep = {}
    for w in xrange(0,W+1):
        V[(0, w)] = 0
        for i in xrange(0,N+1):
            keep[(i,w)] = 0

    for w in xrange(0,W+1):
        for item in items:
            i = item.index
            if i==0:
                continue
            vi = item.value
            wi = item.weight
            if (item.weight > w):
                V[(i,w)] = V[(i-1,w)]
                value = V[(i,w)]
            else:
                V[(i,w)] = max (V[(i-1,w)], vi + V[(i-1,w-wi)])
                if (vi + V[(i-1,w-wi)] > V[(i-1,w)]):
                    keep[(i,w)] = 1
                value = V[(i,w)]

    K = W
    for i in xrange(N,0,-1):
        if keep[(i,K)] == 1:
            taken[i] = 1
            K = K - items[i].weight

    # prepare the solution in the specified output format
    output_data = str(value) + ' ' + str(0) + '\n'
    output_data += ' '.join(map(str, taken))
    return output_data


import sys

if __name__ == '__main__':
    if len(sys.argv) > 1:
        file_location = sys.argv[1].strip()
        input_data_file = open(file_location, 'r')
        input_data = ''.join(input_data_file.readlines())
        input_data_file.close()
        print solve_it(input_data)
    else:
        print 'This test requires an input file.  Please select one from the data directory. (i.e. python solver.py ./data/ks_4_0)'

