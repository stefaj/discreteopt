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

    for item in items:
        if weight + item.weight <= capacity:
            taken[item.index] = 1
            value += item.value
            weight += item.weight


# Input:
# Values (stored in array v)
# Weights (stored in array w)
# Number of distinct items (n)
# Knapsack capacity (W)
    print "Cap: " + str(capacity) + ", N: " + str(item_count)
    W = capacity
    N = item_count  

    m = {}
    for j in xrange(0,W):
        m[(0, j)] = 0

    for i in xrange(1,len(items)):
        item = items[i]
        for j in xrange(0,capacity):
            if item.weight <= j:
                m[(i,j)] = max(m[(i-1,j)], m[(i-1,j-item.weight)] + item.value)
            else:
                m[(i,j)] = m[(i-1,j)]

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

