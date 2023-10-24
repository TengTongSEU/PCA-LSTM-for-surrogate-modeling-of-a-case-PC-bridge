#
# ---------------------  Part 1: modulus import ------------------------#
# -*- coding: utf-8 -*-
from odbAccess import *
from abaqusConstants import*
import csv

# ---------------------  Part 1: modulus import ------------------------#	
filePath = 'node.txt'
	
print 'Parsing file:', 
odbFromTextFile = open(filePath)
odbFromTextLines = odbFromTextFile.readlines()

nodeData = []

for line in odbFromTextLines:
    label, nodeX, nodeY, nodeZ = eval(line)
    nodeData.append((label, nodeX, nodeY, nodeZ)) 
	
# ---------------------  Part 1: modulus import ------------------------#	
filePath = 'element.txt'
	
print 'Parsing file:', 
odbFromTextFile = open(filePath)
odbFromTextLines = odbFromTextFile.readlines()

elementData = []

for line in odbFromTextLines:
    label, c1, c2, c3, c4, c5, c6, c7, c8 = eval(line)
    elementData.append(( label, c1, c2, c3, c4, c5, c6, c7, c8 )) 

# ---------------------  Part 1: modulus import ------------------------#	
filePath = 'disp.txt'
	
print 'Parsing file:', 
odbFromTextFile = open(filePath)
odbFromTextLines = odbFromTextFile.readlines()

nodeDisplacementLabelData = []
nodeDisplacementData  = []

for line in odbFromTextLines:
    label, nodeX, nodeY, nodeZ = eval(line)
    nodeDisplacementLabelData.append(label)
    nodeDisplacementData.append( (nodeX, nodeY, nodeZ) )	