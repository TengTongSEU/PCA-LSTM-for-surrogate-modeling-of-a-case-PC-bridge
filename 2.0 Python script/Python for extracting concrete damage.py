#
# ---------------------  Part 1: modulus import ------------------------#
# -*- coding: utf-8 -*-
from odbAccess import *
from abaqusConstants import*
import csv

# ---------------------  Part 2: open odb files ------------------------#
root =  str('243')
#
odb          =   openOdb(path = root + '.odb')
CsvFilePath  =   'H:\P-13 Concrete creep statistical\damage\damage'+ root + str('.csv')


# ---------------------  Part 3: element identification ----------------#
#
ALLCONCRETE = ['ALLCONCRETE']
#
Time_total     = []
Ele_Label      = []
Ip_Label       = []
Dam_Label      = []
    
#
# ---------------------  Part 4: Stress averaging (half year)----------------------#
#
step = odb.steps['ONEYEAR']
t1   = step.totalTime                     # time till the 'Step-1'
#

for frame in step.frames:
    tinst     =  frame.frameValue # time for the frame in the current step
    tcurrent  =  t1 + tinst
	#  ***************************************
    field    = frame.fieldOutputs['DAMAGET']
	#  ***************************************
	#  stage1-TS
    ALLCONCRETE = odb.rootAssembly.instances['PART-1-1'].elementSets[ 'ALLCONCRETE' ]
    subField    = field.getSubset(region = ALLCONCRETE)
	#
    for val in range(0, (len(subField.values)/8) ):
        Time_total.append(tcurrent)
        Ele_Label.append(subField.values[8*val].elementLabel)
        Ip_Label.append (subField.values[8*val].integrationPoint) 	
        Dam_Label.append(subField.values[8*val].data) 



		
step = odb.steps['Nine YEARS']
t1   = step.totalTime                     # time till the 'Step-1'   

for frame in step.frames:
    tinst     =  frame.frameValue # time for the frame in the current step
    tcurrent  =  t1 + tinst
	#  ***************************************
    field    = frame.fieldOutputs['DAMAGET']
	#  ***************************************
	#  stage1-TS
    ALLCONCRETE = odb.rootAssembly.instances['PART-1-1'].elementSets[ 'ALLCONCRETE' ]
    subField    = field.getSubset(region = ALLCONCRETE)
	#
    for val in range(0, (len(subField.values)/8) ):
        Time_total.append(tcurrent)
        Ele_Label.append(subField.values[8*val].elementLabel)
        Ip_Label.append (subField.values[8*val].integrationPoint) 	
        Dam_Label.append(subField.values[8*val].data) 
#	
# ---------------------  Part 5: data output ---------------------------#
#
	


#open csv file
csvFile = open(CsvFilePath,'wb')

### wrte data ###

writer=csv.writer(csvFile)

i=0
while i < len(Time_total):
    writer.writerow( [  Time_total[i], Ele_Label[i], Dam_Label[i]  ])
    i=i+1


csvFile.close()