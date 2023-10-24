#
# ---------------------  Part 1: modulus import ------------------------#
# -*- coding: utf-8 -*-
from odbAccess import *
from abaqusConstants import*
import csv

# ---------------------  Part 2: open odb files ------------------------#
root =  str('001')
#
odb          =   openOdb(path = root + '.odb')
CsvFilePath  =   'H:\P-13 Concrete creep statistical\Preloss\Pres_loss_10'+ root + str('.csv')


# ---------------------  Part 3: element identification ----------------#
#
TENDON_TOP = ['STAGE1-TS','STAGE2-TS','STAGE3-TS','STAGE4-TS','STAGE5-TS','STAGE6-TS','STAGE7-TS','STAGE8-TS','STAGE9-TS','STAGE10-TS', \
			  'STAGE11-TS','STAGE12-TS','STAGE13-TS','STAGE14-TS','STAGE15-TS','STAGE16-TS','STAGE17-TS','STAGE18-TS','STAGE19-TS','STAGE20-TS', \
			  'STAGE21-TS','STAGE22-TS','STAGE23-TS','STAGE24-TS','STAGE25-TS','STAGE26-TS','STAGE27-TS','STAGE28-TS','STAGE29-TS','STAGE30-TS', \
			  'STAGE31-TS','STAGE32-36-TS']
#
Time_total     = []
Avestress1     = []
Avestress2     = []
Avestress3     = []
Avestress4     = []
Avestress5     = []
Avestress6     = []
Avestress7     = []
Avestress8     = []
Avestress9     = []
Avestress10    = []
Avestress11    = []
Avestress12    = []
Avestress13    = []
Avestress14    = []
Avestress15    = []
Avestress16    = []
Avestress17    = []
Avestress18    = []
Avestress19    = []
Avestress20    = []
Avestress21    = []
Avestress22    = []
Avestress23    = []
Avestress24    = []
Avestress25    = []
Avestress26    = []
Avestress27    = []
Avestress28    = []
Avestress29    = []
Avestress30    = []
Avestress31    = []
Avestress32    = []
    
#
# ---------------------  Part 4: Stress averaging (half year)----------------------#
#
step = odb.steps['ONEYEAR']
t1   = step.totalTime                     # time till the 'Step-1'
#

for frame in step.frames:
    tinst     =  frame.frameValue # time for the frame in the current step
    tcurrent  =  t1 + tinst
    Time_total.append(tcurrent)
	#  ***************************************
    field    = frame.fieldOutputs['S']
	#  ***************************************
	#  stage1-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[0] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress1.append(ave.data[0]) 
	#  ***************************************
	#  stage2-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[1] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress2.append(ave.data[0]) 
	#  ***************************************
	#  stage3-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[2] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress3.append(ave.data[0]) 
	#  ***************************************
	#  stage4-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[3] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress4.append(ave.data[0]) 
	#  ***************************************
	#  stage5-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[4] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress5.append(ave.data[0]) 
	#  ***************************************
	#  stage6-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[5] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress6.append(ave.data[0]) 
	#  ***************************************
	#  stage7-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[6] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress7.append(ave.data[0]) 
	#  ***************************************
	#  stage8-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[7] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress8.append(ave.data[0]) 
	#  ***************************************
	#  stage9-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[8] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress9.append(ave.data[0]) 
	#  ***************************************
	#  stage10-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[9] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress10.append(ave.data[0]) 
	#  ***************************************
	#  stage11-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[10] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress11.append(ave.data[0]) 
	#  ***************************************
	#  stage12-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[11] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress12.append(ave.data[0]) 
	#  ***************************************
	#  stage13-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[12] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress13.append(ave.data[0]) 
	#  ***************************************
	#  stage14-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[13] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress14.append(ave.data[0]) 
	#  ***************************************
	#  stage15-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[14] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress15.append(ave.data[0]) 
	#  ***************************************
	#  stage16-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[15] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress16.append(ave.data[0]) 
	#  ***************************************
	#  stage17-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[16] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress17.append(ave.data[0]) 
	#  ***************************************
	#  stage18-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[17] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress18.append(ave.data[0]) 
	#  ***************************************
	#  stage19-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[18] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress19.append(ave.data[0]) 
	#  ***************************************
	#  stage20-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[19] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress20.append(ave.data[0]) 
	#  ***************************************
	#  stage21-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[20] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress21.append(ave.data[0]) 
	#  ***************************************
	#  stage22-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[21] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress22.append(ave.data[0]) 
	#  ***************************************
	#  stage23-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[22] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress23.append(ave.data[0]) 
	#  ***************************************
	#  stage24-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[23] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress24.append(ave.data[0]) 
	#  ***************************************
	#  stage25-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[24] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress25.append(ave.data[0]) 
	#  ***************************************
	#  stage26-TS6
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[25] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress26.append(ave.data[0]) 
	#  ***************************************
	#  stage27-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[26] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress27.append(ave.data[0]) 
	#  ***************************************
	#  stage28-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[27] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress28.append(ave.data[0]) 
	#  ***************************************
	#  stage29-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[28] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress29.append(ave.data[0]) 
	#  ***************************************
	#  stage30-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[29] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress30.append(ave.data[0]) 
	#  ***************************************
	#  stage31-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[30] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress31.append(ave.data[0]) 
	#  ***************************************
	#  stage32-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[31] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress32.append(ave.data[0]) 
#
# ---------------------  Part 5: Stress averaging (nine year)----------------------#
#
step = odb.steps['Nine YEARS']
t1   = step.totalTime                     # time till the 'Step-1'
#
for frame in step.frames:
    tinst     =  frame.frameValue # time for the frame in the current step
    tcurrent  =  t1 + tinst
    Time_total.append(tcurrent)
	#  ***************************************
    field    = frame.fieldOutputs['S']
	#  ***************************************
	#  stage1-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[0] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress1.append(ave.data[0]) 
	#  ***************************************
	#  stage2-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[1] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress2.append(ave.data[0]) 
	#  ***************************************
	#  stage3-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[2] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress3.append(ave.data[0]) 
	#  ***************************************
	#  stage4-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[3] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress4.append(ave.data[0]) 
	#  ***************************************
	#  stage5-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[4] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress5.append(ave.data[0]) 
	#  ***************************************
	#  stage6-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[5] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress6.append(ave.data[0]) 
	#  ***************************************
	#  stage7-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[6] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress7.append(ave.data[0]) 
	#  ***************************************
	#  stage8-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[7] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress8.append(ave.data[0]) 
	#  ***************************************
	#  stage9-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[8] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress9.append(ave.data[0]) 
	#  ***************************************
	#  stage10-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[9] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress10.append(ave.data[0]) 
	#  ***************************************
	#  stage11-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[10] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress11.append(ave.data[0]) 
	#  ***************************************
	#  stage12-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[11] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress12.append(ave.data[0]) 
	#  ***************************************
	#  stage13-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[12] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress13.append(ave.data[0]) 
	#  ***************************************
	#  stage14-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[13] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress14.append(ave.data[0]) 
	#  ***************************************
	#  stage15-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[14] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress15.append(ave.data[0]) 
	#  ***************************************
	#  stage16-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[15] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress16.append(ave.data[0]) 
	#  ***************************************
	#  stage17-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[16] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress17.append(ave.data[0]) 
	#  ***************************************
	#  stage18-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[17] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress18.append(ave.data[0]) 
	#  ***************************************
	#  stage19-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[18] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress19.append(ave.data[0]) 
	#  ***************************************
	#  stage20-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[19] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress20.append(ave.data[0]) 
	#  ***************************************
	#  stage21-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[20] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress21.append(ave.data[0]) 
	#  ***************************************
	#  stage22-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[21] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress22.append(ave.data[0]) 
	#  ***************************************
	#  stage23-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[22] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress23.append(ave.data[0]) 
	#  ***************************************
	#  stage24-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[23] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress24.append(ave.data[0]) 
	#  ***************************************
	#  stage25-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[24] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress25.append(ave.data[0]) 
	#  ***************************************
	#  stage26-TS6
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[25] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress26.append(ave.data[0]) 
	#  ***************************************
	#  stage27-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[26] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress27.append(ave.data[0]) 
	#  ***************************************
	#  stage28-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[27] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress28.append(ave.data[0]) 
	#  ***************************************
	#  stage29-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[28] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress29.append(ave.data[0]) 
	#  ***************************************
	#  stage30-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[29] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress30.append(ave.data[0]) 
	#  ***************************************
	#  stage31-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[30] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress31.append(ave.data[0]) 
	#  ***************************************
	#  stage32-TS
    TENDON_TEMP = odb.rootAssembly.instances['PART-1-1'].elementSets[ TENDON_TOP[31] ]
    subField = field.getSubset(region = TENDON_TEMP)
    sum      = 0
    for val in subField.values:
        sum = sum + val
    ave    = sum / len(subField.values)
    Avestress32.append(ave.data[0]) 
	
	

#	
# ---------------------  Part 5: data output ---------------------------#
#
	


#open csv file
csvFile = open(CsvFilePath,'wb')

### wrte data ###

writer=csv.writer(csvFile)

#writer.writerow([  'Totaltime','STAGE1-TS','STAGE2-TS','STAGE3-TS','STAGE4-TS','STAGE5-TS','STAGE6-TS','STAGE7-TS','STAGE8-TS','STAGE9-TS','STAGE10-TS', \
#                   'STAGE11-TS','STAGE12-TS','STAGE13-TS','STAGE14-TS','STAGE15-TS','STAGE16-TS','STAGE17-TS','STAGE18-TS','STAGE19-TS','STAGE20-TS', \
#                   'STAGE21-TS','STAGE22-TS','STAGE23-TS','STAGE24-TS','STAGE25-TS','STAGE26-TS','STAGE27-TS','STAGE28-TS','STAGE29-TS','STAGE30-TS', \
#                   'STAGE31-TS','STAGE32-36-TS'  ])


i=0
while i < len(Time_total):
    writer.writerow( [  Time_total[i], \
                      Avestress1[i], Avestress2[i], Avestress3[i], Avestress4[i], Avestress5[i], Avestress6[i], Avestress7[i], Avestress8[i], Avestress9[i], Avestress10[i], \
					  Avestress11[i], Avestress12[i], Avestress13[i], Avestress14[i], Avestress15[i], Avestress16[i], Avestress17[i], Avestress18[i], Avestress19[i], Avestress20[i], \
					  Avestress21[i], Avestress22[i], Avestress23[i], Avestress24[i], Avestress25[i], Avestress26[i], Avestress27[i], Avestress28[i], Avestress29[i], Avestress30[i], \
					  Avestress31[i], Avestress32[i]   ])
    i=i+1

#writer.writerow( [  Time_total[i], \
#                      Avestress1[i]-Avestress1[0],   Avestress2[i]-Avestress2[0],   Avestress3[i]-Avestress3[0],   Avestress4[i]-Avestress4[0],   Avestress5[i]-Avestress5[0],   Avestress6[i]-Avestress6[0],   Avestress7[i]-Avestress7[0],   Avestress8[i]-Avestress8[0],   Avestress9[i]-Avestress9[0],   Avestress10[i]-Avestress10[0], \
#					  Avestress11[i]-Avestress11[0], Avestress12[i]-Avestress12[0], Avestress13[i]-Avestress13[0], Avestress14[i]-Avestress14[0], Avestress15[i]-Avestress15[0], Avestress16[i]-Avestress16[0], Avestress17[i]-Avestress17[0], Avestress18[i]-Avestress18[0], Avestress19[i]-Avestress19[0],  Avestress20[i]-Avestress20[0], \
#					  Avestress21[i]-Avestress21[0], Avestress22[i]-Avestress22[0], Avestress23[i]-Avestress23[0], Avestress24[i]-Avestress24[0], Avestress25[i]-Avestress25[0], Avestress26[i]-Avestress26[0], Avestress27[i]-Avestress27[0], Avestress28[i]-Avestress28[0], Avestress29[i]-Avestress29[0], Avestress30[i]-Avestress30[0], \
#					  Avestress31[i]-Avestress31[0], Avestress32[i]-Avestress32[0]   ])
### close file ###

csvFile.close()