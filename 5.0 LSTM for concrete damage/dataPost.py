#
# ---------------------  Part 1: modulus import ------------------------#	

from odbAccess import *
# 移除前面调试过程中生成的 ODB 文件
import os, osutils
if os.path.exists('odbFromText.odb'):
    osutils.remove('odbFromText.odb')

odb = Odb('OdbFromText', analysisTitle='create file odbFromText.odb', description='creat a model use external data', path='odbFromText.odb')

part1 = odb.Part(name='part-1', embeddedSpace=THREE_D, type=DEFORMABLE_BODY)
part1.addNodes(nodeData=nodeData, nodeSetName='nset-1')
del nodeData

part1.addElements(elementData=elementData, type='C3D8',
    elementSetName='eset-1')
del elementData

#
instance1 = odb.rootAssembly.Instance(name='part-1-1', object=part1)
#
step1 = odb.Step(name='step-1',  description='', domain=TIME, timePeriod=1.0)


#
frame = step1.Frame(frameId = 1, frameValue= 1)


uField = frame.FieldOutput(name='U', description='Displacements', type=VECTOR)
uField.addData(position=NODAL, instance=instance1,
        labels = nodeDisplacementLabelData,
        data   = nodeDisplacementData)
		
del nodeDisplacementLabelData
del nodeDisplacementData

step1.setDefaultDeformedField(uField)



	
	
odb.save() 
odb.close()