
clc
clear all
close all

% LSTM prediction of humen bridge
% 
% the 1st column: the sequence of numerical study
% 
% the 2nd - 7th column: the 6 variables
% 
% the 8th column: the time after bridge completion
% 
% the 9th column: the mid-span deflection

%%
% [XTrain, YTrain, cases] = prepareDataTrain();
%
% XTrain = XTrain(1:10);
% YTrain = YTrain(1:10);
% X = prepareDataTrain(dataTrain);                                 % n 行, m 列

load('Data_damage.mat')

XTest = XTrain(301:582);
YTest = YTrain(301:582);

XTrain = XTrain(1:300);
YTrain = YTrain(1:300);

%%
%将训练预测变量归一化为具有零均值和单位方差。要计算所有观测值的均值和标准差，请水平串联序列数据。

mu  = mean([XTrain{:}],2);
sig = std([XTrain{:}],0,2);

for i = 1:numel(XTrain)
    XTrain{i} = (XTrain{i} - mu) ./ sig;
end
 

%裁剪响应


%准备要填充的数据

%要最大程度地减少添加到小批量的填充量，请按序列长度对训练数据进行排序。然后，选择可均匀划分训练数据的小批量大小，并减少小批量中的填充量。

%按序列长度对训练数据进行排序。

for i=1:numel(XTrain)
    sequence = XTrain{i};
    sequenceLengths(i) = size(sequence,2);
end

[sequenceLengths,idx] = sort(sequenceLengths,'descend');
XTrain = XTrain(idx);
YTrain = YTrain(idx);
%在条形图中查看排序的序列长度。

figure
bar(sequenceLengths)
xlabel("Sequence")
ylabel("Length")
title("Sorted Data")

%% network construction
%
numResponses = size(YTrain{1},1);
featureDimension = size(XTrain{1},1);
numHiddenUnits = 200;

layers = [ ...
    sequenceInputLayer(featureDimension)
    lstmLayer(numHiddenUnits,'OutputMode','sequence')
    fullyConnectedLayer(50)
    dropoutLayer(0.5)
    fullyConnectedLayer(numResponses)
    regressionLayer];

%指定训练选项。使用求解器 'adam' 以大小为 20 的小批量进行 60 轮训练。指定学习率为 0.01。要防止梯度爆炸，请将梯度阈值设置为 1。要使序列保持按长度排序，请将 'Shuffle' 设置为 'never'。

maxEpochs = 5000;
miniBatchSize = 50;

options = trainingOptions('adam', ...
    'MaxEpochs',maxEpochs, ...
    'MiniBatchSize',miniBatchSize, ...
    'InitialLearnRate',0.01, ...
    'GradientThreshold',1, ...
    'Shuffle','never', ...
    'Plots','training-progress',...
    'Verbose',0);
%使用 trainNetwork 训练网络。

net = trainNetwork(XTrain,YTrain,layers,options)



%%  prediction, needs to be further coded.
%
XTest = XTrain;
YTest = YTrain;
YPred = predict(net,XTest)

Damage_Test =  YTest{1}(:,end);
Damage_Pred =  YPred{1}(:,end);





damage_node = []

for i = 1:53690

    [col row] = find(connectivity==i);

    [m n] = size(col);

    dam = 0;

    for j = 1:m
        dam = dam + Damage_Pred(col(j),1);
    end

    dam = dam / m;


    damage_node(i,1) = i;
    damage_node(i,2) = dam;
    damage_node(i,3) = 0;
    damage_node(i,4) = 0;
end

%%

writematrix(damage_node,'disp.txt','Delimiter',',')
