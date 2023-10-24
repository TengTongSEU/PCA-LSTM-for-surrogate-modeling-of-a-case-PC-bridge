%
% https://www.zhihu.com/question/415332414/answer/2884903764?utm_id=0
%

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

[XTrain, YTrain, ZTrain, cases] = prepareDataAbs();
%
XTrain = XTrain(1:500);
YTrain = YTrain(1:500);
%

%%
%将训练预测变量归一化为具有零均值和单位方差。要计算所有观测值的均值和标准差，请水平串联序列数据。

mu  = mean([XTrain{:}],2);
sig = std([XTrain{:}],0,2);

for i = 1:numel(XTrain)
    XTrain{i} = (XTrain{i} - mu) ./ sig;
end

mu_y = mean([YTrain{:}],2);
sig_y = std([YTrain{:}],0,2);

for i = 1:numel(YTrain)
    YTrain{i} = (YTrain{i} - mu_y) ./ sig_y;
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
miniBatchSize = 100;

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



