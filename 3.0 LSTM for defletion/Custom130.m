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
dataFolder = "data";
%
filenamePredictors = fullfile(dataFolder,"train_600.txt")
%
[XTrain,YTrain,cases]    = prepareDataTrain(filenamePredictors);
%
%


%XTrain = data(:,2:7)'
%YTrain = data(:,8)'

%%
%将训练预测变量归一化为具有零均值和单位方差。要计算所有观测值的均值和标准差，请水平串联序列数据。

mu = mean([XTrain{:}],2);
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
    fullyConnectedLayer(numResponses) ];

net = dlnetwork(layers)

% run in parallel
%
%
%% mini-batch calculation
%
numEpochs             =   5000;
miniBatchSize         =   50;
numObservations       =   numel(XTrain);    % 744
numIterationsPerEpoch =   ceil(numObservations./miniBatchSize);

averageGrad   = [];
averageSqGrad = [];

numIterations = numEpochs * numIterationsPerEpoch;

iteration = 0;
epoch     = 0;

learnRate = 0.01;
gradDecay = 0.75;
sqGradDecay = 0.95;

monitor = trainingProgressMonitor(Metrics="Loss", Info=["Epoch","LearnRate"], XLabel="Iteration");

%
while epoch < numEpochs && ~monitor.Stop
    epoch = epoch + 1;

    %idx    = randperm(  size(XTrain,2)  );
    %XTrain = XTrain(:,idx);
    %YTrain = YTrain(idx);

    i = 0;
    while i < numIterationsPerEpoch && ~monitor.Stop
        %
        i = i + 1; 
        iteration = iteration + 1;

        idx = (i - 1) * miniBatchSize+1 : i * miniBatchSize;

        %loss = 0;

        %gradients = 0;

        for ii= idx(1):idx(end)
           %for jj = 1:10 

            X = XTrain(ii);
            T = YTrain(ii);

            X = cell2mat(X);
            T = cell2mat(T);

            X = dlarray(X, 'CBT');          % C-Channel, B-Batch, T-time
            T = dlarray(T, 'CBT');          % C-Channel, B-Batch, T-time
       
            [loss, gradients] = dlfeval(@modelLoss130, net, X, T);

            
            [net, averageGrad, averageSqGrad] = adamupdate(net, gradients, averageGrad, averageSqGrad, iteration, learnRate,gradDecay,sqGradDecay);
           %end


        end


        
        recordMetrics(monitor, iteration, Loss=loss);
        updateInfo(monitor,Epoch=epoch + " of " + numEpochs);
        monitor.Progress = 100 * iteration / numIterations;

    end
end


%% prediction
%测试网络

%LSTM 网络对不完整序列进行预测，一次预测一个时间步。在每个时间步，网络使用此时间步的值进行预测，网络状态仅根据先前的时间步进行计算。网络在各次预测之间更新其状态。predict 函数返回这些预测值的序列。预测值的最后一个元素对应于不完整序列的预测 RUL。

%您也可以使用 predictAndUpdateState 一次对一个时间步进行预测。这在时间步的值以流的方式到达时非常有用。通常，对完整序列进行预测比一次对一个时间步进行预测更快。有关如何通过在相邻的单个时间步预测之间更新网络来预测将来时间步的示例，请参阅使用深度学习进行时序预测。

%在绘图中可视化一些预测值。

%incr = 2000;

%for jj = 1:20

%idx = [1+incr:100+incr];
%figure
%for i = 1:numel(idx)
%    subplot(10,10,i)
    
%    time = XTrain{i}(6,:) * sig(6) + mu(6);
     
%    plot(time,YTrain{i},'--')
%    hold on
    
%    hold off
    
%    title("Test Observation " + idx(i))
%    xlabel("Time Step")
%    ylabel("deflection")
%end
%legend(["Test Data" "Predicted"],'Location','northeast')

%incr = incr +100

%end



idx = [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16];

for i = 1:numel(idx)
    subplot(4,4,i)
    
    time = XTrain{i}(6,:) * sig(6) + mu(6)
     
    plot(time,YTrain{i},'--')
    hold on
    
    XX = cell2mat(XTrain(i));
    XX = dlarray(XX , 'CBT');
    YPred = predict(net, XX);

    plot(time,YPred(1,:,1),'.-')
    hold off
    
    
    title("Test Observation " + idx(i))
    xlabel("Time Step")
    ylabel("deflection")
end
legend(["Test Data" "Predicted"],'Location','northeast')