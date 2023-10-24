
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
%
% 1: rearrange of the data
%
for i=1:numel(XTrain)
    sequence = XTrain{i};
    sequenceLengths(i) = size(sequence,2);
end

[sequenceLengths,idx] = sort(sequenceLengths,'descend');
XTrain  = XTrain(idx);
YTrain  = YTrain(idx);
%在条形图中查看排序的序列长度。

figure
bar(sequenceLengths)
xlabel("Sequence")
ylabel("Length")
title("Sorted Data")
%
%% Step 2: PCA pre-processing of raw data
%
%   Variables need to be store as cell
%       meanX, stdX, Wr, Tr, recoverX
%
meanTrain      = cell(numel(YTrain),1);
stdTrain       = cell(numel(YTrain),1);
WrTrain        = cell(numel(YTrain),1);
TrTrain        = cell(numel(YTrain),1);

%
%
for ii = 1: numel(YTrain)
    % 0: parameter initialiation ---------------------------------------------------------------------
    %
    X = 0;
    meanX = 0;
    centredX = 0;
    stdX = 0;
    normalX = 0;
    C = 0;
    W = 0;
    Lambda = 0;
    ev = 0;
    W = 0;
    Wr = 0;
    Tr = 0;
    recoverX_temp = 0;
    recoverX = 0 ;

    % ---------------------------------------------------------------------
    % 1: standardize the input data;
    %
    X = YTrain{ii};
    X = X';
    meanX = ones(size(X,1), 1) * mean(X);
    %
    centredX  = X - meanX;                       
    stdX      = std(X);
    %
    [m n]     = size(X);
    %
    for i = 1: m
        for j = 1: n
            if (stdX(1,j) < 1.0e-10)
                normalX(i,j) = 0.0;
            else
                normalX(i,j) = centredX(i,j) / stdX(1,j);
            end
        end
    end
    % ---------------------------------------------------------------------
    % 2: compute the covariance matrix;
    %
    C = cov(normalX) ;	                         % 相关性系数
    % ---------------------------------------------------------------------
    % 3: decompose the covariance matrix for eigenvalues and eigenvectors;  and sort the eigenvalues in descending order;
    %
    [W, Lambda] = eig(C);                        % W是特征向量组成的矩阵（m×m），Lambda是特征值组成的对角矩阵
    %
    ev = (diag(Lambda))';		                 % 提取特征值
    ev = ev(:, end:-1:1);		                 % eig计算出的特征值是升序的，这里手动倒序（W同理），需要人工确认是否成功排序

    W = W(:, end:-1:1);
    %sum(W.*W, 1);                                % 可以验证每个特征向量各元素的平方和均为
    % ---------------------------------------------------------------------
    % 4: decompose the covariance matrix for eigenvalues and eigenvectors;  and sort the eigenvalues in descending order;
    %
    Wr = W(:, 1:3);                              % 提取前两个主成分的特征向量
    Tr = normalX * Wr;                          % 新坐标空间的数据点
    % ---------------------------------------------------------------------
    % 5: data reconstruction 
    %
    meanTrain{ii}    = meanX';
    stdTrain{ii}     = stdX';
    WrTrain{ii}      = Wr';
    TrTrain{ii}      = Tr';
    %
    ii
end
%
recoverTrain   = cell(numel(YTrain),1);

%% Step 3: 将训练预测变量归一化为具有零均值和单位方差。要计算所有观测值的均值和标准差，请水平串联序列数据。
%
% 0: get mean value and standard deviation of trian input data
%
mu  = mean([XTrain{:}],2);
sig = std ([XTrain{:}],0,2);
%
% 1: normalization of trian input data
%
for i = 1:numel(XTrain)
    XTrain{i} = (XTrain{i} - mu) ./ sig;
end
%


%% Step 4: network construction
%
numResponses = size(TrTrain{1},1);
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

maxEpochs = 500;
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

net = trainNetwork(XTrain,TrTrain,layers,options)

%
%% Step 5: post processing 
%
XTest   = XTrain;
TrTest  = TrTrain;
TrPred  = predict(net,XTest);


recoverTrainPre   = cell(numel(YTrain),1);
%
for ii = 1: numel(YTrain)
    % --------------------------------------------------------------------- 
    Tr = 0;
    Wr = 0;
    stdX = 0;
    meanX = 0;
    X = 0;
    recoverX_temp = 0;
    recoverX = 0;
    % ---------------------------------------------------------------------
    Tr     =  TrPred{ii}';
    Wr     =  WrTrain{ii}';
    stdX   =  stdTrain{ii}';
    meanX  =  meanTrain{ii}';
    %
    X = YTrain{ii};
    X = X';
    [m n] = size(X);
    %
    recoverX_temp = Tr * Wr';
    %
    for i = 1: m
        for j = 1: n
            recoverX(i,j) = recoverX_temp(i,j) *  stdX(1,j);
        end
    end
    %
    recoverX = recoverX + meanX;    
    %
    recoverTrainPre{ii}      = recoverX';
end


%%  prediction, needs to be further coded.
%
XTest = XTrain;
YTest = YTrain;
YPred = predict(net,XTest);

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
