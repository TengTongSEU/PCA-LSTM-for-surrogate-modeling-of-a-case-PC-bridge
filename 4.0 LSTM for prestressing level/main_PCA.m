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



%% Step 1: Data reading
%
% 0: read train data
%
[XTrain, YTrain, ZTrain, cases] = prepareDataAbs();
%
XTrain = XTrain(1:500);
YTrain = YTrain(1:500);
ZTrain = ZTrain(1:500);

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
            normalX(i,j) = centredX(i,j) / stdX(1,j);
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
    sum(W.*W, 1);                                % 可以验证每个特征向量各元素的平方和均为
    % ---------------------------------------------------------------------
    % 4: decompose the covariance matrix for eigenvalues and eigenvectors;  and sort the eigenvalues in descending order;
    %
    Wr = W(:, 1:1);                              % 提取前两个主成分的特征向量
    Tr = normalX * Wr;                          % 新坐标空间的数据点
    % ---------------------------------------------------------------------
    % 5: data reconstruction 
    %
    meanTrain{ii}    = meanX';
    stdTrain{ii}     = stdX';
    WrTrain{ii}      = Wr';
    TrTrain{ii}      = Tr';
    %
end
%
recoverTrain   = cell(numel(YTrain),1);
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
    Tr     =  TrTrain{ii}'
    Wr     =  WrTrain{ii}'
    stdX   =  stdTrain{ii}'
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
    recoverTrain{ii}      = recoverX';
end

%
figure
plot(YTrain{1}(:,1),'+')
hold on
plot(recoverTrain{1}(:,1),'o')
hold off

figure
plot(YTrain{1}(:,20),'+')
hold on
plot(recoverTrain{1}(:,20),'o')
hold off

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
% 2: get mean value and standard deviation of trian output data after PCA 
%
mu_tr  = mean([TrTrain{:}],2);
sig_tr = std ([TrTrain{:}],0,2);
%
for i = 1:numel(TrTrain)
    TrTrain{i} = (TrTrain{i} - mu_tr) ./ sig_tr;
end


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

net = trainNetwork(XTrain,TrTrain,layers,options)
%

