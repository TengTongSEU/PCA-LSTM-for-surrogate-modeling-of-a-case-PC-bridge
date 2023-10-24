
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
dataFolder = "damage";

filenamePredictors = fullfile(dataFolder,"damage001.csv")

dataTrain = csvread(filenamePredictors);



X = prepareDataTrain(dataTrain);                                 % n 行, m 列




%% 1 standardize the input data;
meanX = ones(size(X,1), 1) * mean(X);        % 中心化处理

centredX = X - meanX;                       

stdX = std(X);

[m n] = size(X);

for i = 1: m
    for j = 1: n
        normalX(i,j) = centredX(i,j) / stdX(1,j);
    end
end

%% 2 compute the covariance matrix;

C = cov(normalX) ;	                         % 相关性系数

%% 3 decompose the covariance matrix for eigenvalues and eigenvectors;  and sort the eigenvalues in descending order

[W, Lambda] = eig(C);                        % W是特征向量组成的矩阵（m×m），Lambda是特征值组成的对角矩阵

ev = (diag(Lambda))';		                 % 提取特征值
ev = ev(:, end:-1:1);		                 % eig计算出的特征值是升序的，这里手动倒序（W同理），需要人工确认是否成功排序

W = W(:, end:-1:1);
sum(W.*W, 1)                                 % 可以验证每个特征向量各元素的平方和均为

figure;
    stairs(cumsum(ev)/sum(ev), 'LineWidth',1.5);
    axis([1 4 0 1]);
    xlabel('$ k $', 'Interpreter', 'latex');
    ylabel('$ f(k)=\frac{\sum _{i=1}^i \lambda_k}{\sum_{i=1}^m \lambda_i} $',...
        'Interpreter', 'latex');
    hold on;
    plot([1 4], [0.90 0.90], '--');         % 从图中可以看出，r为方差贡献率，取r = 2

%% 4 PCA analysis                         
%
Wr = W(:, 1:3)                              % 提取前两个主成分的特征向量
Tr = normalX * Wr;                          %  新坐标空间的数据点

%% 5 data reconstruction 
%
recoverX_temp = Tr * Wr';

for i = 1: m
    for j = 1: n
        recoverX(i,j) = recoverX_temp(i,j) *  stdX(1,j);
    end
end

recoverX = recoverX + meanX;

recoverX - X;

%% 6 post processing  
%



figure
plot(X(20,:),'+')
hold on
plot(recoverX(20,:),'o')


figure
plot(X(30,:),'+')
hold on
plot(recoverX(30,:),'o')

