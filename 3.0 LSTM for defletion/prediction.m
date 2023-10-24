%
% https://www.zhihu.com/question/415332414/answer/2884903764?utm_id=0
%



%%
dataFolder = "data";

filenamePredictors = fullfile(dataFolder,"test_144.txt")
%filenamePredictors = fullfile(dataFolder,"test_10_100.txt")

[XTest,YTest,cases]    = prepareDataTrain(filenamePredictors);


%mu = mean([XTest{:}],2);
%sig = std([XTest{:}],0,2);

for i = 1:numel(XTest)
    XTest{i} = (XTest{i} - mu) ./ sig;
end

%XTrain = data(:,2:7)'
%YTrain = data(:,8)'


%% prediction
%
YPred = cell(numObservations,1);
%
for i = 1:numel(XTest)    
    XX = cell2mat(XTest(i));
    XX = dlarray(XX , 'CBT');
    YPred{i} = predict(net, XX);
end

%
%

for i = 1:numel(XTest)
    subplot(12,12,i)
    %
    time = XTest{i}(6,:) * sig(6) + mu(6)
    % 
    plot(time,YTest{i},'--') 
    hold on
    
    plot(time,YPred{i},'.-')
    hold off
    
    title("Test Observation " + i)
    xlabel("Time Step")
    ylabel("deflection")
end

 AA = YTest{70}'
 BB = time'
 for i = 1: 175
    CC(i,1) = YPred{70}(1,i,1)
 end
%legend(["Test Data" "Predicted"],'Location','northeast')

% ---------------------------------- %
%
MAE  = 0;
RMSE = 0;

%
YTest_mat(numel(XTest),1) = 0;
YPred_mat(numel(XTest),1) = 0;
%
for i = 1:numel(XTest)
    plot(YTest{i}(end),YPred{i}(end),'o')
    hold on
    
    YTest_mat(i,1) = YTest{i}(end);
    YPred_mat(i,1) = YPred{i}(end);
    %
    MAE = MAE + abs( YTest{i}(end) - YPred{i}(end) );
    RMSE = RMSE + (YTest{i}(end) - YPred{i}(end) )^2;
end
xlim([-700 0])
ylim([-700 0])

MAE  = MAE / numel(XTest)
RMSE = sqrt( RMSE / numel(XTest))

R    = 0;
R_1    = 0;
R_2    = 0;
R_3    = 0;
YTest_mat_mean = mean(YTest_mat);
YPred_mat_mean = mean(YPred_mat);
%
for i = 1:numel(XTest)
    R_1 = R_1 + (YTest_mat(i,1) - YTest_mat_mean)*(YPred_mat(i,1) - YPred_mat_mean);
    R_2 = R_2 + (YTest_mat(i,1) - YTest_mat_mean)^2;
    R_3 = R_3 + (YPred_mat(i,1) - YPred_mat_mean)^2;
end
R = R_1 / (sqrt(R_2) * sqrt(R_3))


% ---------------------------------- %
resi_err = 0;
rela_err = 0;
%
for i = 1:numel(XTest)
    resi_err(i,1) = YTest_mat(i) - YPred_mat(i) ;
    rela_err(i,1) = abs( (YTest_mat(i) - YPred_mat(i)) / YTest_mat(i) );
end

histogram( resi_err )
histogram( rela_err )