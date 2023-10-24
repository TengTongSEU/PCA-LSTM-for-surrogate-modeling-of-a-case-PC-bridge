%% Step 1: Data reading
%
% 0: read train data
%
[XTest, YTest, ZTest, cases] = prepareDataAbs();
%
XTest = XTest(501:end);
YTest = YTest(501:end);
ZTest = ZTest(501:end);


%% Normalization
%
for i = 1:numel(XTest)
    XTest{i} = (XTest{i} - mu) ./ sig;
end


%% prediction
%
YPred = predict(net,XTest);
%
for i = 1: numel(YPred)
    AA = 0;
    AA = YPred{i};
    [row col] = size (AA);
    for m = 1:row
        for n = 1:col
            AA(m,n) = AA(m,n) * sig_y(m,1) + mu_y(m,1);
        end
    end

    YPred{i} = AA;
end

%
%

for i = 1:numel(XTest)
    subplot(3,3,i)
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
legend(["Test Data" "Predicted"],'Location','northeast')


%%
MAE  = 0;
RMSE = 0;

%
YTest_mat(numel(YTest)*32,1) = 0;
YPred_mat(numel(XTest)*32,1) = 0;
%
for i = 1:numel(YTest)
    plot(YTest{i}(1:32,end),YPred{i}(1:32,end),'o')
    hold on
    
    YTest_mat( ((i-1)*32+1):((i-1)*32+32),1) = YTest{i}(1:32,end);
    YPred_mat( ((i-1)*32+1):((i-1)*32+32),1) = YPred{i}(1:32,end);   
end
%
for i = 1:numel(YTest)*32
    MAE = MAE + abs( YTest_mat(i,1) - YPred_mat(i,1) );
    RMSE = RMSE + (YTest_mat(i,1) - YPred_mat(i,1) )^2;
end

xlim([-700 0])
ylim([-700 0])

MAE  = MAE / numel(XTest)/32
RMSE = sqrt( RMSE / numel(XTest) /32)

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