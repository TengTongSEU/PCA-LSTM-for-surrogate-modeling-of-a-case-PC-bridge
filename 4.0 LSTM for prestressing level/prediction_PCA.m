%% Step 5: post processing 
%
XTest   = XTrain;
TrTest  = TrTrain;
TrPred  = predict(net,XTest);
%
for i = 1: numel(TrPred)
    AA = 0;
    AA = TrPred{i};
    [row col] = size (AA);
    for m = 1:row
        for n = 1:col
            AA(m,n) = AA(m,n) * sig_tr(m,1) + mu_tr(m,1);
        end
    end

    TrPred{i} = AA;
end


%

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





%
figure
plot(recoverTrainPre{1}(:,1),'+')
hold on
plot(recoverTrain{1}(:,1),'o')
hold off

figure
plot(recoverTrainPre{1}(:,20),'+')
hold on
plot(recoverTrain{1}(:,20),'o')
hold off



idx = [1 2 3 4 5 6 7 8 9 10];
figure
for i = 1:numel(idx)
    subplot(3,3,i)
    %
    time = XTest{i}(6,:) * sig(6) + mu(6)
    %
   
    plot(time,YTrain{idx(i)},'--')
    hold on
    plot(time,recoverTrainPre{idx(i)},'.-')
    hold off
    
    
    title("Test Observation " + idx(i))
    xlabel("Time Step")
    ylabel("Prestressing level")
end
legend(["Test Data" "Predicted"],'Location','northeast')