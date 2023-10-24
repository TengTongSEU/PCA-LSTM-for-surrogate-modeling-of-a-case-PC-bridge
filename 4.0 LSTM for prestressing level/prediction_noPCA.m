%%
XTest = XTrain;
YTest = YTrain;
YPred = predict(net,XTest)

% recover the prestressing force
% --------------------------------
for i = 1: numel(YTest)
    AA = 0;
    AA = YTest{i};
    [row col] = size (AA);
    for m = 1:row
        for n = 1:col
            AA(m,n) = AA(m,n) * sig_y(m,1) + mu_y(m,1);
        end
    end

    YTest{i} = AA;
end
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
% plot the figure
%
idx = [1 2 3 4 5 6 7 8 9 10];
figure
for i = 1:numel(idx)
    subplot(3,3,i)
    %
    time = XTest{i}(6,:) * sig(6) + mu(6)
    %
   
    plot(time,YTest{idx(i)},'--')
    hold on
    plot(time,YPred{idx(i)},'.-')
    hold off
    
    
    title("Test Observation " + idx(i))
    xlabel("Time Step")
    ylabel("Prestressing level")
end
legend(["Test Data" "Predicted"],'Location','northeast')