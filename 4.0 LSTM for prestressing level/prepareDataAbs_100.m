%
function [XTrain, YTrain, ZTrain, cases] = prepareDataAbs()


dataFolder = "Preloss_100";

%
filenamePredictors = fullfile(dataFolder,"test_10_100.txt");
parameter          = dlmread(filenamePredictors);

%
cases = unique( parameter(:,1) );

%
numObservations = size (cases,1);
%
XTrain = cell(numObservations,1);
YTrain = cell(numObservations,1);
%
ZTrain = cell(numObservations,1);
%
for i = 1: numObservations
    %
    if(cases(i) <10)
        name = "Pres_loss" + string(0) + string(0) + string(cases(i)) + ".csv";
    elseif ( cases(i) >=10 )    &&  ( cases(i) <100 ) 
        name = "Pres_loss" +  string(0) + string(cases(i)) + ".csv";
    else
        name = "Pres_loss" +   string(cases(i)) + ".csv";
    end
    %
    AA = [];
    filenamePredictors = fullfile(dataFolder,name );
    AA                 = csvread(filenamePredictors);
    [row col] = size(AA);
    AA_temp = AA;
    %
    % predict prestressing loss
    %
    %for ii = 1:row
    %    for jj = 1: col
    %        AA_temp(ii,jj) = AA_temp(ii,jj) - AA(1,jj); 
    %    end
    %end
    %
    ZTrain{i} = AA(1,:) ;              %store for the initial prestressing
    %
    AA = AA_temp;
    %
    res     = find(parameter(:,1) == cases(i));
    col     = size(AA);
    %
    BB = [];
    BB(1:col(1),1) = parameter(res(1),2);
    BB(1:col(1),2) = parameter(res(1),3);
    BB(1:col(1),3) = parameter(res(1),4);
    BB(1:col(1),4) = parameter(res(1),5);
    BB(1:col(1),5) = parameter(res(1),6);
    %
    CC = [];
    CC = horzcat(BB,AA);
    CC = CC';
    %
    XTrain{i} = CC(1:6,:);
    YTrain{i} = CC(7:end,:);
end



%plot(PreLoss{1}(:,1),PreLoss{1}(:,2))

%for i = 1: numObservations
%    plot(PreLoss{i}(end,2:end))
%    hold on
%end



