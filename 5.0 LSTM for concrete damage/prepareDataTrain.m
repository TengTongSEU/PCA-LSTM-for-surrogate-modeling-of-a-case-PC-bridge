function [XTrain, YTrain, cases] = prepareDataTrain()
%%  -----------------------------------------------------------------
dataFolder = "damage";

%
filenamePredictors = fullfile(dataFolder,"Initial_data.txt");
parameter          = dlmread(filenamePredictors);

cases = unique( parameter(:,1) );

%%  -----------------------------------------------------------------
numObservations = size (cases,1);
%numObservations = 2;
%
XTrain = cell(numObservations,1);
YTrain = cell(numObservations,1);
%
ele_total = 35433;
%

for i = 1: numObservations
    %
    if(cases(i) <10)
        name = "damage" + string(0) + string(0) + string(cases(i)) + ".csv";
    elseif ( cases(i) >=10 )    &&  ( cases(i) <100 ) 
        name = "damage" +  string(0) + string(cases(i)) + ".csv";
    else
        name = "damage" +   string(cases(i)) + ".csv";
    end
    %
    AA = [];
    filenamePredictors = fullfile(dataFolder,name );
    AA                 = csvread(filenamePredictors);
    [row col] = size(AA);
    AA_temp = [];
    %
    for j = 1: row/ele_total
        AA_temp(j,1)                 = AA (  (j-1)*ele_total + 1);
        AA_temp(j, 2:(ele_total+1) ) = AA ( ((j-1)*ele_total + 1) : (j*ele_total) , 3);
    end
    
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

    i
end

