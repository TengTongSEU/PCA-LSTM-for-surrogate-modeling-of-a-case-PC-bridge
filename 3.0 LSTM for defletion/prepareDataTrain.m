function [XTrain,YTrain, cases] = prepareDataTrain(filenamePredictors)

dataTrain = dlmread(filenamePredictors);

%
cases = unique(dataTrain(:,1));

numObservations = size (cases,1);
%


XTrain = cell(numObservations,1);
YTrain = cell(numObservations,1);

%

for i = 1:numObservations
    idx = dataTrain(:,1) == cases(i);
    
    X = dataTrain( idx,2:(end-1) )';
    
    [m n] = size(X);
   
    XTrain{i} = X(1:m,1:ceil(3*n/3));




    
    deflection = dataTrain(idx,end)';

    Y = deflection;

    [m n] = size(Y);

    YTrain{i} = Y(1:m,1:ceil(3*n/3));
end

end