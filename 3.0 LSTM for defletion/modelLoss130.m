function [loss, gradients] = modelLoss(net, X, T)

    Y = forward(net, X);
    %loss = huber(Y, T);
    
    % loss = l1loss(Y, T);       % case 1 
    
    % loss = l2loss(Y, T);       % case 2 
    
    num = numel(Y);
    
    num_1 = floor(num*1/2);
    num_2 = num - num_1;
    
    wei_1 = 1.0;
    wei_2 = 30.0;               % 1:1; 1:30; 1:100
    
    loss_1 = 0 ;
    loss_2 = 0 ;
    loss   = 0 ;

    for i = 1: num_1
        loss_1 = loss_1 + ( T(1,i,1) - Y(1,i,1)) ^2;
    end
    
    for j = (num_1+1): num
        loss_2 = loss_2 + ( T(1,j,1) - Y(1,j,1)) ^2;
    end
    
    loss = (loss_1) * wei_1 /  num    +    (loss_2) * wei_2 /  num  ;
    
    
    %
    
    gradients = dlgradient(loss, net.Learnables);

end