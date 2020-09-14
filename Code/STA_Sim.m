%%% STA_Sim
%%% Written by: Lukas Linde, PhD
%%% Written - September 10, 2020
%%% Last edited - Ssptember 13, 2020
%%% Purpose: The following code was written to create a series of datasets
%%% for a comparison between conventional (across-trial) averaging and
%%% single trial analysis methods of processing contact heat evoked
%%% potentials.  


%%% STEP 1- Import data
% Import data 
A=dlmread('RD_Baseline60.txt');
% Create cell array to store datasets
DAT=cell(1,16);

% STEP 2
% Divide large and small trials
L=A(:,1); % Large Trial
S=A(:,2:16); % Small Trials

DAT{1,1}=S; % Store 15 small trials as first dataset

Base=horzcat(L,S); % store large and small trials together 
    clear A L S


% 20 ms
L20a=Base(:,1); % select large trials
L20b=L20a(41:end,:); % select Lall data but first 20ms
L20c=L20a(1:40,:); % select first 20ms (fs=2000)
L20=vertcat(L20b,L20c); % move first 20ms to end of trial

D20=horzcat(L20,Base(:,2:15)); % places 20ms reduced large amplitude trial into dataset with 15 small trials
DAT{1,2}=D20; % store as second dataset
    clear L20a L20b L20c L20 D20 Base


    
% Loop through progressively adding more large amplitude trials
    for k=2:15
        Temp=DAT{1,2}; % select dataset with 1 large amp trial
        Large=Temp(:,1); % select large trial
        Rep=repmat(Large,[1,k]); % duplicate based on loop interation
        Small=Temp(:,2:15); % select small trials
        Sample=randsample(15-k,15-k); % select small trials for dataset
        Sample=reshape(Sample,1,[]); % reshape
        if k==2 % was originally for different randomization datasets 
            Rand=Small(:,Sample); % select small trials
        else
            Rand=Rand(:,Sample); % select small trials
        end
        
        Trial=horzcat(Rep,Rand); % combined repeated large and random small trials
        
        DAT{1,k+1}=Trial; % store dataset
        clear Temp Large Rep Small Sample Trial
        if k==15
            clear Rand
        else
        end
    end
    
    clear k

%%% Reorganze and Export data to process in EEGlab and STEP1
for k=1:16
    Temp=DAT{1,k};
    Shape=reshape(Temp,[],1); % reshape as one column to read into EEglab
    Trial=Shape*1000000; % amplify signals to convert to uV
    save(sprintf('20ms_%02d.mat',k-1),'Trial');
        clear Temp Shape Trial
end
    
clear k


% Export for 3D figures
% export all avgs for plotting
AVG=zeros(2200,16);

for k=1:16
    Temp=DAT{1,k};
    Trial=Temp*1000000;
    Avg=mean(Trial,2);
    AVG(:,k)=Avg;
    data=horzcat(Trial,Avg);
    save(sprintf('plot_%02d.mat',k-1),'data');
        clear Temp Trial Avg data
    
    
end
    clear k 
    save('plot_AVG.mat','AVG');
    clear AVG
    
    
    
