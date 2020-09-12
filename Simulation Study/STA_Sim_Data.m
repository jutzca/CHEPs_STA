%%% STA_Sim_Data
%%% Written by: Lukas Linde
%%% September 4, 2020
%%% Purpose: The following code was written to create a series of datasets
%%% for a comparison between conventional (across-trial) averaging and
%%% single trial analysis methods of processing contact heat evoked
%%% potentials. A single participants data was used (15 trials,
%%% pre-processed Cz data). Each data set include a different number of
%%% "large amplitude" trials and/or latency shifts of those amplitude
%%% trials. smaller amplitude trials were randomly sampled from remaining
%%% trials. To ensure similar trials were used in subsequent datasets,
%%% random resamples was completed in a stepwise progression, such that
%%% small amplitude trials were similar amoung datasets. 


%%% STEP 1- Import data
% Import data 
A=dlmread('RD_Baseline12.txt');
% Create cell array to store outcomes. 
DAT=cell(1,8);

%%% STEP 2 - creating datasets
% DATA 1 - Baseline Trial (2 large trials)
% Ensure only 15 trials. 
A1=A(:,1:15);
    clear A
dat1=A1;
DAT{1,1}=dat1; % store dataset

% DATA 2 - Double Largest Trials (4 trials)
A2=A1(:,1:2);
A2b=horzcat(A2,A2); % double the large trials
A2c=A1(:,3:end); % array of remaining 'small' trials
A2d=randsample(13,11); % randomly generate 11 numbers (1:13 possible)
A2e=reshape(A2d,1,[]); % Reshape random numbers. 

A2r=A2c(:,A2e); % select random small amplitude trials

dat2=horzcat(A2b,A2r); % combine large and small trials
    clear A2 A2b A2c A2d A2e
DAT{1,2}=dat2; % store dataset
    
% DATA 3 - Triple largest trials (6 trials)
A3=A1(:,1:2);
A3b=horzcat(A3,A3,A3); % six large trials
A3d=randsample(11,9);
A3e=reshape(A3d,1,[]);

A3r=A2r(:,A3e); % Random data is sampled from previous dataset array

dat3=horzcat(A3b,A3r);
    clear A3 A3b A3d A3e A2r
DAT{1,3}=dat3;

% DATA 4 - 4X largest trials (8 trials)
A4=A1(:,1:2);
A4b=horzcat(A4,A4,A4,A4); % eight large trials
A4d=randsample(9,7);
A4e=reshape(A4d,1,[]);

A4r=A3r(:,A4e);

dat4=horzcat(A4b,A4r);
    clear A4 A4b A4d A4e A3r 
DAT{1,4}=dat4;

% DATA 5 - 5X largest trials (10 trials)
A5=A1(:,1:2);
A5b=horzcat(A5,A5,A5,A5,A5); % ten large trials
A5d=randsample(7,5);
A5e=reshape(A5d,1,[]);

A5r=A4r(:,A5e);

dat5=horzcat(A5b,A5r);
    clear A5 A5b A5d A5e A4r A5r
DAT{1,5}=dat5;
   
    
% DATA 6 - Amplitude 3X Latency 10ms 
L10a=dat3(:,1:2); % select first two large amplitude trials
L10b=L10a(21:end,:); % select area of interest
L10c=L10a(1:20,:); % select first 10ms (fs=2000)
L10=vertcat(L10b,L10c); % move first 10ms to end of trial

dat6=horzcat(L10,dat3(:,3:end)); % place 10ms reduced trials into dataset
    clear L10a L10b L10c L10
DAT{1,6}=dat6; % save
    
% DATA 7 - Amplitude 3X Latency 20ms 
L20a=dat3(:,1:2);
L20b=L20a(41:end,:); 
L20c=L20a(1:40,:); % 20 ms
L20=vertcat(L20b,L20c);

dat7=horzcat(L20,dat3(:,3:end));
    clear L20a L20b L20c L20
DAT{1,10}=dat7;
    
% DATA 8 - Amplitude 3X Latency 30ms 
L30a=dat3(:,1:2);
L30b=L30a(61:end,:);
L30c=L30a(1:60,:); % 60ms
L30=vertcat(L30b,L30c);

dat8=horzcat(L30,dat3(:,3:end));
    clear L30a L30b L30c L30
DAT{1,11}=dat8;
    
    clear dat1 dat2 dat3 dat4 dat5 dat6 dat7 dat8

%%% STEP 3 - Save data, and resort to eeglab / STEP1


for k=1:8
    Temp=DAT{1,k};
    Shape=reshape(Temp,[],1);
    Amp=Shape*1000000; % amplify signals to convert to uV
    save(sprintf('dat%02d.mat',k),'Amp');
        clear Temp Shape Amp
end
    clear DAT k 
