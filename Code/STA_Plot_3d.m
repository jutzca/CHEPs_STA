%%% STA_Plot_3d
%%% Written by: Lukas Linde, PhD
%%% Written - September 12, 2020
%%% Last edited - Ssptember 13, 2020
%%% Purpose: The following code was written to plot STA simulation study
%%% outcomes in 3d plots. 3 datasets were chosed from "STA_Sim"
%%% and Time3D and Trial3D datasets are also needed. 


load('Time3D.mat')
load('Trial3D.mat')



% plot 1 (Figure 7D) - 5:10
load('plot_05.mat') % from STA_Sim
data5_10=data;
    clear data


figure1 = figure;
% Create axes
axes1 = axes('Parent',figure1);
hold(axes1,'on');
plot31 = plot3(Trial3D,Time3D,data5_10,'Parent',axes1,...
    'Color',[0.501960784313725 0.501960784313725 0.501960784313725]);
set(plot31(16),'LineWidth',2,...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(1),...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(2),...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(3),...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(4),...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(5),...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(gca,'Zdir','reverse');
set(gca,'fontsize',14);

view(axes1,[96 7]);
box(axes1,'on');
% Add title and axis labels
% xlabel('Trial')
ylabel('Time (ms)')
zlabel('uV')
title('5:10 Simulated Data')



%%% plot 2 (Figure 7E) - 10:5 Simulation 

load('plot_10.mat')% from STA_Sim
data10_5=data;
    clear data


figure2 = figure;
% Create axes
axes2 = axes('Parent',figure2);
hold(axes2,'on');
plot31 = plot3(Trial3D,Time3D,data10_5,'Parent',axes2,...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(16),'LineWidth',2,...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(11),...
    'Color',[0.501960784313725 0.501960784313725 0.501960784313725]);
set(plot31(12),...
    'Color',[0.501960784313725 0.501960784313725 0.501960784313725]);
set(plot31(13),...
    'Color',[0.501960784313725 0.501960784313725 0.501960784313725]);
set(plot31(14),...
    'Color',[0.501960784313725 0.501960784313725 0.501960784313725]);
set(plot31(15),...
    'Color',[0.501960784313725 0.501960784313725 0.501960784313725]);
set(gca,'Zdir','reverse');
set(gca,'fontsize',14);

view(axes2,[96 7]);
box(axes2,'on');
% Add title and axis labels
% xlabel('Trial')
ylabel('Time (ms)')
zlabel('uV')
title('10:5 Simulated Data')






%%% plot 3 (Figure 7F) Averages

load('plot_AVG.mat')% from STA_Sim


figure3 = figure;
% Create axes
axes3 = axes('Parent',figure3);
hold(axes3,'on');
plot31 = plot3(Trial3D,Time3D,AVG,'Parent',axes3,...
    'Color',[0.501960784313725 0.501960784313725 0.501960784313725]);
set(plot31(16),'LineWidth',2,...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);
set(plot31(1),'LineWidth',2,...
    'Color',[0.149019607843137 0.149019607843137 0.149019607843137]);

set(gca,'Zdir','reverse');
set(gca,'fontsize',14);

view(axes3,[96 7]);
box(axes3,'on');
% Add title and axis labels
% xlabel('Sim')
ylabel('Time (ms)')
zlabel('uV')
title('Simulated Across-Trial Averages')
