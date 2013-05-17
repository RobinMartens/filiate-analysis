% clean workspace
clear;clc

% prepare
chapters = {'1', '5', '11', '16', '20', '25', '38', '42', '48', '56'};

% load data from file
data = cell(1, length(chapters));
for ii = 1:length(chapters)
    data{ii} = load(['work/' 'cc_' chapters{ii} '.csv']);
end

% get filenames
names = {'received laozi' 'xihan laozi' 'mawangdui-yi'};

% test for one
% build_dendrogram(data{1}, names, 1);

% test for average
total = 0
for ii = 1:length(data)
   total = total + data{ii}; 
end

build_dendrogram(total, names, 2);