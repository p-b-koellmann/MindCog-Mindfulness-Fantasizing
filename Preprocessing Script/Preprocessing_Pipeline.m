%Preprocessing Script for the EEG Data up to exporting the ERPs

% This file combines several preprocessing scripts into one document. During preprocessing, these steps were run as separate scripts,
% which explains the repeated redefinition of root folders and variables.
%
% The script documents the preprocessing workflow used for the ERP analyses.
% Local paths need to be adapted before running. Raw participant-level EEG data
% are not publicly shared at this stage.

%this script requires the EEGLAB extension for MATLAB, with ERPLAB and Biosig plugins
%before running: use home -> set path to add EEGLAB folder to the MATLAB path, and type eeglab in the command window to start up EEGLAB

%SCRIPT A - removing channels, filtering, addidng eventlists

eeglab 

clear all;
close all;
rootfolder = 'YOUR_ROOTFOLDER'; %the folder in which the data, processing, and codefolders are situated
datafolder = 'ERT EEG\'; %the folder name of the subfolder containing the experimental data
savefolder = 'Preprocessed Data\Script A\'; %the folder in which you want to save your filtered data
codefolder = 'Codes\'; %the folder where your equationlists and binlisters are situated

files = dir([rootfolder datafolder '*.bdf']); %this finds all the .bdf datasets in your datafolder
v = 1:length(files); %an array with the length and/or location of the EEG datafiles in your datafolder that you want to have processed (first file in matlabs files list:last file)

v_files = files(v);
for v = 1:length(v_files) %this forms a loop for doing the filtering etc. on multiple datasets, on the datasets specified before in v
    [a,shortName,c] = fileparts(v_files(v).name);
    outputFile = fullfile(rootfolder, savefolder, [shortName '.set']);

    if exist(outputFile, 'file')
        fprintf('Skipping already processed file: %s\n', shortName);
        continue;
    end

    EEG = pop_biosig([rootfolder datafolder v_files(v).name],'channels',[1:38]);
    EEG = pop_select(EEG, 'nochannel', 39:EEG.nbchan); %some files had 45 channels, this gets rid of the extra channels

    EEG = pop_chanedit(EEG, 'load', 'YOUR_ROOTFOLDER\BioSemi32+6chans.loc');

    EEG = pop_eegfiltnew(EEG, 0.1,[],67584,0,[],1);  %A high-pass filter with cutoff at 0.1 Hz (roll-off 67584 points). Can take some time (5-10 minutes)
    EEG = pop_eegfiltnew(EEG, [],45,12,0,[],1); %A low-pass filter, with cutoff at 45 Hz (roll-off of 8 points) 

    EEG  = pop_editeventlist( EEG , 'AlphanumericCleaning', 'on', 'ExportEL', [rootfolder codefolder 'eventlist_binedMDD.txt'], 'List', [rootfolder codefolder 'equationList.txt'],...
    'SendEL2', 'EEG&Text', 'UpdateEEG', 'codelabel' );  
    EEG  = pop_binlister( EEG , 'BDF', [rootfolder codefolder 'binlisterMDD.txt'], 'ExportEL', [rootfolder codefolder 'eventlist_binedMDD_Marlijn.txt'], 'IndexEL',  1, 'SendEL2', 'All', 'UpdateEEG', 'on', 'Voutput', 'EEG' );
    %exports/creates a txt file with the event lists - requires the documents for the equationlist and binlister in the correct folders to work


    EEG = pop_saveset( EEG, 'filename', [shortName '.set'], 'filepath',[rootfolder savefolder]); %saves the full filtered and eventlisted EEG data
end

%%

%Script A2 channel interpolation

eeglab;

clear all;
close all;
rootfolder = 'YOUR_ROOTFOLDER';  % Folder containing data, processing, and code folders
loadfolder = 'Preprocessed Data\Script A\';                     % Folder to load filtered and event-listed data
savefolder = 'Preprocessed Data\Script A2\';                     % Folder to save the processed data
codefolder = 'Codes\';                                           % Folder for your equation lists and binlisters

files = dir([rootfolder loadfolder '*.set']);                    % Find all .set files in the data folder
v_files = files;                                                 % Files to be processed

% Open or create a log file to record which channels were interpolated.
logFile = fullfile(rootfolder, 'interpolation_log.txt');
logFID = fopen(logFile, 'a'); % Open in append mode
fprintf(logFID, 'Interpolation log - Session started on %s\n', datestr(now));
fprintf(logFID, '---------------------------------------\n');

for v = 1:length(v_files)
    % Load the dataset
    EEG = pop_loadset(v_files(v).name, [rootfolder loadfolder]);
    
    % Display the EEG data for visual inspection.
    pop_eegplot(EEG, 1, 1, 1);
    disp(['Examine the data for file: ' v_files(v).name]);
    pause;  % Pause to allow manual inspection; press any key to continue.
    
    % Prompt to manually input the bad channel indices.
    prompt = {'Enter bad channel indices separated by commas (e.g., 1,3,5):'};
    dlgtitle = ['Bad channels for ' v_files(v).name];
    dims = [1 50];
    definput = {''};
    answer = inputdlg(prompt, dlgtitle, dims, definput);
    
    % Parse the input string to a numeric vector. If empty, no channels are marked as bad.
    if ~isempty(answer)
        if isempty(answer{1})
            badChannels = [];
        else
            badChannels = str2num(answer{1});  
        end
    else
        badChannels = [];
    end
    
    fprintf('File %s: Selected bad channels: %s\n', v_files(v).name, mat2str(badChannels));
    % Write to log file
    fprintf(logFID, 'File: %s - Interpolated channels: %s\n', v_files(v).name, mat2str(badChannels));
    
    % If any bad channels were specified, remove and interpolate them.
    if ~isempty(badChannels)
        % Save original channel locations for interpolation.
        orig_chanlocs = EEG.chanlocs;
        % Remove the bad channels.
        EEG = pop_select(EEG, 'nochannel', badChannels);
        % Interpolate the removed channels using spherical spline interpolation.
        EEG = pop_interp(EEG, orig_chanlocs, 'spherical');
    end
    
    % Save the modified dataset in the specified save folder.
    pop_saveset(EEG, 'filename', [ v_files(v).name], 'filepath', [rootfolder savefolder]);
end

fclose(logFID);  % Close the log file when done

%%

eeglab % SCRIPT B - Running ICA

clear all;
close all;
rootfolder = 'YOUR_ROOTFOLDER'; %the folder in which the data, processing, and codefolders are situated
loadfolder = 'Preprocessed Data\Script A2\'; %the folder from which you load the filtered and eventlisted data
savefolder = 'Preprocessed Data\Script B\'; %the folder where you save your data with ICA weights
codefolder = 'Codes\'; %the folder where your equationlists and binlisters are situated

files = dir([rootfolder loadfolder '*.set']); %this finds all the .set datasets datafolder
v = 1:length(files); %an array with the length and/or location of the EEG datafiles in the datafolder 
v_files = files(v);
 for v = 1:length(v_files) %this forms a loop for doing the ICA on multiple datasets, on the datasets specified before in v

[a,shortName,c] = fileparts(v_files(v).name);     
outputFile = fullfile(rootfolder, savefolder, [shortName '.set']); %this allows to stop the loop and continue at a later time point
if exist(outputFile, 'file')
        fprintf('Skipping already processed file: %s\n', shortName);
        continue;
    end

EEG = pop_loadset( v_files(v).name, [rootfolder loadfolder]);

pop_eegplot(EEG,1,1,0); %plots the EEG data, mostly a check to see if the data loaded correctly

EEG = pop_runica(EEG, 'extended',1,'stop',1e-07,'interupt','off'); 
%runs the ICA. !!This takes a long time (around an hour)
%if you prefer a popup window, use: EEG = pop_runica(EEG)

pop_saveset(EEG, 'filename',[shortName '.set'],'filepath',[rootfolder savefolder]) %saves the EEG data with ICA weights

 end


 %% SCRIPT C - Manually ICA component rejection + logging

 eeglab 

clear all;
close all;
rootfolder = 'YOUR_ROOTFOLDER'; % the folder for data, processing, and code
loadfolder = 'Preprocessed Data\Script B\';                     % folder to load filtered/eventlisted data
savefolder = 'Preprocessed Data\Script C\';                        % folder to save data with ICA weights
codefolder = 'Codes\';                                             % folder with equation lists and binlisters

files = dir([rootfolder loadfolder '*.set']); % finds all the .set datasets in the datafolder
v = 1:length(files); % indices of EEG datafiles to process
v_files = files(v);

for v = 1:length(v_files)  % loop over each file

    [a, shortName, c] = fileparts(v_files(v).name);     %skips files that have been processed before, again allows to break loop and continue later
    outputFile = fullfile(rootfolder, savefolder, [shortName '.set']);
    if exist(outputFile, 'file')
        fprintf('Skipping already processed file: %s\n', shortName);
        continue;
    end

    EEG = pop_loadset(v_files(v).name, [rootfolder loadfolder]);
    % pop_topoplot(EEG, 0) % (optional) plot topographical maps of ICA components
 
    %due to interpolation variable size of ICA matrix
    %pop_selectcomps(EEG, 1:EEG.nbchan)
    pop_selectcomps(EEG, 1:size(EEG.icaweights,1))


    pop_eegplot(EEG, 0, 1, 0) % plot timeline of components
    pop_eegplot(EEG, 1, 1, 0) % plot EEG data timeline

    % Interactive component rejection loop:
    accepted = false;
    while ~accepted
        ICAEEG = pop_subcomp(EEG);  % open interactive window to reject components
        response = questdlg('Do you accept these rejected components?', ...
                            'Component Rejection Confirmation', ...
                            'Yes','No','Yes');
        if strcmp(response, 'Yes')
            accepted = true;
        else
            disp('Re-run component rejection. You can select a different set of components.');
        end
    end



    % Pop up an input dialog for logging the rejected components.
    % The default value is the computed list.
    logResponse = inputdlg({'Enter rejected components:'}, ...
                        'Log Rejected Components', [1 50], {''});
   if ~isempty(logResponse) && ~isempty(logResponse{1})
     loggedRejComponents = str2num(logResponse{1});  
    else
     loggedRejComponents = NaN;
    end

    % Log the file name and the rejected components.
    logFile = fullfile(rootfolder, 'ICA_component_rejections.txt');
    fid = fopen(logFile, 'a');
    fprintf(fid, 'File: %s, Rejected Components: %s\n', shortName, mat2str(loggedRejComponents));
    fclose(fid);

    pop_saveset(ICAEEG, 'filename', [shortName '.set'], 'filepath', [rootfolder savefolder])
end

%%

% Script D: Rereference + Resample + Epoch Rejection

clearvars; close all; clc;
eeglab;

rootfolder   = 'YOUR_ROOTFOLDER';
loadfolder   = 'Preprocessed Data\Script C\';
savefolder   = 'Preprocessed Data\Script D\';
csv_filename = 'Epoch_Rejection_Log_ALL.csv';

% Input files 
files  = dir(fullfile(rootfolder, loadfolder, '*.set'));
nFiles = numel(files);

% Output folder 
full_savefolder = fullfile(rootfolder, savefolder);
if ~exist(full_savefolder, 'dir')
    mkdir(full_savefolder);
end


% CSV log 
csv_logfile = fullfile(full_savefolder, csv_filename);
csv_exists  = isfile(csv_logfile);
csv_header  = {'Filename', 'Event_Type', 'Kept', 'Rejected'};

for v = 1:nFiles

    [~, shortName, ~] = fileparts(files(v).name);
    outputFile = fullfile(full_savefolder, [shortName '.set']);

    % Skip if already processed
    if exist(outputFile, 'file')
        fprintf('Skipping already processed file: %s\n', shortName);
        continue;
    end

    fprintf('\n Processing file %d/%d: %s\n', v, nFiles, shortName);

    % Load EEG set
    EEG = pop_loadset(files(v).name, fullfile(rootfolder, loadfolder));

    %  Rereference + Resample + Epoching 
    EEG = pop_reref(EEG, [], 'exclude', 33:38);
    EEG = pop_resample(EEG, 500);
    EEG = pop_epoch(EEG, {'2','3','4'}, [-0.200 3]);
    EEG = pop_rmbase(EEG, [-200 0]);

    %  Premark epochs exceeding ±100 µV in selected channels 
    relevant_channels = {'Pz', 'CP1', 'CP2'};
    chan_labels = {EEG.chanlocs.labels};
    chan_idxs   = find(ismember(chan_labels, relevant_channels));

    EEG = pop_eegthresh(EEG, 1, chan_idxs, -100, 100, EEG.xmin, EEG.xmax, 0, 0);

    % prevent auto-rejection (mark only)
    EEG.reject.rejthreshE = zeros(size(EEG.reject.rejthresh));

    %  Ensure rejection vectors exist and match trial count 
    if ~isfield(EEG.reject, 'rejmanual') || length(EEG.reject.rejmanual) ~= EEG.trials
        EEG.reject.rejmanual = false(1, EEG.trials);
    end
    if ~isfield(EEG.reject, 'rejthresh') || length(EEG.reject.rejthresh) ~= EEG.trials
        EEG.reject.rejthresh = false(1, EEG.trials);
    end

    % Combine premarked and manual rejection for visualization
    EEG.reject.rejmanual  = EEG.reject.rejmanual | EEG.reject.rejthresh;
    EEG.reject.rejmanualE = EEG.reject.rejmanual;

    %  Manual rejection: show only first 32 channels 
    EEG_temp = EEG;
    EEG_temp.data     = EEG.data(1:32, :, :);
    EEG_temp.nbchan   = 32;
    EEG_temp.chanlocs = EEG.chanlocs(1:32);
    EEG_temp.chaninfo = EEG.chaninfo;

    % Copy rejection info but fix field sizes to avoid indexing error
    EEG_temp.reject.rejmanual  = EEG.reject.rejmanual;
    EEG_temp.reject.rejthresh  = EEG.reject.rejthresh;

    % Ensure electrode-specific fields match 32 channels (32 x trials)
    EEG_temp.reject.rejmanualE = repmat(EEG_temp.reject.rejmanual, 32, 1);
    EEG_temp.reject.rejthreshE = repmat(EEG_temp.reject.rejthresh, 32, 1);

    %  Launch EEG plot safely 
    pop_eegplot(EEG_temp, 1, 1, 0, [], 'winlength', 5);
    disp('Mark bad epochs in the EEG plot and close the window to continue...');
    while ~isempty(findall(0, 'tag', 'EEGPLOT'))
        pause(1);
    end

    %  Ensure rejmanual exists (again, for safety) 
    if ~isfield(EEG.reject, 'rejmanual') || length(EEG.reject.rejmanual) ~= EEG.trials
        EEG.reject.rejmanual = false(1, EEG.trials);
    end

    %  Count rejected/kept epochs 
    event_types = {'2','3','4'};
    log_rows = {};

    for i = 1:length(event_types)
        etype = event_types{i};
        idxs  = find(strcmp({EEG.epoch.eventtype}, etype));

        kept     = sum(~EEG.reject.rejmanual(idxs));
        rejected = length(idxs) - kept;

        log_rows{end+1,1} = shortName;
        log_rows{end,2}   = etype;
        log_rows{end,3}   = kept;
        log_rows{end,4}   = rejected;
    end

    % Convert to table and write to CSV
    T = cell2table(log_rows, 'VariableNames', csv_header);

    if ~csv_exists
        writetable(T, csv_logfile); % write with headers
        csv_exists = true;
        fprintf('CSV log created: %s\n', csv_logfile);
    else
        writetable(T, csv_logfile, 'WriteMode', 'append', 'WriteVariableNames', false);
        fprintf('Appended to CSV log: %s\n', csv_logfile);
    end

    % Save EEG set
    EEG = pop_saveset(EEG, 'filename', [shortName '.set'], 'filepath', full_savefolder);

    fprintf('Processed %s | Total kept: %d | Total rejected: %d\n', ...
        shortName, sum(~EEG.reject.rejmanual), sum(EEG.reject.rejmanual));

end


%%
%Script E (averaging)

eeglab
clear all;

rootfolder = 'YOUR_ROOTFOLDER'; 
loadfolder = 'Preprocessed Data\Script D\'; 
savefolder = 'Preprocessed Data\Script E\'; 

files = dir([rootfolder loadfolder '*.set']);
v_files = files(1:length(files));

full_savefolder = fullfile(rootfolder, savefolder);
if ~exist(full_savefolder, 'dir')
    mkdir(full_savefolder);
end


for v = 1:length(v_files)

    [~, shortName, ~] = fileparts(v_files(v).name);
    outputFile = fullfile(full_savefolder, [shortName '.erp']);

    if exist(outputFile, 'file')
        fprintf('Skipping already processed file: %s\n', shortName);
        continue;
    end
    
    EEG = pop_loadset('filename', v_files(v).name, 'filepath', [rootfolder loadfolder]);

    ERP = pop_averager( EEG , 'Criterion', 'good', 'DQ_flag', 1, 'ExcludeBoundary', 'on', 'SEM', 'on');
%computes the average ERPs from all the separate events
%only includes epochs not marked during epoch rejection


ERP = pop_savemyerp(ERP, 'erpname', shortName, 'filename', [shortName '.erp'], 'filepath', [rootfolder savefolder]);
%saves the ERP component 

end


%% SCRIPT F:
%          (1) Create Pz/CP1/CP2 average channel in each ERP file
%         (2) Load the new ERPsets into ALLERP
%         (3) Export mean amplitudes (bins 2–4) in time windows to .xls (long format)

clear;
[ALLEEG, EEG, CURRENTSET, ALLCOM] = eeglab; 

% ----------------------------
% USER SETTINGS
rootfolder   = 'YOUR_ROOTFOLDER';

inERPfolder  = 'Script E';          % input .erp files
outERPfolder = 'Script F';      % output .erp with averaged channel
outXLSfolder = 'Script F\';                  % output measurement files

bins_to_use  = 2:4;      % conditions
avgChanIdx   = 39;       % the new channel index 
labelNewChan = 'P3avg';  % name for the new averaged channel

% as extracted before the channel indices
idxPz  = 13;
idxCP1 = 9;
idxCP2 = 22;

% Time windows (ms)
win_P3   = [300 500];
win_Early= [500 800];
win_Mid  = [800 1400];
win_Late = [1400 3000];

% ----------------------------
% PATHS + FOLDERS
inPath   = fullfile(rootfolder, inERPfolder);
erpOutPath = fullfile(rootfolder, outERPfolder);
xlsOutPath = fullfile(rootfolder, outXLSfolder);


% ----------------------------
% 1) CREATE NEW ERP FILES WITH AVERAGED CHANNEL
erpFiles = dir(fullfile(inPath, '*.erp'));
fprintf('Found %d ERP files in: %s\n', numel(erpFiles), inPath);
if isempty(erpFiles)
    error('No .erp files found in %s', inPath);
end

for iFile = 1:numel(erpFiles)
    fname = erpFiles(iFile).name;
    [~, short] = fileparts(fname);

    ERP = pop_loaderp('filename', fname, 'filepath', inPath);

    % --- Optional sanity print (does not change data)
    try
        labels = {ERP.chanlocs.labels};
        fprintf('%s | ch%d=%s, ch%d=%s, ch%d=%s\n', fname, idxCP1, labels{idxCP1}, idxPz, labels{idxPz}, idxCP2, labels{idxCP2});
    catch
        % if chanlocs missing/odd, continue silently
    end

    % Create averaged channel at ch39
    expr = sprintf('ch%d = (ch%d + ch%d + ch%d)/3', avgChanIdx, idxPz, idxCP1, idxCP2);
    ERP  = pop_erpchanoperator(ERP, {expr});

    % Label the new channel 
    if isfield(ERP, 'chanlocs') && numel(ERP.chanlocs) >= avgChanIdx
        ERP.chanlocs(avgChanIdx).labels = labelNewChan;
    end

    newName = [short '_withAvg.erp'];
    pop_savemyerp(ERP, 'erpname', ERP.erpname, ...
        'filename', newName, ...
        'filepath', erpOutPath);

    fprintf(' Saved: %s\n', fullfile(erpOutPath, newName));
end

fprintf('Finished creating ERPsets with averaged channel.\n\n');

% ----------------------------
% 2) LOAD NEW ERP FILES INTO ALLERP
global ALLERP
ALLERP = []; 

newERPFiles = dir(fullfile(erpOutPath, '*_withAvg.erp'));
fprintf('Found %d new ERP files in: %s\n', numel(newERPFiles), erpOutPath);
if isempty(newERPFiles)
    error('No *_withAvg.erp files found in %s', erpOutPath);
end

ALLERP = repmat(struct, 0, 1);  % initialize struct array cleanly

for iFile = 1:numel(newERPFiles)
    ERP = pop_loaderp('filename', newERPFiles(iFile).name, 'filepath', erpOutPath);

    if isempty(fieldnames(ALLERP))
        ALLERP = ERP;
    else
        ALLERP(end+1) = ERP; 
    end
end

nERPsets = numel(ALLERP);
fprintf('Loaded %d ERPsets into ALLERP.\n\n', nERPsets);


% ----------------------------
% 3) EXPORT MEASUREMENTS (LONG FORMAT)

erpsets_to_use = 1:nERPsets;

chan_P3  = avgChanIdx;  % Pz/CP1/CP2 average channel
chan_LPP = idxPz;       % Pz only

% Helper: run pop_geterpvalues
do_export = @(win, chan, outFile) pop_geterpvalues(ALLERP, win, bins_to_use, chan, ...
    'Baseline', 'none', ...
    'Binlabel', 'on', ...
    'Erpsets', erpsets_to_use, ...
    'FileFormat', 'long', ...
    'Filename', outFile, ...
    'Fracreplace', 'NaN', ...
    'InterpFactor', 1, ...
    'Measure', 'meanbl', ...
    'PeakOnset', 1, ...
    'Resolution', 4);

% Export files
outFile_P3    = fullfile(xlsOutPath, 'P3.txt');
outFile_Early = fullfile(xlsOutPath, 'Early_LPP_long.txt');
outFile_Mid   = fullfile(xlsOutPath, 'Mid_LPP.txt');
outFile_Late  = fullfile(xlsOutPath, 'Late_LPP.txt');

% P3 from Pz/CP1/CP2 average
ALLERP = do_export(win_P3, chan_P3, outFile_P3);

% LPP windows from Pz only
ALLERP = do_export(win_Early, chan_LPP, outFile_Early);
ALLERP = do_export(win_Mid,   chan_LPP, outFile_Mid);
ALLERP = do_export(win_Late,  chan_LPP, outFile_Late);

fprintf('Export complete:\n');
fprintf('  %s\n', outFile_P3);
fprintf('  %s\n', outFile_Early);
fprintf('  %s\n', outFile_Mid);
fprintf('  %s\n', outFile_Late);

 %Combine ERPLAB long-format TXT files into one CSV

clear; close all;

rootfolder = 'YOUR_ROOTFOLDER';
loadfolder = 'Script F';

inPath  = fullfile(rootfolder, loadfolder);
outFile = fullfile(inPath, 'All_ERPs_V2.csv');

files = { ...
    'P3.txt',             'P3'; ...
    'Early_LPP_long.txt', 'Early_LPP'; ...
    'Mid_LPP.txt',        'Mid_LPP'; ...
    'Late_LPP.txt',       'Late_LPP' ...
};

AllData = table();

for i = 1:size(files,1)
    fname  = fullfile(inPath, files{i,1});
    window = files{i,2};

    T = readtable(fname, 'FileType', 'text');

    % add window label
    T.Window = repmat({window}, height(T), 1);

    AllData = [AllData; T]; 
end

% Write CSV (R-friendly)
writetable(AllData, outFile);

fprintf('Saved CSV:\n%s\n', outFile);
