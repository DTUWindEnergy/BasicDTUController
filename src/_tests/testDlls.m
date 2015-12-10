%% Init
clear all 
close all
clc

if ispc() 
    os_name='windows';
    dll='dll';
else
    os_name='linux';
    dll='so';
end
[~,maxArraySize]=computer; 
if maxArraySize> 2^31
    os_archi='amd64';
else
    os_archi='ia32';
end
config=[os_name '-' os_archi];
%% Defining lib_file and lib_header
lib_name='dtu_we_controller';
lib_header=['../' lib_name '/' lib_name '.h'];
%lib_path='../dtu_we_controller/x64/Debug/';
%lib_path='../_lib-windows-amd64-gfortran/';
%lib_path='../_lib-windows-amd64-ifort/';

%
% Finding compatible directories
DIRs=dir(sprintf('../_lib-%s-*',config));
if isempty(DIRs)
    error('Not candidate lib folder found. Have you compiled the dlls?')
else
    fprintf('Found %d candidate library folders\n',length(DIRs));
end
% Finding compatible dlls
dlls={};
for idir=1:length(DIRs)
    d=DIRs(idir);
    pattern=sprintf('../%s/*%s.*%s',d.name,lib_name,dll);
    f=dir(pattern);
    if ~isempty(f)
        dlls{end+1}=sprintf('../%s/%s',d.name,f.name);
    end
end
fprintf('Found %d candidate library files\n',length(dlls));

%%
% lib_file  =[lib_path lib_name '.dll'];


%% Testing library calls
for ilib = 1:length(dlls)
    lib_file=dlls{ilib};
    fprintf('Attempt to load and call %s...\n',lib_file);
    %loading
    [notfound,warnings]=loadlibrary(lib_file,lib_header,'alias',lib_name);
    if ~isempty(notfound); disp(notfound);end
    % calling
    a1=ones(100);
    a2=ones(1);
    [a1,a2]=calllib(lib_name,'init_regulation',a1,a2);
    if (a2==0); disp('[ ok ]'); end
    % unloading
    unloadlibrary(lib_name)
end