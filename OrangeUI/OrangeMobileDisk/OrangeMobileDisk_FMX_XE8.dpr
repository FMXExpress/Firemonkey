program OrangeMobileDisk_FMX_XE8;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  FileManageFrame in 'FileManageFrame.pas' {FrameFileManage: TFrame},
  HintFrame in 'HintFrame.pas' {FrameHint: TFrame},
  NewFolderFrame in 'NewFolderFrame.pas' {FrameNewFolder: TFrame},
  PictureFileFrame in 'PictureFileFrame.pas' {FramePictureFile: TFrame},
  TextFileFrame in 'TextFileFrame.pas' {FrameTextFile: TFrame},
  uFileManage in 'uFileManage.pas',
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  MainForm in 'MainForm.pas' {frmMain},
  FMX.Platform.iOS in '..\..\OrangeProjectCommon\FMX.Platform.iOS.pas';

{$R *.res}

begin
//  ReportMemoryLeaksONShutdown:=DebugHook<>0;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
