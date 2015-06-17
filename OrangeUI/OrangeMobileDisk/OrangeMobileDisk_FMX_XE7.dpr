program OrangeMobileDisk_FMX_XE7;

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
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
