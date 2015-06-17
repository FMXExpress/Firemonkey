program OrangeGuide_FMX_XE7;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  GuideFrame in 'GuideFrame.pas' {FrameGuide: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
