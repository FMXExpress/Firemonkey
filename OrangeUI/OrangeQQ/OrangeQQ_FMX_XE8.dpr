program OrangeQQ_FMX_XE8;























uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uBaseLog,
  ContactorFrame in 'ContactorFrame.pas' {FrameContactor: TFrame},
  LoginFrame in 'LoginFrame.pas' {FrameLogin: TFrame},
  MainForm in 'MainForm.pas' {frmMain},
  MessageFrame in 'MessageFrame.pas' {FrameMessage: TFrame},
  SettingFrame in 'SettingFrame.pas' {FrameSetting: TFrame},
  StateFrame in 'StateFrame.pas' {FrameState: TFrame},
  TalkFrame in 'TalkFrame.pas' {FrameTalk: TFrame},
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  MessageHintFrame in 'MessageHintFrame.pas' {FrameMessageHint: TFrame},
  uMobileUtils in '..\..\OrangeProjectCommon\uMobileUtils.pas',
  FMX.Platform.iOS in '..\..\OrangeProjectCommon\FMX.Platform.iOS.pas';

{$R *.res}

begin
//  ReportMemoryLeaksONShutdown:=DebugHook<>0;
  Application.Initialize;

  GetGlobalBaseLog.RemoteDebugServer:='192.168.10.103';
//  {$IFDEF MSWINDOWS}
//  GetGlobalBaseLog.RemoteDebugServer:='127.0.0.1';
//  {$ENDIF}
//  GetGlobalBaseLog.EnableRemoteDebug:=True;


  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
