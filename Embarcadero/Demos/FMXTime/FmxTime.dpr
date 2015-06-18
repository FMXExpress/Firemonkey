program FmxTime;









uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  UMain in 'UMain.pas' {FrmMain},
  UInfo in 'UInfo.pas' {FrmInfo};

{$IFDEF IOS}
{$R *.ios.res}
{$ENDIF}
{$IFDEF ANDROID}
{$R *.android.res}
{$ENDIF}
{$IFDEF WIN32}
{$R *.win.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmInfo, FrmInfo);
  Application.Run;
end.
