unit Android.Services;

interface

uses
  XPlat.Services, Android.ProgressDialog, Androidapi.JNI.App;

type

  TAndroidPleaseWait = class(TInterfacedObject, IPleaseWaitService)
  private
    class var
      FTitleText: string;
      FMessageText: string;
  public
    FDialog: JProgressDialog;
    procedure StartWait;
    procedure StopWait;
    class property TitleText: string read FTitleText write FTitleText;
    class property MessageText: string read FMessageText write FMessageText;
    class constructor Create;
  end;

resourcestring
  rsPleaseWaitTitle = 'In progress';
  rsPleaseWaitMessage = 'Please wait...';

implementation

uses
  FMX.Platform, Xplat.IniFiles, System.IniFiles, System.IOUtils,
  FMX.Platform.Android, FMX.Helpers.Android, FMX.Dialogs, System.UITypes,
  System.SysUtils;

{ TPleaseWaitService }

class constructor TAndroidPleaseWait.Create;
begin
  inherited;
  FTitleText := rsPleaseWaitTitle;
  FMessageText := rsPleaseWaitMessage;
end;

procedure TAndroidPleaseWait.StartWait;
begin
  if not Assigned(FDialog) then
    CallInUIThreadAndWaitFinishing(procedure
      begin
        FDialog := TJProgressDialog.JavaClass.show(SharedActivity,
          StrToJCharSequence(FTitleText), StrToJCharSequence(FMessageText), False, False);
      end);
end;

procedure TAndroidPleaseWait.StopWait;
begin
  if Assigned(FDialog) then
  begin
    FDialog.cancel;
    FDialog := nil;
  end;
end;

initialization
  //On Android, the ini file will be stored in TPath.GetDocumentsPath + '/Prefs.ini'.
  //For Android 4.2, this results in a path of '/data/data/<app package>/files/Prefs.ini'.
  //However, if you want to deploy a default ini file with your app, you should
  //set its Remote Path to 'Assets\Internal' when configuring the file for deployment.
  TPlatformServices.Current.AddPlatformService(IIniFileService, TXplatIniFile.Create);
  TPlatformServices.Current.AddPlatformService(IPleaseWaitService, TAndroidPleaseWait.Create);

finalization
  TPlatformServices.Current.RemovePlatformService(IPleaseWaitService);
  TPlatformServices.Current.RemovePlatformService(IIniFileService);

end.
