unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, Winapi.Winrt, System.WinrtHelpers,
  ShlObj, ComObj, ActiveX, Winapi.PropSys, Winapi.PropKey, WinAPI.UI.Notifications, WinAPI.Data,
  WinAPI.Foundation.Types, WinAPI.Storage, WinAPI.Foundation, WinAPI.CommonTypes,
  Winapi.Windows, FMX.Platform.Win;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

type
  TAcceptedEventHandler = class(TInspectableObject, TypedEventHandler_2__IToastNotification__IInspectable_Delegate_Base{, Windows_UI_Notifications_IToastActivated}, TypedEventHandler_2__IToastNotification__IInspectable)
    procedure Invoke(sender: IToastNotification; args: IInspectable); safecall;
  end;


implementation

{$R *.fmx}

function GetDesktopFolder: string;
var
  PIDList: PItemIDList;
  Buffer: array [0..MAX_PATH-1] of Char;
begin
  Result := '';
  SHGetSpecialFolderLocation(FmxHandleToHWND(Form1.Handle), CSIDL_DESKTOP, PIDList);
  if Assigned(PIDList) then
    if SHGetPathFromIDList(PIDList, Buffer) then
      Result := Buffer;
end;

function GetStartMenuFolder: string;
var
  Buffer: array [0..MAX_PATH-1] of Char;
begin
  Result := '';
  GetEnvironmentVariable(PChar('APPDATA'), Buffer, MAX_PATH - 1);
  Result := Buffer + '\Microsoft\Windows\Start Menu\Programs\Desktop Delphi Toasts App.lnk';
end;


function CreateDesktopShellLink(const TargetName: string): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  LinkName: string;

  LStore: IPropertyStore;
  LValue: TPropVariant;
begin
  Result := False;

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;
  LStore := IObject as IPropertyStore;

  with ISLink do
  begin
    SetPath(PChar(ParamStr(0)));
  end;
  ISLink.SetArguments(PChar(''));

  if Succeeded(InitPropVariantFromStringAsVector(
    PWideChar('Delphi.DesktopNotification.Sample'), LValue)) then
  begin
    if Succeeded(LStore.SetValue(PKEY_AppUserModel_ID, LValue)) then
      LStore.Commit;
  end;

  LinkName := GetStartMenuFolder;

  if not FileExists(LinkName) then
    if IPFile.Save(PWideChar(LinkName), TRUE) = S_OK then
      Result := True;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  LString: HString;
  LString2: HString;
  LINotificationManagerStatics: IToastNotificationManagerStatics;
  LXMLTemplate: Xml_Dom_IXmlDocument;
  LCreated: IInspectable;
  LToast: IToastNotification;
  LToastFactory: IToastNotificationFactory;
  LToastNotifier: IToastNotifier;
  LAccepted: TAcceptedEventHandler;
begin
  if Succeeded(WindowsCreateString(PWideChar(SToastNotificationManager), Length(SToastNotificationManager), LString)) then
  begin
    if Succeeded(RoGetActivationFactory(LString, TGUID.Create('{50AC103F-D235-4598-BBEF-98FE4D1A3AD4}'), LCreated)) then
    begin
      LINotificationManagerStatics := LCreated as IToastNotificationManagerStatics;
      if Succeeded(WindowsCreateString(PWideChar(Edit1.Text), Length(Edit1.Text), LString2)) then
      begin
        LToastNotifier := LINotificationManagerStatics.CreateToastNotifier(LString2);
        LXMLTemplate := LINotificationManagerStatics.GetTemplateContent(ToastTemplateType.ToastText02);

        if Succeeded(WindowsCreateString(PWideChar(SToastNotification), Length(SToastNotification), LString)) then
        begin
          if Succeeded(RoGetActivationFactory(LString, TGUID.Create('{04124B20-82C6-4229-B109-FD9ED4662B53}'), LCreated)) then
          LToastFactory := LCreated as IToastNotificationFactory;
          LToast := LToastFactory.CreateToastNotification(LXMLTemplate);
          LAccepted := TAcceptedEventHandler.Create;
          LToast.add_Activated(LAccepted);
          LToastNotifier.Show(LToast);
        end;
      end;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RoInitialize(RO_INIT_MULTITHREADED);
  CreateDesktopShellLink(ParamStr(0));
end;

{ TAcceptedEventHandler }

procedure TAcceptedEventHandler.Invoke(sender: IToastNotification; args: IInspectable);
begin
  Form1.Memo1.Lines.Add ('You clicked on the notification!');
end;

end.
