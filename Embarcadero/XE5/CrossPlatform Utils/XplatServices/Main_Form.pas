unit Main_Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, Xplat.Services, FMX.TabControl, FMX.Layouts, System.Actions,
  FMX.ActnList;

type
  TMainForm = class(TForm)
    btnReadFloat: TButton;
    btnWriteFloat: TButton;
    btnReadBool: TButton;
    btnWriteBool: TButton;
    btnReadString: TButton;
    btnWriteString: TButton;
    btnReadInteger: TButton;
    btnWriteInteger: TButton;
    btnReadDate: TButton;
    btnWriteDate: TButton;
    btnReadDateTime: TButton;
    btnWriteDateTime: TButton;
    btnReadTime: TButton;
    btnWriteTime: TButton;
    TabControl1: TTabControl;
    IniFileTab: TTabItem;
    PleaseWaitTab: TTabItem;
    ToolBar1: TToolBar;
    Label1: TLabel;
    VertScrollBox1: TVertScrollBox;
    btnStartWait: TButton;
    btnStopWait: TButton;
    ActionList1: TActionList;
    ReadStringAction: TAction;
    WriteStringAction: TAction;
    ReadIntegerAction: TAction;
    WriteIntegerAction: TAction;
    ReadFloatAction: TAction;
    WriteFloatAction: TAction;
    ReadBoolAction: TAction;
    WriteBoolAction: TAction;
    ReadDateAction: TAction;
    WriteDateAction: TAction;
    ReadTimeAction: TAction;
    WriteTimeAction: TAction;
    ReadDateTimeAction: TAction;
    WriteDateTimeAction: TAction;
    ReadSectionsAction: TAction;
    ReadSectionAction: TAction;
    ReadSectionValuesAction: TAction;
    DeleteKeyAction: TAction;
    StartWaitAction: TAction;
    StopWaitAction: TAction;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    Layout7: TLayout;
    Layout8: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnUpdateFileClick(Sender: TObject);
    procedure ReadStringActionExecute(Sender: TObject);
    procedure WriteStringActionExecute(Sender: TObject);
    procedure ReadIntegerActionExecute(Sender: TObject);
    procedure WriteIntegerActionExecute(Sender: TObject);
    procedure ReadFloatActionExecute(Sender: TObject);
    procedure WriteFloatActionExecute(Sender: TObject);
    procedure ReadBoolActionExecute(Sender: TObject);
    procedure WriteBoolActionExecute(Sender: TObject);
    procedure ReadDateActionExecute(Sender: TObject);
    procedure WriteDateActionExecute(Sender: TObject);
    procedure ReadTimeActionExecute(Sender: TObject);
    procedure WriteTimeActionExecute(Sender: TObject);
    procedure ReadDateTimeActionExecute(Sender: TObject);
    procedure WriteDateTimeActionExecute(Sender: TObject);
    procedure ReadSectionsActionExecute(Sender: TObject);
    procedure ReadSectionActionExecute(Sender: TObject);
    procedure ReadSectionValuesActionExecute(Sender: TObject);
    procedure DeleteKeyActionExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure StartWaitActionExecute(Sender: TObject);
    procedure StopWaitActionExecute(Sender: TObject);
  private
    { Private declarations }
    FIni: IIniFileService;
    FWait: IPleaseWaitService;
    procedure EnsureSections;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  DateUtils, FMX.Platform;

const
  cnSection = 'SectionName';
  cnStringKey = 'StringKey';
  cnStringValue = 'StringValue';
  cnStringDefault = '';
  cnIntegerKey = 'IntegerKey';
  cnIntegerValue = 42;
  cnIntegerDefault = 0;
  cnFloatKey = 'FloatKey';
  cnFloatValue= 2.6;
  cnFloatDefault = 0;
  cnBoolKey = 'BoolKey';
  cnBoolValue= True;
  cnBoolDefault = False;
  cnDateKey = 'DateKey';
  cnDateDefault = 0;
  cnTimeKey = 'TimeKey';
  cnTimeDefault = 0;
  cnDateTimeKey = 'DateTimeKey';
  cnDateTimeDefault = 0;


{$R *.fmx}

procedure TMainForm.EnsureSections;
begin
  FIni.WriteString('Foo', 'FooKey', 'FooVal');
  FIni.WriteInteger('Foo', 'FooKey2', 42);
  FIni.WriteFloat('Foo', 'FooKey3', 2.3);
  FIni.WriteDate('Foo', 'FooKey4', Now);
  FIni.WriteTime('Foo', 'FooKey5', Now);
  FIni.WriteDateTime('Foo', 'FooKey6', Now);
  FIni.WriteBool('Foo', 'FooKey7', True);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if TPlatformServices.Current.SupportsPlatformService(IIniFileService) then
    FIni := TPlatformServices.Current.GetPlatformService(IIniFileService) as IIniFileService;

  if TPlatformServices.Current.SupportsPlatformService(IPleaseWaitService) then
    FWait := TPlatformServices.Current.GetPlatformService(IPleaseWaitService) as IPleaseWaitService;
end;

procedure TMainForm.ReadBoolActionExecute(Sender: TObject);
begin
  ShowMessage(BoolToStr(FIni.ReadBool(cnSection, cnBoolKey, cnBoolDefault), True));
end;

procedure TMainForm.ReadDateActionExecute(Sender: TObject);
begin
  ShowMessage(DateToStr(Fini.ReadDate(cnSection, cnDateKey, cnDateDefault)));
end;

procedure TMainForm.ReadDateTimeActionExecute(Sender: TObject);
begin
  ShowMessage(DateTimeToStr(FIni.ReadDateTime(cnSection, cnDateTimeKey, cnDateTimeDefault)));
end;

procedure TMainForm.ReadFloatActionExecute(Sender: TObject);
begin
  ShowMessage(FloatToStr(FIni.ReadFloat(cnSection, cnFloatKey, cnFloatDefault)));
end;

procedure TMainForm.ReadIntegerActionExecute(Sender: TObject);
begin
  ShowMessage(IntToStr(FIni.ReadInteger(cnSection, cnIntegerKey, cnIntegerDefault)));
end;

procedure TMainForm.ReadSectionActionExecute(Sender: TObject);
var
  lStrs: TStringList;
begin
  EnsureSections;
  lStrs := TStringList.Create;
  FIni.ReadSection('Foo', lStrs);
  ShowMessage(lStrs.Text);
end;

procedure TMainForm.ReadSectionsActionExecute(Sender: TObject);
var
  lStrs: TStringList;
begin
  EnsureSections;
  lStrs := TStringList.Create;
  FIni.ReadSections(lStrs);
  ShowMessage(lStrs.Text);
end;

procedure TMainForm.ReadSectionValuesActionExecute(Sender: TObject);
var
  lStrs: TStringList;
begin
  EnsureSections;
  lStrs := TStringList.Create;
  FIni.ReadSectionValues('Foo', lStrs);
  ShowMessage(lStrs.Text);
end;

procedure TMainForm.ReadStringActionExecute(Sender: TObject);
begin
  ShowMessage(FIni.ReadString(cnSection, cnStringKey, cnStringDefault));
end;

procedure TMainForm.ReadTimeActionExecute(Sender: TObject);
begin
  ShowMessage(TimeToStr(FIni.ReadTime(cnSection, cnTimeKey, cnTimeDefault)));
end;

procedure TMainForm.StartWaitActionExecute(Sender: TObject);
begin
  FWait.StartWait;
  {$IFDEF ANDROID}
  //The Android implementation of IPleaseWaitService uses the
  //android.app.ProgressDialog class, which overlays the UI with a modal view.
  //So we'll automatically call IPleaseWaitService.StopWait after a couple of
  //seconds
  Sleep(2000);
  FWait.StopWait;
  {$ENDIF}
end;

procedure TMainForm.StopWaitActionExecute(Sender: TObject);
begin
  FWait.StopWait;
end;

procedure TMainForm.WriteBoolActionExecute(Sender: TObject);
begin
  FIni.WriteBool(cnSection, cnBoolKey, cnBoolValue);
end;

procedure TMainForm.WriteDateActionExecute(Sender: TObject);
begin
  FIni.WriteDate(cnSection, cnDateKey, DateOf(Now));
end;

procedure TMainForm.WriteDateTimeActionExecute(Sender: TObject);
begin
  FIni.WriteDateTime(cnSection, cnDateTimeKey, Now);
end;

procedure TMainForm.WriteFloatActionExecute(Sender: TObject);
begin
  FIni.WriteFloat(cnSection, cnFloatKey, cnFloatValue);
end;

procedure TMainForm.WriteIntegerActionExecute(Sender: TObject);
begin
  FIni.WriteInteger(cnSection, cnIntegerKey, cnIntegerValue);
end;

procedure TMainForm.WriteStringActionExecute(Sender: TObject);
begin
  FIni.WriteString(cnSection, cnStringKey, cnStringValue);
end;

procedure TMainForm.WriteTimeActionExecute(Sender: TObject);
begin
  FIni.WriteTime(cnSection, cnTimeKey, TimeOf(Now));
end;

procedure TMainForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
var
  lAction: TAction;
begin
  lAction := TAction(Action);
  lAction.Enabled :=
    ((lAction.Category = 'IIniFileService') and Assigned(FIni)) or
    ((lAction.Category = 'IPleaseWaitService') and Assigned(FWait));

{$IFDEF ANDROID}
  //The Android implementation of IPleaseWaitService uses the
  //android.app.ProgressDialog class, which overlays the UI with a modal view.
  //So we'll automatically call IPleaseWaitService.StopWait after a couple of
  //seconds
  if lAction.Enabled AND (lAction = StopWaitAction) then
    lAction.Enabled := False;
{$ENDIF}
end;

procedure TMainForm.btnUpdateFileClick(Sender: TObject);
begin
  FIni.UpdateFile;
end;

procedure TMainForm.DeleteKeyActionExecute(Sender: TObject);
  procedure ShowKeys;
  begin
    ShowMessage(Format('Foo->FooKey=%s, FooKey2=%d',
      [FIni.ReadString('Foo', 'FooKey', ''),
       FIni.ReadInteger('Foo', 'FooKey2', 0)]));
  end;

begin
  EnsureSections;
  ShowKeys;
  FIni.DeleteKey('Foo', 'FooKey');
  ShowKeys;
  FIni.DeleteKey('Foo', 'FooKey2');
  ShowKeys;
end;

end.
