unit Main_Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, Apple.IniFiles, FMX.TabControl;

type
  TMainForm = class(TForm)
    btnReadFloat: TButton;
    btnWriteFloat: TButton;
    btnReadBool: TButton;
    btnWriteBool: TButton;
    btnReadSections: TButton;
    btnReadSectionValues: TButton;
    btnEraseSection: TButton;
    btnDeleteKey: TButton;
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
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    btnUpdateFile: TButton;
    btnReadSection: TButton;
    procedure btnReadStringClick(Sender: TObject);
    procedure btnWriteStringClick(Sender: TObject);
    procedure btnReadIntegerClick(Sender: TObject);
    procedure btnWriteIntegerClick(Sender: TObject);
    procedure btnReadFloatClick(Sender: TObject);
    procedure btnWriteFloatClick(Sender: TObject);
    procedure btnReadBoolClick(Sender: TObject);
    procedure btnWriteBoolClick(Sender: TObject);
    procedure btnReadDateClick(Sender: TObject);
    procedure btnWriteDateClick(Sender: TObject);
    procedure btnReadTimeClick(Sender: TObject);
    procedure btnWriteTimeClick(Sender: TObject);
    procedure btnReadDateTimeClick(Sender: TObject);
    procedure btnWriteDateTimeClick(Sender: TObject);
    procedure btnReadSectionsClick(Sender: TObject);
    procedure btnReadSectionValuesClick(Sender: TObject);
    procedure btnEraseSectionClick(Sender: TObject);
    procedure btnDeleteKeyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnUpdateFileClick(Sender: TObject);
    procedure btnReadSectionClick(Sender: TObject);
  private
    { Private declarations }
    FIni: TUserIniFile;
    procedure EnsureSections;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  DateUtils, iOSapi.Foundation, iOSapi.CocoaTypes,
  MacApi.ObjectiveC, Apple.Utils, Macapi.ObjCRuntime;

const
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

procedure TMainForm.btnEraseSectionClick(Sender: TObject);
var
  lDefaults: NSUserDefaults;
  lDict: NSDictionary;

begin
  EnsureSections;
  lDefaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);

  lDict := lDefaults.dictionaryForKey(NSStr('Foo'));
  if Assigned(lDict) then
    ShowMessage('I expected that')
  else
    ShowMessage('Did not expect that');

  FIni.EraseSection('Foo');
  lDict := lDefaults.dictionaryForKey(NSStr('Foo'));
  if Assigned(lDict) then
    ShowMessage('Didn''t expect that either')
  else
    ShowMessage('I expected that too');
end;


procedure TMainForm.EnsureSections;
begin
  FIni.WriteString('Foo', 'FooKey', 'FooVal');
  FIni.WriteInteger('Foo', 'FooKey2', 42);
  FIni.WriteFloat('Foo', 'FooKey3', 2.3);
  FIni.WriteDate('Foo', 'FooKey4', Now);
  FIni.WriteTime('Foo', 'FooKey5', Now);
  FIni.WriteDateTime('Foo', 'FooKey6', Now);
  FIni.WriteBool('Foo', 'FooKey7', True);

  FIni.WriteInteger('FooBar', 'FooBarKey', 42);
  FIni.WriteDate('FooBarFoo', 'FooBarFooKey', Now);
  FIni.WriteString('FooKey4', 'FooVal4');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FIni := TUserIniFile.Create;
end;

procedure TMainForm.btnReadBoolClick(Sender: TObject);
begin
  ShowMessage(BoolToStr(FIni.ReadBool(cnBoolKey, cnBoolDefault), True));
end;

procedure TMainForm.btnReadDateClick(Sender: TObject);
begin
  ShowMessage(DateToStr(Fini.ReadDate(cnDateKey, cnDateDefault)));
end;

procedure TMainForm.btnReadDateTimeClick(Sender: TObject);
begin
  ShowMessage(DateTimeToStr(FIni.ReadDateTime(cnDateTimeKey, cnDateTimeDefault)));
end;

procedure TMainForm.btnReadFloatClick(Sender: TObject);
begin
  ShowMessage(FloatToStr(FIni.ReadFloat(cnFloatKey, cnFloatDefault)));
end;

procedure TMainForm.btnReadIntegerClick(Sender: TObject);
begin
  ShowMessage(IntToStr(FIni.ReadInteger(cnIntegerKey, cnIntegerDefault)));
end;

procedure TMainForm.btnReadSectionsClick(Sender: TObject);
var
  lStrs: TStringList;
begin
  EnsureSections;
  lStrs := TStringList.Create;
  FIni.ReadSections(lStrs);
  ShowMessage(lStrs.Text);
end;

procedure TMainForm.btnReadSectionValuesClick(Sender: TObject);
var
  lStrs: TStringList;
begin
  try
    EnsureSections;
    lStrs := TStringList.Create;
    FIni.ReadSectionValues('Foo', lStrs);
    ShowMessage(lStrs.Text);
  except on E: Exception do
    ShowMessage(E.ToString);
  end;
end;

procedure TMainForm.btnReadStringClick(Sender: TObject);
begin
  ShowMessage(FIni.ReadString(cnStringKey, cnStringDefault));
end;

procedure TMainForm.btnReadTimeClick(Sender: TObject);
begin
  ShowMessage(TimeToStr(FIni.ReadTime(cnTimeKey, cnTimeDefault)));
end;

procedure TMainForm.btnUpdateFileClick(Sender: TObject);
begin
  FIni.UpdateFile;
end;

procedure TMainForm.btnWriteBoolClick(Sender: TObject);
begin
  FIni.WriteBool(cnBoolKey, cnBoolValue);
end;

procedure TMainForm.btnWriteDateClick(Sender: TObject);
begin
  FIni.WriteDate(cnDateKey, DateOf(Now));
end;

procedure TMainForm.btnWriteDateTimeClick(Sender: TObject);
begin
  FIni.WriteDateTime(cnDateTimeKey, Now);
end;

procedure TMainForm.btnWriteFloatClick(Sender: TObject);
begin
  FIni.WriteFloat(cnFloatKey, cnFloatValue);
end;

procedure TMainForm.btnWriteIntegerClick(Sender: TObject);
begin
  FIni.WriteInteger(cnIntegerKey, cnIntegerValue);
end;

procedure TMainForm.btnWriteStringClick(Sender: TObject);
begin
  FIni.WriteString(cnStringKey, cnStringValue);
end;

procedure TMainForm.btnWriteTimeClick(Sender: TObject);
begin
  FIni.WriteTime(cnTimeKey, TimeOf(Now));
end;

procedure TMainForm.btnReadSectionClick(Sender: TObject);
var
  lStrs: TStringList;
begin
  try
    EnsureSections;
    lStrs := TStringList.Create;
    FIni.ReadSection('Foo', lStrs);
    ShowMessage(lStrs.Text);
  except on E: Exception do
    ShowMessage(E.ToString);
  end;
end;

procedure TMainForm.btnDeleteKeyClick(Sender: TObject);
  procedure ShowKeys;
  begin
    ShowMessage(Format('Foo->FooKey=%s, FooKey4=%s',
      [FIni.ReadString('Foo', 'FooKey', ''),
      FIni.ReadString('FooKey4', '')]));
  end;

begin
  EnsureSections;
  ShowKeys;
  FIni.DeleteKey('Foo', 'FooKey');
  ShowKeys;
  FIni.DeleteKey('FooKey4');
  ShowKeys;
end;

end.
