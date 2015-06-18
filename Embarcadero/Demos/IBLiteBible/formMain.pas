unit formMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.MobilePreview, FMX.ListView.Types, FMX.ListView, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, FMX.Layouts,
  FMX.TabControl, Fmx.Bind.Navigator, System.Actions, FMX.ActnList, FMX.Memo,
  FMX.WebBrowser, Data.Bind.GenData, Data.Bind.ObjectScope, unitSettings;

type
  TfrmMain = class(TForm)
    Header: TToolBar;
    HeaderLabel: TLabel;
    lvBooks: TListView;
    bsBooks: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    tcBible: TTabControl;
    tabBooks: TTabItem;
    tabChapters: TTabItem;
    tabVerses: TTabItem;
    ToolBar1: TToolBar;
    lblChapters: TLabel;
    lvChapters: TListView;
    bsChapters: TBindSourceDB;
    LinkListControlToField2: TLinkListControlToField;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    actPriorBook: TFMXBindNavigatePrior;
    actNextBook: TFMXBindNavigateNext;
    ToolBar2: TToolBar;
    lblVerses: TLabel;
    Button3: TButton;
    Button4: TButton;
    actPriorChapter: TFMXBindNavigatePrior;
    actNextChapter: TFMXBindNavigateNext;
    dsVerses: TBindSourceDB;
    Button5: TButton;
    Button6: TButton;
    ctaChapter: TChangeTabAction;
    ctaBook: TChangeTabAction;
    Label1: TLabel;
    LinkPropertyToFieldText: TLinkPropertyToField;
    ToolBar3: TToolBar;
    WebBrowser1: TWebBrowser;
    btnSettings: TButton;
    tcVerses: TTabControl;
    tabVerseText: TTabItem;
    tabVerseSettings: TTabItem;
    ToolBar4: TToolBar;
    Label2: TLabel;
    ToolBar5: TToolBar;
    Button8: TButton;
    ctaVerseDetail: TChangeTabAction;
    WebBrowser2: TWebBrowser;
    tbFontNewSize: TTrackBar;
    btnUpdateFont: TButton;
    lblFontSize: TLabel;
    PrototypeBindSource1: TPrototypeBindSource;
    LinkControlToField1: TLinkControlToField;
    LinkPropertyToFieldText2: TLinkPropertyToField;
    procedure FormCreate(Sender: TObject);
    procedure lvBooksItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure lvChaptersItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure UpdateVerses(Sender: TObject);
    function UpdateVersesCommon(Size : Integer): Boolean;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbFontNewSizeChange(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure btnUpdateFontClick(Sender: TObject);
    procedure tcVersesChange(Sender: TObject);
    procedure tcBibleChange(Sender: TObject);
  private
    { Private declarations }
    Loaded : Boolean;
    TempFile : string;
    procedure SetVisibleBrowser;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  Settings : TSettings;

implementation

uses dmMain,  BackButtonManager, IOUtils, StrUtils;

{$R *.fmx}

procedure TfrmMain.UpdateVerses(Sender: TObject);
begin
  UpdateVersesCommon(Settings.FontSize);
  WebBrowser1.Navigate('');
  WebBrowser1.Navigate('file://'+TempFile);
end;

function TfrmMain.UpdateVersesCommon(Size: Integer): Boolean;
var
  SL : TStringList;
  VerseText: string;
begin
  SL := TStringList.Create;
  try
    if not dtmdlMain.qryVerses.Active then
      Exit;

    lblVerses.Text := dtmdlMain.qryBooksSHORT_NAME.Text+':'+dtmdlMain.qryChaptersCHAPTER_NUMBER.AsString;

    SL.Add('<head>');
    SL.Add('<style>');
    SL.Add(' p {font-size:'+Size.ToString+'%;}');
    SL.Add(' .vnum {font-size:80%;font-weight:bold;vertical-align:text-middle;}');
    SL.Add('</style>');
    SL.Add('</head>');
    SL.Add('<body>');
    SL.Add('<p>');

    dtmdlMain.qryVerses.First;
    while not dtmdlMain.qryVerses.Eof do begin
      VerseText := dtmdlMain.qryVersesVERSE_TEXT.AsString;
      // Remove first # and add paragraph
      if VerseText.StartsWith('#') then begin
        SL.Add('</p><p>');
        //VerseText := Copy(VerseText,VerseText.IndexOf('#')+1,VerseText.Length-1);
        VerseText := Copy(VerseText,Pos('#',VerseText)+1,VerseText.Length-1);
        //VerseText := VerseText.Remove(0,1);
      end;
      SL.Add('<span class="vnum">'+dtmdlMain.qryVersesVERSE_NUMBER.AsString+':</span> '+VerseText.Trim);
      dtmdlMain.qryVerses.Next;
    end;

    SL.Add('</p>');
    SL.Add('</body>');

    SL.Text := SL.Text.Replace('[','<I>',[rfReplaceAll]);
    SL.Text := SL.Text.Replace(']','</I>',[rfReplaceAll]);
    // incase of mid verse break
    SL.Text := SL.Text.Replace('#','</p><p>',[rfReplaceAll]);

    SL.SaveToFile(TempFile);
  finally
    SL.Free;
  end;
end;

procedure TfrmMain.btnUpdateFontClick(Sender: TObject);
begin
  PrototypeBindSource1.ApplyUpdates;
  try
    Settings.Save;
  except
  end;
  UpdateVerses(nil);
  tcVerses.SetActiveTabWithTransition(tabVerseText,TTabTransition.Slide);
end;

procedure TfrmMain.btnSettingsClick(Sender: TObject);
begin
  WebBrowser2.Navigate('');
  WebBrowser2.Navigate('file://'+TempFile);
  tcVerses.SetActiveTabWithTransition(tabVerseSettings,TTabTransition.Slide);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.Terminate;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Loaded := False;
  try
    tcBible.ActiveTab := tabBooks;
    tcVerses.ActiveTab := tabVerseText;
    SetVisibleBrowser;

    TempFile := IOUtils.TPath.GetTempPath+PathDelim+'Temp.html';

    dtmdlMain.OpenConnections;

    dmMain.dtmdlMain.OnAfterChapterChange := UpdateVerses;
    if (TOSVersion.Platform = pfAndroid) then
      TBackActionManager.HideBackActionControls(Self,True);
  finally
    Loaded := True;
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  BackAction : TChangeTabAction;
begin
  if (TOSVersion.Platform = pfAndroid) and (Key = vkHardwareBack) then begin
    BackAction := TBackActionManager.FindBackAction(tcBible,True);
    if Assigned(BackAction) then begin
      BackAction.ExecuteTarget(Self);
      Key := 0;
    end;

    if tcBible.ActiveTab = tabBooks then
      Key := 0;
  end;
end;

procedure TfrmMain.lvBooksItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  tcBible.SetActiveTabWithTransition(tabChapters,TTabTransition.Slide);
end;

procedure TfrmMain.lvChaptersItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  UpdateVerses(Sender);
  // Show
  tcBible.SetActiveTabWithTransition(tabVerses,TTabTransition.Slide);
end;

procedure TfrmMain.PrototypeBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  if Settings = nil then begin
    Settings := TSettings.Create;
    try
      Settings.Load;
    except
    end;
  end;
  ABindSourceAdapter := TObjectBindSourceAdapter<TSettings>.Create(Self, Settings, False);
end;

procedure TfrmMain.tbFontNewSizeChange(Sender: TObject);
begin
  UpdateVersesCommon(Trunc(tbFontNewSize.Value));
  if Loaded then begin
    WebBrowser2.Navigate('');
    WebBrowser2.Navigate('file://'+TempFile);
  end;
end;

procedure TfrmMain.tcBibleChange(Sender: TObject);
begin
  SetVisibleBrowser;
end;

procedure TfrmMain.tcVersesChange(Sender: TObject);
begin
  SetVisibleBrowser;
end;

procedure TfrmMain.SetVisibleBrowser;
begin
  WebBrowser1.Visible := (tcBible.ActiveTab = tabVerses) and (tcVerses.ActiveTab = tabVerseText);
  WebBrowser2.Visible := (tcBible.ActiveTab = tabVerses) and (tcVerses.ActiveTab = tabVerseSettings);
end;

end.
