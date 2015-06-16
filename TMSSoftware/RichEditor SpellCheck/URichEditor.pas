unit URichEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSRichEditorToolBar, FMX.TMSToolBar, FMX.TMSBaseControl, UIConsts,
  FMX.TMSScrollControl, FMX.TMSRichEditorBase, FMX.TMSRichEditor, IOUtils,
  FMX.Layouts, FMX.ListBox, FMX.TMSSpellCheckCorrectForm, FMX.TMSSpellCheck,
  FMX.TMSRichEditorSpellCheck;

type
  TForm1185 = class(TForm)
    TMSFMXRichEditor1: TTMSFMXRichEditor;
    TMSFMXDockPanel1: TTMSFMXDockPanel;
    TMSFMXRichEditorFormatToolBar1: TTMSFMXRichEditorFormatToolBar;
    TMSFMXRichEditorEditToolBar1: TTMSFMXRichEditorEditToolBar;
    Panel1: TPanel;
    chkSpellCheck: TCheckBox;
    chkAutoCorrect: TCheckBox;
    TMSFMXRichEditorSpellCheckDialog1: TTMSFMXRichEditorSpellCheckDialog;
    TMSFMXRichEditorProofingToolBar1: TTMSFMXRichEditorProofingToolBar;
    TMSFMXRichEditorSpellCheck1: TTMSFMXRichEditorSpellCheck;
    TMSFMXRichEditorSpellCheckPanel1: TTMSFMXRichEditorSpellCheckPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label2: TLabel;
    procedure TMSFMXRichEditor1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXRichEditor1CorrectWord(Sender: TObject; var AWord: string;
      var Error: Boolean);
    procedure TMSFMXRichEditorSpellCheck1RequestsProcessed(Sender: TObject;
      Context: TProcessRequestContext);
    procedure ComboBox1Change(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    {$IF DEFINED(ANDROID) or DEFINED(IOS)}
    procedure TMSFMXRichEditorEditToolBar1OpenFile(Sender: TObject);
    procedure TMSFMXRichEditorEditToolBar1SaveFile(Sender: TObject);
    {$ENDIF}
  private
    { Private declarations }
  public
    { Public declarations }
    function GetPath: String;
    function GetSavePath: String;
    procedure SpellCheckCallback(Sender: TObject; CallBackContext: TTMSFMXSpellCheckCallbackContext);

  end;

var
  Form1185: TForm1185;

implementation

{$R *.fmx}

procedure TForm1185.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
  0: TMSFMXRichEditor1.LoadFromTextFile('spellcheck_english.txt');
  1: TMSFMXRichEditor1.LoadFromTextFile('spellcheck_german.txt');
  2: TMSFMXRichEditor1.LoadFromTextFile('spellcheck_french.txt');
  3: TMSFMXRichEditor1.LoadFromTextFile('spellcheck_dutch.txt');
  end;
end;

procedure TForm1185.FormCreate(Sender: TObject);
begin
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
  TMSFMXRichEditorEditToolBar1.State := esLarge;
  TMSFMXRichEditorFormatToolBar1.State := esLarge;
  TMSFMXRichEditor1.Font.Size := 20;
  TMSFMXRichEditorEditToolBar1.OnOpenFile :=
    TMSFMXRichEditorEditToolBar1OpenFile;
  TMSFMXRichEditorEditToolBar1.OnSaveFile :=
    TMSFMXRichEditorEditToolBar1SaveFile;
{$ENDIF}
end;

{$IF DEFINED(ANDROID) or DEFINED(IOS)}
procedure TForm1185.TMSFMXRichEditorEditToolBar1OpenFile(Sender: TObject);
begin
  TMSFMXRichEditor1.LoadFromFile(GetPath + 'richeditorio.rte');
end;

procedure TForm1185.TMSFMXRichEditorEditToolBar1SaveFile(Sender: TObject);
begin
  TMSFMXRichEditor1.SaveToFile(GetSavePath + 'richeditor_output.rte');
end;
{$ENDIF}

function TForm1185.GetPath: String;
begin
{$IFDEF ANDROID}
  Result := TPath.GetDocumentsPath + PathDelim;
{$ELSE}
  Result := ExtractFilePath(ParamStr(0))
{$ENDIF}
end;

function TForm1185.GetSavePath: String;
begin
  Result := TPath.GetSharedDocumentsPath + PathDelim;
end;

procedure TForm1185.RadioButton1Change(Sender: TObject);
begin
  TMSFMXRichEditorSpellCheckPanel1.Visible := true;
end;

procedure TForm1185.RadioButton2Change(Sender: TObject);
begin
  TMSFMXRichEditorSpellCheckPanel1.Visible := false;

  if TMSFMXRichEditor1.ErrorCount > 0 then
  begin
    TMSFMXRichEditor1.SelectError(esFirst);
    TMSFMXRichEditorSpellCheckDialog1.Execute;
  end;

end;

procedure TForm1185.SpellCheckCallback(Sender: TObject;
  CallBackContext: TTMSFMXSpellCheckCallbackContext);
var
  hl: TSpellCheckHighlight;
begin
  if not CallBackContext.BooleanResult then
  begin
    hl := TSpellCheckHighlight(Callbackcontext.Data);
    TMSFMXRichEditor1.SelStart := hl.SelStart;
    TMSFMXRichEditor1.SelLength := hl.SelLength;
    TMSFMXRichEditor1.SetSelectionError(true);
    TMSFMXRichEditor1.ClearSelection;
  end;

  TSpellCheckHighlight(Callbackcontext.Data).Free;
end;

procedure TForm1185.TMSFMXRichEditor1CorrectWord(Sender: TObject;
  var AWord: string; var Error: Boolean);
var
  s: string;
begin
  if chkSpellcheck.IsChecked and not chkAutocorrect.IsChecked then
  begin
    error := (TMSFMXRichEditorSpellCheck1.Validate(aword) = wvrNotValidated);
  end;

  if chkSpellcheck.IsChecked and chkAutocorrect.IsChecked then
  begin
    s := TMSFMXRichEditorSpellCheck1.FirstSuggestion(aword);
    if (s <> aword) and (s <> '') then
    begin
      aword := s;
      Error := false;
    end;
    if (s = '') then
      Error := true;
  end;

end;

procedure TForm1185.TMSFMXRichEditor1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  s:string;
begin
  s := TMSFMXRichEditor1.WordAtXY(Round(X),Round(Y));
  Caption := 'x='+floattostr(x)+' y='+floattostr(y)+' top='+floattostr(x+ TMSFMXRichEditor1.TopLeft.X) + ' word='+s;
end;

procedure TForm1185.TMSFMXRichEditorSpellCheck1RequestsProcessed(
  Sender: TObject; Context: TProcessRequestContext);
begin
  TMSFMXRichEditor1.SelectError(esFirst);
  TMSFMXRichEditorSpellCheckPanel1.DoUpdate;
end;


end.
