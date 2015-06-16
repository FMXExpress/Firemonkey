unit Uspellcheckdemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSSpellCheck, FMX.TMSSpellCheckCorrectLinesForm, FMX.Edit, FMX.Objects,
  System.UIConsts, FMX.TMSSpellParser;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    TMSFMXSpellCheckCorrectLinesPanel1: TTMSFMXSpellCheckCorrectLinesPanel;
    TMSFMXSpellCheck1: TTMSFMXSpellCheck;
    TMSFMXSpellCheckCorrectLinesDialog1: TTMSFMXSpellCheckCorrectLinesDialog;
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure Edit4Exit(Sender: TObject);
    procedure TMSFMXSpellCheckCorrectLinesPanel1SpellCheckComplete(
      Sender: TObject; OriginalText, CorrectedText: string);
    procedure Edit3Exit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function ValidSentence(s: string): boolean;
    function LastWord(s: string): string;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Edit1Exit(Sender: TObject);
begin
  if not ValidSentence(Edit1.Text) then
    Edit1.FontColor := claRed
  else
    Edit1.FontColor := claBlack;
end;

procedure TForm2.Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  s: string;
begin
  if keyChar = ' ' then
  begin
    s := LastWord(Edit1.Text);

    if TMSFMXSpellCheck1.Validate(s) <> wvrValidated then
      Edit1.FontColor := claRed
    else
      Edit1.FontColor := claBlack;
  end;
end;

procedure TForm2.Edit2Exit(Sender: TObject);
var
  sl: TStringList;
  i: integer;
  s,res: string;
begin
  sl := TStringList.Create;

  try
    ParseStringToWords(Edit2.Text, sl);

    res := '';

    for i := 0 to sl.Count - 1 do
    begin
      s := TMSFMXSpellCheck1.FirstSuggestion(sl.Strings[i]);

      if i > 0 then
        res := res + ' ';

      if (s <> '') and (s <> sl.Strings[i]) then
        res := res + s
      else
        res := res + sl.Strings[i];
    end;

    Edit2.Text := res;
  finally
    sl.Free;
  end;

end;

procedure TForm2.Edit3Exit(Sender: TObject);
var
  s: string;
begin
  s := Edit3.Text;
  if not ValidSentence(s) then
  begin
    if TMSFMXSpellCheckCorrectLinesDialog1.Execute(Edit3, s) = mrOK then
      Edit3.Text := s;
  end;

end;

procedure TForm2.Edit4Exit(Sender: TObject);
begin
  if not ValidSentence(Edit4.Text) then
  begin
    TMSFMXSpellCheckCorrectLinesPanel1.Init(Edit4.Text);
  end;
end;

function TForm2.LastWord(s: string): string;
var
  i: integer;
  su: string;
begin
  s := Trim(s);
  Result := '';
  for i := Length(s) downto 1 do
  begin
    su := Copy(s, i, 1);
    if su <> ' ' then
      Result := su + Result
    else
      break;
  end;

end;


procedure TForm2.TMSFMXSpellCheckCorrectLinesPanel1SpellCheckComplete(
  Sender: TObject; OriginalText, CorrectedText: string);
begin
  Edit4.Text := TMSFMXSpellCheckCorrectLinesPanel1.Text;
end;

function TForm2.ValidSentence(s: string): boolean;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  Result := true;

  try
    ParseStringToWords(s, sl);

    for i := 0 to sl.Count - 1 do
    begin
      if TMSFMXSpellCheck1.Validate(sl.Strings[i]) = wvrNotValidated then
        Result := false;
    end;
  finally
    sl.Free;
  end;

end;

end.
