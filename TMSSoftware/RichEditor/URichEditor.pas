unit URichEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSRichEditorToolBar, FMX.TMSToolBar, FMX.TMSBaseControl, UIConsts,
  FMX.TMSScrollControl, FMX.TMSRichEditorBase, FMX.TMSRichEditor, IOUtils,
  FMX.Layouts, FMX.ListBox;

type
  TForm1185 = class(TForm)
    TMSFMXRichEditor1: TTMSFMXRichEditor;
    TMSFMXDockPanel1: TTMSFMXDockPanel;
    TMSFMXRichEditorFormatToolBar1: TTMSFMXRichEditorFormatToolBar;
    TMSFMXRichEditorEditToolBar1: TTMSFMXRichEditorEditToolBar;
    Panel1: TPanel;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure TMSFMXRichEditor1DrawGraphic(Sender: TObject; ACanvas: TCanvas;
      ARect: TRectF; AID: string);
    procedure TMSFMXRichEditor1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
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
    procedure SampleIndent;
    procedure SampleFile;
    procedure SampleBullet;
    procedure SampleAlignment;
    procedure SampleFormat;
    procedure SampleImages;
    procedure SampleManyLines;
    procedure SampleClear;
    procedure SimpleTest;
    procedure TwoLines;
    procedure MultipleSpaces;
    procedure Verylongword;
    procedure ManyEmptyLines;
    procedure Longlines;
    procedure WrappedText;
    procedure Helloworld;
  end;

var
  Form1185: TForm1185;

implementation

{$R *.fmx}

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

procedure TForm1185.Helloworld;
begin
  TMSFMXRichEditor1.AddText(' Hello world message with some long line that should wrap to the next line here in the tms rich editor control');
  TMSFMXRichEditor1.AddLineBreak;
end;

procedure TForm1185.ListBox1Click(Sender: TObject);
begin
  TMSFMXRichEditor1.Clear;

  case ListBox1.ItemIndex of
  0: SampleFile;
  1: SampleIndent;
  2: SampleBullet;
  3: SampleAlignment;
  4: SampleFormat;
  5: SampleManyLines;
  6: SampleImages;
  7: SampleClear;
  end;

  if TMSFMXRichEditor1.Enabled then
    TMSFMXRichEditor1.SetFocus;
end;

procedure TForm1185.Longlines;
begin
    TMSFMXRichEditor1.BeginUpdate;
    TMSFMXRichEditor1.AddText('1234567890aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab');
    TMSFMXRichEditor1.AddLineBreak;
    TMSFMXRichEditor1.AddText('abc');
    TMSFMXRichEditor1.SelectAll;
    TMSFMXRichEditor1.SetSelectionIndent(300);
    TMSFMXRichEditor1.EndUpdate;
end;

procedure TForm1185.ManyEmptyLines;
var
  i: integer;
begin
  for i := 0 to 50 do
    TMSFMXRichEditor1.AddLineBreak;
end;

procedure TForm1185.MultipleSpaces;
begin
  TMSFMXRichEditor1.AddText('Hello world                           editor');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Second line');
  TMSFMXRichEditor1.AddLineBreak;
end;

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

procedure TForm1185.SampleAlignment;
var
  s: string;
  i,l: integer;
begin
  s := 'Left justified line';
  i := 0;

  TMSFMXRichEditor1.AddText(s);
  TMSFMXRichEditor1.AddLineBreak;

  l := Length(s);
  TMSFMXRichEditor1.SelectText(0,i + l);

  i := i + l + 2;

  TMSFMXRichEditor1.SetSelectionAttribute(taLeftJustify);

  s := 'Centered line';

  TMSFMXRichEditor1.AddText(s);
  TMSFMXRichEditor1.AddLineBreak;

  l := length(s);

  TMSFMXRichEditor1.SelectText(i,i + l);

  TMSFMXRichEditor1.SetSelectionAttribute(taCenter);

  i := i + l + 2;

  s := 'Right justified line';

  l := length(s);

  TMSFMXRichEditor1.AddText(s);
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.SelectText(i, i + l);

  TMSFMXRichEditor1.SetSelectionAttribute(taRightJustify);

  TMSFMXRichEditor1.ClearSelection;
  TMSFMXRichEditor1.ClearCaret;
end;

procedure TForm1185.SampleBullet;
begin
  TMSFMXRichEditor1.AddText('First bullet type:');
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.AddText('Item 1');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Item 2');
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.SelectText(20,16);
  TMSFMXRichEditor1.SetSelectionBullets(btCircle);

  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Second bullet type:');
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.AddText('Item 1');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Item 2');
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.SelectText(60,16);
  TMSFMXRichEditor1.SetSelectionBullets(btStar);

  TMSFMXRichEditor1.ClearSelection;
  TMSFMXRichEditor1.ClearCaret;
end;

procedure TForm1185.SampleClear;
begin
  TMSFMXRichEditor1.Clear;
end;

procedure TForm1185.SampleFile;
begin
  TMSFMXRichEditor1.LoadFromTextFile(GetPath + 'bellogallico.txt')
end;

procedure TForm1185.SampleFormat;
begin
  TMSFMXRichEditor1.AddText('This is sample formatted text with bold, italic, underline, strikethrough');

  TMSFMXRichEditor1.SelectText(35,4);
  TMSFMXRichEditor1.SetSelectionBold(true);

  TMSFMXRichEditor1.SelectText(41,6);
  TMSFMXRichEditor1.SetSelectionItalic(true);

  TMSFMXRichEditor1.SelectText(49,9);
  TMSFMXRichEditor1.SetSelectionUnderline(true);

  TMSFMXRichEditor1.SelectText(60,13);
  TMSFMXRichEditor1.SetSelectionStrikeOut(true);

  TMSFMXRichEditor1.SelectText(15,9);
  TMSFMXRichEditor1.SetSelectionColor(claGreen);

  TMSFMXRichEditor1.SelectText(25,4);
  TMSFMXRichEditor1.SetSelectionColor(claRed);
  TMSFMXRichEditor1.SetSelectionBkColor(claYellow);

  TMSFMXRichEditor1.ClearSelection;
  TMSFMXRichEditor1.ClearCaret;
end;

procedure TForm1185.SampleImages;
begin
  TMSFMXRichEditor1.AddText('Car (PNG)');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddImage(GetPath + 'car.png');
  TMSFMXRichEditor1.AddLineBreak;

  {$IFDEF MSWINDOWS}
  TMSFMXRichEditor1.AddText('Grammophone (ICO)');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddImage(GetPath + 'grammo.ico');
  TMSFMXRichEditor1.AddLineBreak;
  {$ENDIF}

  TMSFMXRichEditor1.AddText('Toothpaste (BMP)');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddImage(GetPath + 'toothpaste.bmp');
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.AddText('Beach (JPEG)');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddImage(GetPath + 'beach.jpg');
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.AddText('GIFImage (GIF)');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddImage(GetPath + 'gifimage.gif');
  TMSFMXRichEditor1.AddLineBreak;
end;

procedure TForm1185.SampleIndent;
begin
  TMSFMXRichEditor1.AddText('First indent');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('First indent');
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.SelectText(0,24);
  TMSFMXRichEditor1.SetSelectionIndent(50);

  TMSFMXRichEditor1.AddText('Second larger indent');
  TMSFMXRichEditor1.SelectText(26,20);
  TMSFMXRichEditor1.SetSelectionIndent(150);

  TMSFMXRichEditor1.ClearSelection;
  TMSFMXRichEditor1.ClearCaret;
end;

procedure TForm1185.SampleManyLines;
var
  i: integer;
begin
  TMSFMXRichEditor1.BeginUpdate;
  for i := 0 to 400 do
    begin
      TMSFMXRichEditor1.AddText('This is line nr. '+inttostr(i)+' in the rich text editor');
      TMSFMXRichEditor1.AddLineBreak;
    end;
  TMSFMXRichEditor1.EndUpdate;
end;

procedure TForm1185.SimpleTest;
begin
  TMSFMXRichEditor1.AddText('Hello world editor');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('First line');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Second line');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Third line');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Fourth line');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Extra line');
end;

procedure TForm1185.TMSFMXRichEditor1DrawGraphic(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; AID: string);
begin
  ACanvas.Fill.Color := clYellow;
  ACanvas.Stroke.Color := claRed;
  ACanvas.FillRect(ARect, 0, 0, AllCorners, 1);
  ACanvas.DrawRect(ARect, 0, 0, AllCorners, 1);
end;

procedure TForm1185.TMSFMXRichEditor1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  s:string;
begin
  s := TMSFMXRichEditor1.WordAtXY(Round(X),Round(Y));
  Caption := 'x='+floattostr(x)+' y='+floattostr(y)+' top='+floattostr(x+ TMSFMXRichEditor1.TopLeft.X) + ' word='+s;
end;

procedure TForm1185.TwoLines;
begin
  TMSFMXRichEditor1.BeginUpdate;
  TMSFMXRichEditor1.AddText('Hello world 1');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText('Hello world 2');
  TMSFMXRichEditor1.AddLineBreak;
//  TMSFMXRichEditor1.AddText('Line3');
//  TMSFMXRichEditor1.AddLineBreak;
//  TMSFMXRichEditor1.AddText('Line4');
//  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.SelectAll;
//  TMSFMXRichEditor1.SetSelectionIndent(650);
  TMSFMXRichEditor1.EndUpdate;
end;

procedure TForm1185.Verylongword;
begin
  TMSFMXRichEditor1.AddText('12');
  TMSFMXRichEditor1.AddText('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab');
end;

procedure TForm1185.WrappedText;
var
  i: integer;
begin
  TMSFMXRichEditor1.BeginUpdate;
  TMSFMXRichEditor1.AddText('Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. '
  + 'Duis aute irure dolor in '
  +'reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.');

  for i := 0 to 40 do
  TMSFMXRichEditor1.AddLineBreak;

  TMSFMXRichEditor1.EndUpdate;
(*
  TMSFMXRichEditor1.BeginUpdate;
  TMSFMXRichEditor1.AddText(
     'Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry''s standard'
     +' dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.');
  TMSFMXRichEditor1.AddLineBreak;
  TMSFMXRichEditor1.AddText(
     ' It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged.'
     +' It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently'
     +' with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.');
  TMSFMXRichEditor1.EndUpdate;
*)
end;

end.
