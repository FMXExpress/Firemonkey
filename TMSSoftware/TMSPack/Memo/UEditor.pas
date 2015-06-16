unit UEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBitmapContainer,
  FMX.TMSSpeedButton, FMX.TMSBaseControl, FMX.TMSMemo, FMX.Objects,
  FMX.TMSMemoStyles, FMX.Layouts, FMX.ListBox, FMX.Menus, FMX.TMSMemoDialogs,
  FMX.Edit, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    PasteBtn: TTMSFMXSpeedButton;
    CopyBtn: TTMSFMXSpeedButton;
    CutBtn: TTMSFMXSpeedButton;
    ToolBar1: TToolBar;
    UndoBtn: TTMSFMXSpeedButton;
    RedoBtn: TTMSFMXSpeedButton;
    TMSFMXMemoPascalStyler1: TTMSFMXMemoPascalStyler;
    TMSFMXMemo1: TTMSFMXMemo;
    Panel1: TPanel;
    TMSFMXMemoHTMLStyler1: TTMSFMXMemoHTMLStyler;
    TMSFMXMemoCSharpStyler1: TTMSFMXMemoCSharpStyler;
    OpenDialog1: TOpenDialog;
    OpenBtn: TTMSFMXSpeedButton;
    SaveBtn: TTMSFMXSpeedButton;
    SaveDialog1: TSaveDialog;
    BookBtn: TTMSFMXSpeedButton;
    Label1: TLabel;
    TMSFMXMemoFindDialog1: TTMSFMXMemoFindDialog;
    TMSFMXSpeedButton1: TTMSFMXSpeedButton;
    Edit1: TEdit;
    TMSFMXSpeedButton2: TTMSFMXSpeedButton;
    StyleBook1: TStyleBook;
    procedure CopyBtnClick(Sender: TObject);
    procedure CutBtnClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure UndoBtnClick(Sender: TObject);
    procedure RedoBtnClick(Sender: TObject);
    procedure TMSFMXMemo1SelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXMemo1UndoChange(Sender: TObject; CanUndo, CanRedo: Boolean);
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure TMSFMXMemo1GutterClick(Sender: TObject; LineNo: Integer);
    procedure BookBtnClick(Sender: TObject);
    procedure TMSFMXSpeedButton1Click(Sender: TObject);
    procedure TMSFMXSpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateSelection;
    procedure UpdateUndo;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.TMSFMXMemo1GutterClick(Sender: TObject; LineNo: Integer);
begin
  TMSFMXMemo1.BreakPoint[LineNo] := not TMSFMXMemo1.BreakPoint[LineNo];
end;

procedure TForm1.TMSFMXMemo1SelectionChange(Sender: TObject);
begin
  UpdateSelection;
end;

procedure TForm1.TMSFMXMemo1UndoChange(Sender: TObject; CanUndo,
  CanRedo: Boolean);
begin
  UpdateUndo;
end;

procedure TForm1.TMSFMXSpeedButton1Click(Sender: TObject);
begin
   TMSFMXMemoFindDialog1.Execute;
end;

procedure TForm1.TMSFMXSpeedButton2Click(Sender: TObject);
begin
  TMSFMXMemo1.HighlightText := Edit1.Text;
end;

procedure TForm1.BookBtnClick(Sender: TObject);
begin
  TMSFMXMemo1.Bookmark[TMSFMXMemo1.CurY] := not TMSFMXMemo1.Bookmark[TMSFMXMemo1.CurY];
end;

procedure TForm1.CopyBtnClick(Sender: TObject);
begin
  TMSFMXMemo1.CopyToClipboard;
end;

procedure TForm1.CutBtnClick(Sender: TObject);
begin
  TMSFMXMemo1.CutToClipboard;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateSelection;
  UpdateUndo;
end;

procedure TForm1.OpenBtnClick(Sender: TObject);
var
  fn: string;
begin
  if OpenDialog1.Execute then
  begin
    fn := OpenDialog1.FileName;
    TMSFMXMemo1.Lines.LoadFromFile(fn);
    if pos('.PAS', UpperCase(fn)) > 0 then
     TMSFMXMemo1.SyntaxStyles := TMSFMXMemoPascalStyler1;

    if pos('.HTM', UpperCase(fn)) > 0 then
     TMSFMXMemo1.SyntaxStyles := TMSFMXMemoHTMLStyler1;

    if pos('.CS', UpperCase(fn)) > 0 then
     TMSFMXMemo1.SyntaxStyles := TMSFMXMemoCSharpStyler1;
  end;
end;

procedure TForm1.UndoBtnClick(Sender: TObject);
begin
  TMSFMXMemo1.Undo;
end;

procedure TForm1.PasteBtnClick(Sender: TObject);
begin
  TMSFMXMemo1.PasteFromClipboard;
end;

procedure TForm1.RedoBtnClick(Sender: TObject);
begin
  TMSFMXMemo1.Redo;
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    TMSFMXMemo1.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.UpdateSelection;
begin
  CutBtn.Enabled := TMSFMXMemo1.CanCut;
  CopyBtn.Enabled := TMSFMXMemo1.CanCopy;
end;

procedure TForm1.UpdateUndo;
begin
  UndoBtn.Enabled := TMSFMXMemo1.CanUndo;
  RedoBtn.Enabled := TMSFMXMemo1.CanRedo;
end;

end.
