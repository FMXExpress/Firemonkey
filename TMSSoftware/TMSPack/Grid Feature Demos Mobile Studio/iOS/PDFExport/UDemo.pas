unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData,
  FMX.TMSCustomGrid, FMX.TMSGrid, FMX.TMSBitmapContainer, FMX.TMSGridPDFIO, FMX.TMSXUtil;

type
  TForm25 = class(TForm)
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    TMSFMXGrid1: TTMSFMXGrid;
    ToolBar1: TToolBar;
    Button1: TButton;
    StyleBook2: TStyleBook;
    TMSFMXGridPDFIO1: TTMSFMXGridPDFIO;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGrid1GetCellLayout(Sender: TObject; ACol, ARow: Integer;
      ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

{$R *.fmx}

procedure TForm25.Button1Click(Sender: TObject);
var
  fn: String;
begin
  fn := XGetDocumentsDirectory + '/pdfexport.pdf';
  TMSFMXGridPDFIO1.ExportPDF(fn);
  if FileExists(fn) then
    XOpenFile(fn);
end;

procedure TForm25.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.Options.Printing.PrintCellBackGround := cbFull;
  TMSFMXGrid1.ColumnCount := 6;
  TMSFMXGrid1.RandomFill;
  TMSFMXGrid1.AutoNumberCol(0);
  TMSFMXGrid1.Colors[2,2] := TAlphaColorRec.Red;
  TMSFMXGrid1.Colors[3,3] := TAlphaColorRec.Lime;
  TMSFMXGrid1.Colors[4,4] := TAlphaColorRec.Yellow;

  TMSFMXGrid1.HorzAlignments[2,3] := TTextAlign.taCenter;
  TMSFMXGrid1.HorzAlignments[3,4] := TTextAlign.taTrailing;

  TMSFMXGrid1.FontStyles[1,1] := [TFontStyle.fsBold];
  TMSFMXGrid1.FontStyles[1,2] := [TFontStyle.fsItalic];

  TMSFMXGrid1.MergeCells(1,4,2,2);

  TMSFMXGrid1.AddBitmap(1,7,'1');
  TMSFMXGrid1.AddBitmap(1,8,'2');
  TMSFMXGrid1.AddBitmap(1,9,'3');
end;

procedure TForm25.TMSFMXGrid1GetCellLayout(Sender: TObject; ACol, ARow: Integer;
  ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
begin
  if (ACol = 5) then
  begin
    ALayout.FontFill.Color := TAlphaColorRec.Red;
    ALayout.TextAlign := TTextAlign.taTrailing;
  end;
end;

end.
