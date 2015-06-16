unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridPDFIO, FMX.TMSBitmapContainer,
  FMX.TMSGridCell, FMX.TMSGridData, FMX.TMSCustomGrid, FMX.TMSGrid, FMX.TMSXUtil,
  FMX.TMSQuickPDFRenderLib;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    Button1: TButton;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    TMSFMXGridPDFIO1: TTMSFMXGridPDFIO;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXGrid1GetCellLayout(Sender: TObject; ACol, ARow: Integer;
      ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitGrid;
  end;

var
  Form719: TForm719;
  renderlib: TTMSFMXQuickPDFRenderLib;

implementation

{$R *.fmx}

{ TForm719 }

procedure TForm719.Button1Click(Sender: TObject);
begin
  TMSFMXGridPDFIO1.ExportPDF(XGetDocumentsDirectory + '/pdfexport.pdf');
end;

procedure TForm719.FormCreate(Sender: TObject);
begin
  InitGrid;
end;

procedure TForm719.InitGrid;
begin
  renderlib := TTMSFMXQuickPDFRenderLib.Create(Self);
  TMSFMXGridPDFIO1.PDFRenderLib := renderlib;
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

procedure TForm719.TMSFMXGrid1GetCellLayout(Sender: TObject; ACol,
  ARow: Integer; ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
begin
  if (ACol = 5) then
  begin
    ALayout.FontFill.Color := TAlphaColorRec.Red;
    ALayout.TextAlign := TTextAlign.taTrailing;
  end;

end;

end.
