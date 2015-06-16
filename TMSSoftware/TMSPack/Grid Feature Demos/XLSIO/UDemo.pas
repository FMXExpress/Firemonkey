unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.GridExcelIO,
  FMX.TMSBitmapContainer, FMX.TMSGridCell, FMX.TMSXUtil, UIConsts,
  FMX.TMSCustomGrid, FMX.StdCtrls;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    Button1: TButton;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    TMSFMXGridExcelIO1: TTMSFMXGridExcelIO;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

implementation

{$IFDEF WIN32}
uses
  ShellAPI,Windows;
{$ENDIF}

{$R *.fmx}

{ TForm719 }

procedure TForm719.Button1Click(Sender: TObject);
begin
  TMSFMXGridExcelIO1.XLSExport('..\..\gridexport.xls');
  XOpenFile('open', '..\..\gridexport.xls', '', '');
end;

procedure TForm719.Button2Click(Sender: TObject);
begin
  TMSFMXGridExcelIO1.XLSImport('..\..\cars.xls');
end;

procedure TForm719.FormCreate(Sender: TObject);
begin
  TMSFMXGridExcelIO1.Options.ImportCellProperties := true;
  TMSFMXGridExcelIO1.Options.ExportCellProperties := true;
  TMSFMXGridExcelIO1.Options.ExportOverwrite := omAlways;
  TMSFMXGridExcelIO1.Options.ExportImages := true;

  InitGrid;
end;

procedure TForm719.InitGrid;
begin
  TMSFMXGrid1.ColumnCount := 6;
  TMSFMXGrid1.RandomFill;
  TMSFMXGrid1.AutoNumberCol(0);
  TMSFMXGrid1.Colors[2,2] := claRed;
  TMSFMXGrid1.Colors[3,3] := claLime;
  TMSFMXGrid1.Colors[4,4] := claYellow;

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
    ALayout.FontFill.Color := claRed;
    ALayout.TextAlign := TTextAlign.taTrailing;
  end;

end;

end.
