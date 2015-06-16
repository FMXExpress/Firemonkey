unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl, FMX.TMSGridCell,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.TMSPrintPreview, FMX.TMSXUtil,
  FMX.TMSCustomGrid, FMX.StdCtrls;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    TMSFMXGridPrintPreviewDialog1: TTMSFMXGridPrintPreviewDialog;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGrid1GetCellClass(Sender: TObject; ACol, ARow: Integer;
      var CellClassType: TFmxObjectClass);
    procedure TMSFMXGrid1GetCellProperties(Sender: TObject; ACol, ARow: Integer;
      Cell: TFmxObject);
    procedure TMSFMXGrid1CellAnchorClick(Sender: TObject; ACol, ARow: Integer;
      AAnchor: string);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form719: TForm719;

implementation

{$R *.fmx}

procedure TForm719.Button1Click(Sender: TObject);
begin
  TMSFMXGridPrintPreviewDialog1.Execute;
end;

procedure TForm719.Button2Click(Sender: TObject);
begin
  TMSFMXGrid1.PrintPageToImage('..\..\PrintImage.png');
  XOpenFile('open', '..\..\PrintImage.png', '', '');
end;

procedure TForm719.FormCreate(Sender: TObject);
var
  I: Integer;
  K: Integer;
begin
   TMSFMXGrid1.LoadFromCSV(XGetRootDirectory + 'cars.csv');

// As there are issues in the FireMonkey framework to reliably get the printer DPI
// it might be required to programmatically force it.
// TMSFMXGrid1.Options.Printing.DPI := Point(600, 600);

  TMSFMXGrid1.DefaultRowHeight := 30;
  TMSFMXGrid1.DefaultColumnWidth := 100;

  TMSFMXGrid1.ScrollMode := smPixelScrolling;

  TMSFMXGrid1.AutoNumberCol(0);
  TMSFMXGrid1.AutoNumberRow(0);
  TMSFMXGrid1.AddCheckBoxColumn(2);

  TMSFMXGrid1.MergeCells(3, 5, 3, 2);
  TMSFMXGrid1.MergeCells(2, 10, 1, 4);

  for I := 0 to TMSFMXGrid1.RowCount - 1 do
    TMSFMXGrid1.AddBitmapFile(3, I, XGetRootDirectory +   {$IFNDEF MACOS}'images\img ('{$ELSE}'img ('{$ENDIF}+inttostr(Random(10) + 1)+').gif');

  TMSFMXGrid1.ColumnWidths[7] := 200;

  for I := 0 to TMSFMXGrid1.ColumnCount - 1 do
  begin
    if I <> 6 then
    begin
      for K := 0 to TMSFMXGrid1.RowCount - 1 do
      begin
        if I = 7 then
          TMSFMXGrid1.Cells[I, K] := '<b><i>Cell</i></b><font size="18" color="clared">'+inttostr(I)+':'+inttostr(K)+'</font> <a href="test">Click !</a>';
      end;
    end;
  end;

  TMSFMXGridPrintPreviewDialog1.Grid := TMSFMXGrid1;
end;

procedure TForm719.TMSFMXGrid1CellAnchorClick(Sender: TObject; ACol,
  ARow: Integer; AAnchor: string);
begin
  ShowMessage('Anchor in cell ' + inttostr(acol)+'.'+inttostr(arow)+ ' clicked.');
end;

procedure TForm719.TMSFMXGrid1GetCellClass(Sender: TObject; ACol, ARow: Integer;
  var CellClassType: TFmxObjectClass);
begin
  if ACol = 6 then
    CellClassType := TTMSFMXProgressGridCell
  else if ACol = 4 then
    CellClassType := TTMSFMXButtonGridCell;
end;

procedure TForm719.TMSFMXGrid1GetCellProperties(Sender: TObject; ACol,
  ARow: Integer; Cell: TFmxObject);
begin
  if Cell is TTMSFMXProgressGridCell then
  begin
    (cell as TTMSFMXProgressGridCell).ProgressBar.Max := TMSFMXGrid1.RowCount;
    (cell as TTMSFMXProgressGridCell).ProgressBar.Value := TMSFMXGrid1.RowCount - ARow;
  end;

  if Cell is TTMSFMXButtonGridCell then
  begin
    (cell as TTMSFMXButtonGridCell).Button.Text := 'X';

  end;

end;

end.
