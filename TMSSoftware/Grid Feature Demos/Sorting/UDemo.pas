unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.TMSGridCell, UIConsts,
  FMX.TMSCustomGrid, FMX.StdCtrls;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure TMSFMXGrid1GetCellLayout(Sender: TObject; ACol, ARow: Integer;
      ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form719: TForm719;

implementation

{$R *.fmx}

procedure TForm719.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.IOOffset := Point(1,1);
  TMSFMXGrid1.LoadFromCSV('..\..\CARS.csv');
  TMSFMXGrid1.Cells[1,0] := 'Brand';
  TMSFMXGrid1.Cells[2,0] := 'Type';
  TMSFMXGrid1.Cells[3,0] := 'CC';
  TMSFMXGrid1.Cells[4,0] := 'Hp';
  TMSFMXGrid1.Cells[5,0] := 'Cyl';
  TMSFMXGrid1.Cells[6,0] := 'Kw';
  TMSFMXGrid1.Cells[7,0] := 'Price';
  TMSFMXGrid1.Cells[8,0] := 'Country';
  TMSFMXGrid1.Options.Mouse.FixedColumnSizing := true;

  TMSFMXGrid1.Cells[0,0] := '<b>TMS</b><font color="clablue"><i>grid</i></font>!';
end;

procedure TForm719.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.IsChecked then
    TMSFMXGrid1.Options.Sorting.Mode := gsmNone;

  if RadioButton2.IsChecked then
    TMSFMXGrid1.Options.Sorting.Mode := gsmNormal;

  if RadioButton3.IsChecked then
    TMSFMXGrid1.Options.Sorting.Mode := gsmIndexed;
end;

procedure TForm719.TMSFMXGrid1GetCellLayout(Sender: TObject; ACol,
  ARow: Integer; ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
begin
  if (ACol = 8) and (ARow > 0) then
  begin
    if tmsfmxgrid1.Cells[acol,arow] = '0' then
    begin
      ALayout.Font.Style := [TFontStyle.fsBold];
      ALayout.Font.Size := 8;
      ALayout.FontFill.Color := claRed;
    end;

    if tmsfmxgrid1.Cells[acol,arow] = '1' then
    begin
      ALayout.Font.Style := [TFontStyle.fsBold, TFontStyle.fsItalic];
      ALayout.Font.Size := 10;
      ALayout.FontFill.Color := claBlue;
    end;

    if tmsfmxgrid1.Cells[acol,arow] = '2' then
    begin
      ALayout.Font.Style := [];
      ALayout.Font.Size := 12;
      ALayout.FontFill.Color := claGreen;
    end;

    if tmsfmxgrid1.Cells[acol,arow] = '4' then
    begin
      ALayout.Font.Style := [TFontStyle.fsBold, TFontStyle.fsStrikeOut];
      ALayout.Font.Size := 12;
      ALayout.FontFill.Color := claOrange;
    end;
    ALayout.TextAlign := TTextAlign.taTrailing;
  end;
end;

end.

