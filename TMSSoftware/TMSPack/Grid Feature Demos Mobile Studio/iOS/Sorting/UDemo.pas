unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData,
  FMX.TMSCustomGrid, FMX.TMSGrid, FMX.TMSXUtil;

type
  TForm1 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TToolBar;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Label1: TLabel;
    StyleBook2: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGrid1GetCellLayout(Sender: TObject; ACol, ARow: Integer;
      ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
    procedure RadioButton1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.IOOffset := Point(1,1);
  XCopyFile(XGetRootDirectory+'CARS.CSV', XGetDocumentsDirectory+'/CARS.CSV');
  TMSFMXGrid1.LoadFromCSV(XGetDocumentsDirectory + '/CARS.CSV');
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

procedure TForm1.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.IsChecked then
    TMSFMXGrid1.Options.Sorting.Mode := gsmNone;

  if RadioButton2.IsChecked then
    TMSFMXGrid1.Options.Sorting.Mode := gsmNormal;

  if RadioButton3.IsChecked then
    TMSFMXGrid1.Options.Sorting.Mode := gsmIndexed;
end;

procedure TForm1.TMSFMXGrid1GetCellLayout(Sender: TObject; ACol, ARow: Integer;
  ALayout: TTMSFMXGridCellLayout; ACellState: TCellState);
begin
  if (ACol = 8) and (ARow > 0) then
  begin
    if tmsfmxgrid1.Cells[acol,arow] = '0' then
    begin
      ALayout.Font.Style := [TFontStyle.fsBold];
      ALayout.Font.Size := 8;
      ALayout.FontFill.Color := TAlphaColorRec.Red;
    end;

    if tmsfmxgrid1.Cells[acol,arow] = '1' then
    begin
      ALayout.Font.Style := [TFontStyle.fsBold, TFontStyle.fsItalic];
      ALayout.Font.Size := 10;
      ALayout.FontFill.Color := TAlphaColorRec.Blue;
    end;

    if tmsfmxgrid1.Cells[acol,arow] = '2' then
    begin
      ALayout.Font.Style := [];
      ALayout.Font.Size := 12;
      ALayout.FontFill.Color := TAlphaColorRec.Green;
    end;

    if tmsfmxgrid1.Cells[acol,arow] = '4' then
    begin
      ALayout.Font.Style := [TFontStyle.fsBold, TFontStyle.fsStrikeOut];
      ALayout.Font.Size := 12;
      ALayout.FontFill.Color := TAlphaColorRec.Orange;
    end;
    ALayout.TextAlign := TTextAlign.taTrailing;
  end;
end;

end.
