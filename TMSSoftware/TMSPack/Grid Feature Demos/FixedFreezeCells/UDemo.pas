unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.Edit, FMX.TMSGridCell,
  FMX.TMSCustomGrid, FMX.StdCtrls;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitGrid;
  end;

var
  Form719: TForm719;

implementation

{$R *.fmx}

procedure TForm719.CheckBox1Change(Sender: TObject);
begin
  if checkbox1.IsChecked then
  begin
    if TMSFMXGrid1.FixedRightColumns = 0 then
      TMSFMXGrid1.ColumnCount := TMSFMXGrid1.ColumnCount + 1;

    TMSFMXGrid1.FixedRightColumns := 1
  end
  else
  begin
    if TMSFMXGrid1.FixedRightColumns = 1 then
      TMSFMXGrid1.ColumnCount := TMSFMXGrid1.ColumnCount - 1;
    TMSFMXGrid1.FixedRightColumns := 0;
  end;

end;

procedure TForm719.CheckBox2Change(Sender: TObject);
begin
  if checkbox2.IsChecked then
  begin
    if TMSFMXGrid1.FixedFooterRows = 0 then
      TMSFMXGrid1.RowCount := TMSFMXGrid1.RowCount + 1;

    TMSFMXGrid1.FixedFooterRows := 1
  end
  else
  begin
    if TMSFMXGrid1.FixedFooterRows = 1 then
      TMSFMXGrid1.RowCount := TMSFMXGrid1.RowCount - 1;
    TMSFMXGrid1.FixedFooterRows := 0;
  end;

end;

procedure TForm719.CheckBox3Change(Sender: TObject);
begin
  if checkbox3.IsChecked then
  begin
    TMSFMXGrid1.FreezeColumns := 2;
  end
  else
  begin
    TMSFMXGrid1.FreezeColumns := 0;
  end;

end;

procedure TForm719.CheckBox4Change(Sender: TObject);
begin
  if checkbox4.IsChecked then
  begin
    TMSFMXGrid1.FreezeRows := 2;
  end
  else
  begin
    TMSFMXGrid1.FreezeRows := 0;
  end;
end;

procedure TForm719.FormCreate(Sender: TObject);
begin

  InitGrid;

end;

procedure TForm719.InitGrid;
begin
  TMSFMXGrid1.IOOffset := Point(1,1);
  TMSFMXGrid1.ColumnCount := 50;
  TMSFMXGrid1.RowCount := 115;
  TMSFMXGrid1.LinearFill(false);
  TMSFMXGrid1.LoadFromCSV('..\..\cars.csv');
  TMSFMXGrid1.ColumnWidths[1] := 100;
  TMSFMXGrid1.AutoNumberCol(0);
  TMSFMXGrid1.SelectionMode := smCellRange;

  TMSFMXGrid1.Cells[1,0] := 'Brand';
  TMSFMXGrid1.Cells[2,0] := 'Type';
  TMSFMXGrid1.Cells[3,0] := 'CC';
  TMSFMXGrid1.Cells[4,0] := 'Hp';
  TMSFMXGrid1.Cells[5,0] := 'Cyl';
  TMSFMXGrid1.Cells[6,0] := 'Kw';
  TMSFMXGrid1.Cells[7,0] := 'Price';
  TMSFMXGrid1.Cells[8,0] := 'Country';
end;

procedure TForm719.RadioButton1Change(Sender: TObject);
begin
  if radiobutton1.IsChecked then
    TMSFMXGrid1.ScrollMode := smCellScrolling
  else
    TMSFMXGrid1.ScrollMode := smPixelScrolling;

end;

end.
