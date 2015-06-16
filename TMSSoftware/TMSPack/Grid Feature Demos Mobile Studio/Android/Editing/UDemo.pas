unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX.TMSXUtil,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.Edit, FMX.ListBox,
  FMX.ExtCtrls, FMX.Colors, FMX.Menus, FMX.TMSGridCell, FMX.TMSCustomGrid,
  FMX.StdCtrls, IOUtils;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXGrid1CellEditSetData(Sender: TObject; ACol, ARow: Integer;
      CellEditor: TFmxObject; var CellString: string);
    procedure TMSFMXGrid1GetCellClass(Sender: TObject; ACol, ARow: Integer;
      var CellClassType: TFmxObjectClass);
    procedure TMSFMXGrid1GetCellEditorProperties(Sender: TObject; ACol,
      ARow: Integer; CellEditor: TFmxObject);
    procedure TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol, ARow: Integer;
      var CellEditorType: TTMSFMXGridEditorType);
    procedure TMSFMXGrid1GetCellEditorCustomClassType(Sender: TObject; ACol,
      ARow: Integer; var CellEditorCustomClassType: TFmxObjectClass);
    procedure TMSFMXGrid1CellEditGetData(Sender: TObject; ACol, ARow: Integer;
      CellEditor: TFmxObject; var CellString: string);
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
var
  I: Integer;
begin
  TMSFMXGrid1.IOOffset := Point(1, 1);
  TMSFMXGrid1.LoadFromCSV(GetHomePath + '/CARS.CSV');
  TMSFMXGrid1.DefaultColumnWidth := 100;
  TMSFMXGrid1.DefaultRowHeight := 30;

  TMSFMXGrid1.Cells[1, 0] := 'ComboBox';
  TMSFMXGrid1.Cells[2, 0] := 'Edit';
  TMSFMXGrid1.Cells[3, 0] := 'TrackBar';
  TMSFMXGrid1.Cells[4, 0] := 'DatePicker';
  TMSFMXGrid1.Cells[5, 0] := 'ArcDial';
  TMSFMXGrid1.Cells[6, 0] := 'ColorPicker';
  TMSFMXGrid1.Cells[7, 0] := 'Switch';
  TMSFMXGrid1.Cells[8, 0] := 'Pasword';

  TMSFMXGrid1.CellComboBox.Items.Add('Mercedes');
  TMSFMXGrid1.CellComboBox.Items.Add('Audi');
  TMSFMXGrid1.CellComboBox.Items.Add('Bugatti');
  TMSFMXGrid1.CellComboBox.Items.Add('Alfa Romeo');
  TMSFMXGrid1.CellComboBox.Items.Add('Jaguar');
  TMSFMXGrid1.CellComboBox.Items.Add('BMW');

  TMSFMXGrid1.AutoNumberCol(0);

  for I := 1 to TMSFMXGrid1.RowCount - 1 do
  begin
    TMSFMXGrid1.Cells[7, I] := '0';
    TMSFMXGrid1.Cells[4, I] := DateToStr(Now - 100 + Random(200));
  end;

  TMSFMXGrid1.Cells[7, 2] := '1';
  TMSFMXGrid1.Cells[7, 5] := '1';
  TMSFMXGrid1.Cells[7, 7] := '1';
  TMSFMXGrid1.Cells[7, 10] := '1';

  TMSFMXGrid1.SetFocus;
end;

procedure TForm719.TMSFMXGrid1CellEditGetData(Sender: TObject; ACol,
  ARow: Integer; CellEditor: TFmxObject; var CellString: string);
begin
  if CellEditor is TSwitch then
    (CellEditor as TSwitch).IsChecked := StrToBool(TMSFMXGrid1.Cells[ACol, ARow]);
end;

procedure TForm719.TMSFMXGrid1CellEditSetData(Sender: TObject; ACol,
  ARow: Integer; CellEditor: TFmxObject; var CellString: string);
begin
  case ACol of
    3: CellString := floattostr((CellEditor as TTrackBar).Value);
    4: CellString := DateToStr((CellEditor as TCellCalendarBox).Date);
    5: CellString := floattostr((CellEditor as TArcDial).Value);
    6: TMSFMXGrid1.Colors[ACol, ARow] := (CellEditor as TColorComboBox).Color;
    7:
    begin
      if (CellEditor as TSwitch).IsChecked then
        CellString := '1'
      else
        CellString := '0';
    end;
  end;
end;

procedure TForm719.TMSFMXGrid1GetCellClass(Sender: TObject; ACol, ARow: Integer;
  var CellClassType: TFmxObjectClass);
begin
  if ARow = 7 then
    CellClassType := TTMSFMXFixedGridCell;
end;

procedure TForm719.TMSFMXGrid1GetCellEditorCustomClassType(Sender: TObject;
  ACol, ARow: Integer; var CellEditorCustomClassType: TFmxObjectClass);
begin
  if ACol = 7 then
    CellEditorCustomClassType := TSwitch;
end;

procedure TForm719.TMSFMXGrid1GetCellEditorProperties(Sender: TObject; ACol,
  ARow: Integer; CellEditor: TFmxObject);
begin
  if (CellEditor is TEdit) then
    (CellEditor as TEdit).Password := Acol = 8;
end;

procedure TForm719.TMSFMXGrid1GetCellEditorType(Sender: TObject; ACol,
  ARow: Integer; var CellEditorType: TTMSFMXGridEditorType);
begin
  case ACol of
    1: CellEditorType := etComboBox;
    3: CellEditorType := etTrackBar;
    4: CellEditorType := etDatePicker;
    5: CellEditorType := etArcDial;
    6: CellEditorType := etColorComboBox;
    7: CellEditorType := etCustom;
  end;
end;

end.
