unit UTableView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.TMSNativeNSView, FMX.TMSNativeNSBaseControl,
  FMX.TMSNativeNSTableView, FMX.TMSNativeNSTextField, FMX.TMSNativeNSLabel,
  FMX.TMSNativeNSComboBox, FMX.TMSNativeNSButton, FMX.TMSNativeNSCheckBox,
  FMX.TMSNativeNSStepper, MacApi.AppKit, MacApi.Foundation,
  FMX.TMSNativeNSImageView, FMX.Graphics;

type
  TForm1087 = class(TForm)
    TMSFMXNativeNSTableView1: TTMSFMXNativeNSTableView;
    TMSFMXNativeNSView1: TTMSFMXNativeNSView;
    TMSFMXNativeNSComboBox1: TTMSFMXNativeNSComboBox;
    TMSFMXNativeNSLabel1: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSCheckBox1: TTMSFMXNativeNSCheckBox;
    TMSFMXNativeNSLabel2: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel4: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSCheckBox2: TTMSFMXNativeNSCheckBox;
    TMSFMXNativeNSLabel3: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSComboBox2: TTMSFMXNativeNSComboBox;
    TMSFMXNativeNSTableView2: TTMSFMXNativeNSTableView;
    TMSFMXNativeNSLabel5: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSView2: TTMSFMXNativeNSView;
    TMSFMXNativeNSLabel9: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSComboBox3: TTMSFMXNativeNSComboBox;
    TMSFMXNativeNSLabel6: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSButton1: TTMSFMXNativeNSButton;
    TMSFMXNativeNSView3: TTMSFMXNativeNSView;
    TMSFMXNativeNSImageView2: TTMSFMXNativeNSImageView;
    TMSFMXNativeNSLabel11: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel12: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel13: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel14: TTMSFMXNativeNSLabel;
    procedure TMSFMXNativeNSComboBox1Change(Sender: TObject);
    procedure TMSFMXNativeNSCheckBox1Click(Sender: TObject);
    procedure TMSFMXNativeNSCheckBox2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeNSComboBox2Change(Sender: TObject);
    procedure TMSFMXNativeNSTableView1CellEditingDidBegin(Sender: TObject;
      AColumn, AItem: Integer);
    procedure TMSFMXNativeNSTableView1CellEditingDidChange(Sender: TObject;
      AColumn, AItem: Integer; Value: TValue);
    procedure TMSFMXNativeNSTableView1SelectionDidChange(Sender: TObject);
    procedure TMSFMXNativeNSTableView1CellEditingDidEnd(Sender: TObject;
      AColumn, AItem: Integer);
    procedure TMSFMXNativeNSButton1Click(Sender: TObject);
    procedure TMSFMXNativeNSTableView1ColumnDidMove(Sender: TObject; AOldColumn,
      ANewColumn: Integer);
    procedure TMSFMXNativeNSTableView1ColumnClick(Sender: TObject;
      AColumn: Integer);
    procedure TMSFMXNativeNSTableView1CellEditingCheckboxClick(Sender: TObject;
      AColumn, AItem: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplySelection;
    procedure AddEvent(AEvent: String);
  end;

var
  Form1087: TForm1087;

implementation

{$R *.fmx}

procedure TForm1087.AddEvent(AEvent: String);
begin
  TMSFMXNativeNSTableView2.BeginUpdate;
  TMSFMXNativeNSTableView2.Items.Add.Values.Add.Value := AEvent;
  TMSFMXNativeNSTableView2.EndUpdate;
  TMSFMXNativeNSTableView2.SelectedRow := TMSFMXNativeNSTableView2.Items.Count - 1;
  TMSFMXNativeNSTableView2.ScrollToSelectedRow;
end;

procedure TForm1087.ApplySelection;
var
  it: TTMSFMXNativeNSTableViewItem;
begin
  if TMSFMXNativeNSTableView1.SelectedRow <> -1 then
  begin
    it := TMSFMXNativeNSTableView1.Items[TMSFMXNativeNSTableView1.SelectedRow];
    TMSFMXNativeNSLabel14.Text := 'Product: ' + it.Values[0].Value.AsString;
    TMSFMXNativeNSLabel13.Text := 'Quantity: ' + it.Values[1].Value.AsString;
    TMSFMXNativeNSLabel12.Text := 'Price: ' + it.Values[3].Value.AsString;
    TMSFMXNativeNSLabel11.Text := 'Total Price: ' + it.Values[4].Value.AsString;
    TMSFMXNativeNSImageView2.Bitmap.Assign(TBitmap(it.Values[5].Value.AsObject));
  end;
end;

procedure TForm1087.FormCreate(Sender: TObject);
var
  bmp: TBitmap;
  it: TTMSFMXNativeNSTableViewItem;
  I: Integer;
begin
  TMSFMXNativeNSCheckBox1.Text := '';
  TMSFMXNativeNSCheckBox2.Text := '';

  TMSFMXNativeNSTableView1.BeginUpdate;
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Product';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Quantity';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Available';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Price';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Total Price';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Image';

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Ice cream';
  it.Values.Add.Value := '3';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '20$';
  it.Values.Add.Value := '60$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '0.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Bananas';
  it.Values.Add.Value := '10';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '1$';
  it.Values.Add.Value := '10$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '1.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Coffee';
  it.Values.Add.Value := '5';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '15$';
  it.Values.Add.Value := '75$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '2.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Bread';
  it.Values.Add.Value := '1';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '5$';
  it.Values.Add.Value := '5$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '3.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Butter';
  it.Values.Add.Value := '6';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '1.5$';
  it.Values.Add.Value := '9$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '4.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Strawberries';
  it.Values.Add.Value := '50';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '0.1$';
  it.Values.Add.Value := '5$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '5.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Coconut';
  it.Values.Add.Value := '4';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '0.15$';
  it.Values.Add.Value := '0.6$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '6.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Pineapple';
  it.Values.Add.Value := '1';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '10$';
  it.Values.Add.Value := '10$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '7.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Cherries';
  it.Values.Add.Value := '25';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '1$';
  it.Values.Add.Value := '25$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '8.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Candy';
  it.Values.Add.Value := '9';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '0.1$';
  it.Values.Add.Value := '0.9$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '9.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Cookies';
  it.Values.Add.Value := '2';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '2.5$';
  it.Values.Add.Value := '5$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '10.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Pie';
  it.Values.Add.Value := '15';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '0.35$';
  it.Values.Add.Value := '5.25$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '11.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Milkshake';
  it.Values.Add.Value := '1';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '25$';
  it.Values.Add.Value := '25$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '12.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Chocolate';
  it.Values.Add.Value := '50';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '0.1$';
  it.Values.Add.Value := '5$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '13.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Milk';
  it.Values.Add.Value := '1';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '6$';
  it.Values.Add.Value := '6$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '14.jpg');
  it.Values.Add.Value := bmp;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Oranges';
  it.Values.Add.Value := '3.5';
  it.Values.Add.Value := Boolean(Random(2));
  it.Values.Add.Value := '1.5$';
  it.Values.Add.Value := '5.25$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '15.jpg');
  it.Values.Add.Value := bmp;

  TMSFMXNativeNSTableView1.EndUpdate;

  TMSFMXNativeNSTableView1.SelectedRow := 0;
  ApplySelection;

  TMSFMXNativeNSComboBox3.Editable := False;
  TMSFMXNativeNSComboBox3.Items.Clear;
  for I := 0 to TMSFMXNativeNSTableView1.Columns.Count - 1 do
    TMSFMXNativeNSComboBox3.Items.Add(TMSFMXNativeNSTableView1.Columns[I].Header);
end;

procedure TForm1087.TMSFMXNativeNSButton1Click(Sender: TObject);
begin
  if TMSFMXNativeNSComboBox3.ItemIndex <> -1 then
    TMSFMXNativeNSTableView1.SortColumn(TMSFMXNativeNSComboBox3.ItemIndex, csAscending);
end;

procedure TForm1087.TMSFMXNativeNSCheckBox1Click(Sender: TObject);
begin
  TMSFMXNativeNSTableView1.Options.Appearance.AlternatingRowBackgroundColors := TMSFMXNativeNSCheckBox1.Checked;
end;

procedure TForm1087.TMSFMXNativeNSCheckBox2Click(Sender: TObject);
begin
  TMSFMXNativeNSTableView1.Options.Layout.ColumnHeaders := TMSFMXNativeNSCheckBox2.Checked;
end;

procedure TForm1087.TMSFMXNativeNSComboBox1Change(Sender: TObject);
begin
  TMSFMXNativeNSTableView1.Options.Appearance.SelectionHighlightStyle := TTMSFMXNativeNSTableViewSelectionHighlightStyle(TMSFMXNativeNSComboBox1.ItemIndex);
end;

procedure TForm1087.TMSFMXNativeNSComboBox2Change(Sender: TObject);
begin
  case TMSFMXNativeNSComboBox2.ItemIndex of
  0: TMSFMXNativeNSTableView1.Options.Layout.RowSizeStyle := rsTableViewRowSizeStyleDefault;
  1: TMSFMXNativeNSTableView1.Options.Layout.RowSizeStyle := rsTableViewRowSizeStyleSmall;
  2: TMSFMXNativeNSTableView1.Options.Layout.RowSizeStyle := rsTableViewRowSizeStyleMedium;
  3: TMSFMXNativeNSTableView1.Options.Layout.RowSizeStyle := rsTableViewRowSizeStyleLarge;
  end;
end;

procedure TForm1087.TMSFMXNativeNSTableView1CellEditingCheckboxClick(
  Sender: TObject; AColumn, AItem: Integer);
begin
  AddEvent('Col ' + inttostr(AColumn) + ':' + inttostr(AItem) + ' = ' + BoolToStr(TMSFMXNativeNSTableView1.Items[AItem].Values[AColumn].Value.AsBoolean, True));
end;

procedure TForm1087.TMSFMXNativeNSTableView1CellEditingDidBegin(Sender: TObject;
  AColumn, AItem: Integer);
begin
  ApplySelection;
end;

procedure TForm1087.TMSFMXNativeNSTableView1CellEditingDidChange(
  Sender: TObject; AColumn, AItem: Integer; Value: TValue);
begin
  AddEvent('Col ' + inttostr(AColumn) + ':' + inttostr(AItem) + ' = ' + TMSFMXNativeNSTableView1.Items[AItem].Values[AColumn].Value.AsString);
  ApplySelection;
end;

procedure TForm1087.TMSFMXNativeNSTableView1CellEditingDidEnd(Sender: TObject;
  AColumn, AItem: Integer);
begin
  ApplySelection;
end;

procedure TForm1087.TMSFMXNativeNSTableView1ColumnClick(Sender: TObject;
  AColumn: Integer);
begin
  AddEvent('Column : ' + inttostr(AColumn) + ' Clicked');
end;

procedure TForm1087.TMSFMXNativeNSTableView1ColumnDidMove(Sender: TObject;
  AOldColumn, ANewColumn: Integer);
begin
  AddEvent('Column : ' + inttostr(AOldColumn) + ' to ' + inttostr(ANewColumn));
end;

procedure TForm1087.TMSFMXNativeNSTableView1SelectionDidChange(Sender: TObject);
begin
  AddEvent('Selection : ' + inttostr(TMSFMXNativeNSTableView1.SelectedRow));
  ApplySelection;
end;

end.
