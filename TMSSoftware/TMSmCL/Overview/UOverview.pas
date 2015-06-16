unit UOverview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.TMSNativeNSTextField, FMX.TMSNativeNSLabel,
  FMX.TMSNativeNSTableView, FMX.TMSNativeNSTabView, FMX.TMSNativeNSBaseControl,
  FMX.TMSNativeNSImageView, FMX.TMSNativeNSButton, FMX.TMSNativeNSCheckBox,
  FMX.TMSNativeNSTextView, FMX.TMSNativeNSToolBar,
  FMX.TMSNativeNSProgressIndicator, FMX.TMSNativeNSView, FMX.TMSNativeNSPopover,
  FMX.TMSNativeNSStepper, FMX.TMSNativeNSComboBox, FMX.TMSNativeNSSlider,
  FMX.TMSNativeNSDatePicker, FMX.TMSNativeNSLevelIndicator,
  FMX.TMSNativeNSRadioButton
  {$if compilerversion > 25}
  ,FMX.Graphics
  {$endif}
  ;

type
  TForm1087 = class(TForm)
    TMSFMXNativeNSTabView1: TTMSFMXNativeNSTabView;
    TMSFMXNativeNSTabViewItem1: TTMSFMXNativeNSTabViewItem;
    TMSFMXNativeNSTableView1: TTMSFMXNativeNSTableView;
    TMSFMXNativeNSLabel1: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSImageView1: TTMSFMXNativeNSImageView;
    TMSFMXNativeNSLabel2: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel3: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel5: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel6: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSTextView1: TTMSFMXNativeNSTextView;
    TMSFMXNativeNSToolbar1: TTMSFMXNativeNSToolbar;
    TMSFMXNativeNSTabViewItem2: TTMSFMXNativeNSTabViewItem;
    TMSFMXNativeNSPopover1: TTMSFMXNativeNSPopover;
    TMSFMXNativeNSButton1: TTMSFMXNativeNSButton;
    TMSFMXNativeNSView1: TTMSFMXNativeNSView;
    TMSFMXNativeNSLabel4: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSLabel7: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSTextField1: TTMSFMXNativeNSTextField;
    TMSFMXNativeNSTextField2: TTMSFMXNativeNSTextField;
    TMSFMXNativeNSButton2: TTMSFMXNativeNSButton;
    TMSFMXNativeNSLevelIndicator1: TTMSFMXNativeNSLevelIndicator;
    TMSFMXNativeNSDatePicker1: TTMSFMXNativeNSDatePicker;
    TMSFMXNativeNSDatePicker2: TTMSFMXNativeNSDatePicker;
    TMSFMXNativeNSSlider1: TTMSFMXNativeNSSlider;
    TMSFMXNativeNSSlider3: TTMSFMXNativeNSSlider;
    TMSFMXNativeNSComboBox1: TTMSFMXNativeNSComboBox;
    TMSFMXNativeNSStepper1: TTMSFMXNativeNSStepper;
    TMSFMXNativeNSImageView2: TTMSFMXNativeNSImageView;
    TMSFMXNativeNSImageView3: TTMSFMXNativeNSImageView;
    TMSFMXNativeNSImageView4: TTMSFMXNativeNSImageView;
    TMSFMXNativeNSImageView5: TTMSFMXNativeNSImageView;
    TMSFMXNativeNSImageView6: TTMSFMXNativeNSImageView;
    TMSFMXNativeNSLabel8: TTMSFMXNativeNSLabel;
    TMSFMXNativeNSTableView2: TTMSFMXNativeNSTableView;
    TMSFMXNativeNSDatePicker3: TTMSFMXNativeNSDatePicker;
    TMSFMXNativeNSLabel9: TTMSFMXNativeNSLabel;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeNSTableView1SelectionDidChange(Sender: TObject);
    procedure TMSFMXNativeNSToolbar1ItemClick(ASender: TObject;
      AItem: TTMSFMXNativeNSToolbarItem);
    procedure TMSFMXNativeNSButton2Click(Sender: TObject);
    procedure TMSFMXNativeNSButton1Click(Sender: TObject);
    procedure TMSFMXNativeNSStepper1ValueChanged(ASender: TObject;
      AValue: Single);
    procedure TMSFMXNativeNSDatePicker2DateChanged(ASender: TObject;
      ADateTime: TDateTime);
    procedure TMSFMXNativeNSTableView2GetCellTextColor(Sender: TObject; AColumn,
      AItem: Integer; var ATextColor: TAlphaColor);
    procedure TMSFMXNativeNSSlider1ValueChanged(ASender: TObject;
      AValue: Single);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplySelection;
  end;

var
  Form1087: TForm1087;

implementation

{$R *.fmx}

procedure TForm1087.ApplySelection;
var
  it: TTMSFMXNativeNSTableViewItem;
begin
  if TMSFMXNativeNSTableView1.SelectedRow <> -1 then
  begin
    it := TMSFMXNativeNSTableView1.Items[TMSFMXNativeNSTableView1.SelectedRow];
    TMSFMXNativeNSLabel2.Text := 'Product: ' + it.Values[0].Value.AsString;
    TMSFMXNativeNSLabel3.Text := 'Quantity: ' + it.Values[1].Value.AsString;
    TMSFMXNativeNSLabel5.Text := 'Price: ' + it.Values[3].Value.AsString;
    TMSFMXNativeNSLabel6.Text := 'Total Price: ' + it.Values[4].Value.AsString;
    TMSFMXNativeNSTextView1.Text := it.DataString;
    TMSFMXNativeNSImageView1.BitmapFile := ExtractFilePath(ParamStr(0)) + inttostr(it.Index) + '.jpg';
  end;
end;

procedure TForm1087.FormCreate(Sender: TObject);
var
  it: TTMSFMXNativeNSTableViewItem;
  I: Integer;
  bmp: TBitmap;
begin
  TMSFMXNativeNSTableView1.BeginUpdate;
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Product';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Quantity';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Available';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Price';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Total Price';
  TMSFMXNativeNSTableView1.Columns.Add.Header := 'Image';
  TMSFMXNativeNSTableView1.Options.Appearance.SelectionHighlightStyle := hsTableViewSelectionHighlightStyleSourceList;
  TMSFMXNativeNSTableView1.Options.Interaction.EmptySelection := False;
  TMSFMXNativeNSTableView1.Options.Interaction.ColumnReordering := False;
  for I := 0 to TMSFMXNativeNSTableView1.Columns.Count - 1 do
  begin
    TMSFMXNativeNSTableView1.Columns[I].ReadOnly := True;
    TMSFMXNativeNSTableView1.Columns[I].MinimumWidth := 50;
  end;

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Ice cream';
  it.Values.Add.Value := '3';
  it.Values.Add.Value := true;
  it.Values.Add.Value := '20$';
  it.Values.Add.Value := '60$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '0.jpg');
  it.Values.Add.Value := bmp;
  it.DataString := 'Ice cream (derived from earlier iced cream or cream ice[1]) is a frozen dessert usually made from dairy products, such as milk and cream and often combined with '+
  'fruits or other ingredients and flavours. Most varieties contain sugar, although some are made with other sweeteners. In some cases, artificial flavourings and colourings are used '+
  'in addition to, or instead of, the natural ingredients';

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Bananas';
  it.Values.Add.Value := '10';
  it.Values.Add.Value := false;
  it.Values.Add.Value := '1$';
  it.Values.Add.Value := '10$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '1.jpg');
  it.Values.Add.Value := bmp;
  it.DataString := 'A banana is an edible fruit produced by several kinds of large herbaceous flowering plants of the genus Musa.[1] (In some countries, bananas used for cooking may '+
  'be called plantains.) The fruit is variable in size, color and firmness, but is usually elongated and curved, with soft flesh rich in starch covered with a rind which may be green,'+
  ' yellow, red, purple, or brown when ripe';

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Coffee';
  it.Values.Add.Value := '5';
  it.Values.Add.Value := false;
  it.Values.Add.Value := '15$';
  it.Values.Add.Value := '75$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '2.jpg');
  it.Values.Add.Value := bmp;
  it.DataString := 'Coffee is a brewed beverage prepared from the roasted seeds of several species of an evergreen shrub of the genus Coffea. The two most common sources of coffee beans'+
  ' are the highly regarded Coffea arabica, and the "robusta" form of the hardier Coffea canephora. The latter is resistant to the coffee leaf rust (Hemileia vastatrix), but has a more bitter taste.';

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Bread';
  it.Values.Add.Value := '1';
  it.Values.Add.Value := true;
  it.Values.Add.Value := '5$';
  it.Values.Add.Value := '5$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '3.jpg');
  it.Values.Add.Value := bmp;
  it.DataString := 'Bread is a staple food prepared by baking a dough of flour and water. It is popular around the world and is one of the world''s oldest foods.';

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Butter';
  it.Values.Add.Value := '6';
  it.Values.Add.Value := true;
  it.Values.Add.Value := '1.5$';
  it.Values.Add.Value := '9$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '4.jpg');
  it.Values.Add.Value := bmp;
  it.DataString := 'Butter is a dairy product made by churning fresh or fermented cream or milk. It is generally used as '+
  'a spread and a condiment, as well as in cooking, such as baking, sauce making, and pan frying. Butter consists of butterfat, milk proteins and water.';

  it := TMSFMXNativeNSTableView1.Items.Add;
  it.Values.Add.Value := 'Strawberries';
  it.Values.Add.Value := '50';
  it.Values.Add.Value := true;
  it.Values.Add.Value := '0.1$';
  it.Values.Add.Value := '5$';
  bmp := TBitmap.Create(0, 0);
  bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + '5.jpg');
  it.Values.Add.Value := bmp;
  it.DataString := 'Fragaria × ananassa, commonly known as strawberry or garden strawberry, is a hybrid species that is '+
  'cultivated worldwide for its fruit. The fruit (which is not a botanical berry, but an aggregate accessory fruit) is widely appreciated '+
  'for its characteristic aroma, bright red color, juicy texture, and sweetness.';

  TMSFMXNativeNSTableView1.Options.Appearance.AlternatingRowBackgroundColors := True;
  TMSFMXNativeNSTableView1.Options.Layout.ColumnAutoresizingStyle := asTableViewUniformColumnAutoresizingStyle;
  TMSFMXNativeNSTableView1.EndUpdate;
  TMSFMXNativeNSTableView1.SelectedRow := 0;
  ApplySelection;
end;

procedure TForm1087.TMSFMXNativeNSButton1Click(Sender: TObject);
begin
  TMSFMXNativeNSPopover1.ShowFromControl(TMSFMXNativeNSButton1);
end;

procedure TForm1087.TMSFMXNativeNSButton2Click(Sender: TObject);
begin
  TMSFMXNativeNSPopover1.Hide;
end;

procedure TForm1087.TMSFMXNativeNSDatePicker2DateChanged(ASender: TObject;
  ADateTime: TDateTime);
begin
  TMSFMXNativeNSLabel8.Text := DateTimeToStr(ADateTime);
end;

procedure TForm1087.TMSFMXNativeNSSlider1ValueChanged(ASender: TObject;
  AValue: Single);
begin
  TMSFMXNativeNSLabel9.Text := Format('%.2f', [AValue]);
end;

procedure TForm1087.TMSFMXNativeNSStepper1ValueChanged(ASender: TObject;
  AValue: Single);
begin
  TMSFMXNativeNSImageView2.Visible := AValue >= 1;
  TMSFMXNativeNSImageView3.Visible := AValue >= 2;
  TMSFMXNativeNSImageView4.Visible := AValue >= 3;
  TMSFMXNativeNSImageView5.Visible := AValue >= 4;
  TMSFMXNativeNSImageView6.Visible := AValue >= 5;
end;

procedure TForm1087.TMSFMXNativeNSTableView1SelectionDidChange(Sender: TObject);
begin
  ApplySelection;
end;

procedure TForm1087.TMSFMXNativeNSTableView2GetCellTextColor(Sender: TObject;
  AColumn, AItem: Integer; var ATextColor: TAlphaColor);
begin
  case AItem of
  0: ATextColor := TAlphaColorRec.Red;
  1: ATextColor := TAlphaColorRec.Blue;
  2: ATextColor := TAlphaColorRec.Green;
  3: ATextColor := TAlphaColorRec.Orange;
  4: ATextColor := TAlphaColorRec.Purple;
  end;
end;

procedure TForm1087.TMSFMXNativeNSToolbar1ItemClick(ASender: TObject;
  AItem: TTMSFMXNativeNSToolbarItem);
begin
  ShowMessage(AItem.Text);
end;

end.
