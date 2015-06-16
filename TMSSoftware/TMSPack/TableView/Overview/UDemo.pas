unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes,
  Variants, FMX.Forms, FMX.Types, FMX.TMSBaseControl, FMX.TMSTableView,
  FMX.TMSBitmapContainer, FMX.Objects, FMX.TMSBitmap,  FMX.Controls,
  FMX.TMSBarButton, FMX.TMSPopup, FMX.ListBox, FMX.StdCtrls;

type
  TForm536 = class(TForm)
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    Label1: TLabel;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    ComboBox2: TComboBox;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    CheckBox2: TCheckBox;
    Label4: TLabel;
    ComboBox3: TComboBox;
    Label5: TLabel;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    Label6: TLabel;
    CheckBox3: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    CheckBox4: TCheckBox;
    TMSFMXTableView1: TTMSFMXTableView;
    TMSFMXTableView2: TTMSFMXTableView;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    CheckBox5: TCheckBox;
    Label11: TLabel;
    CheckBox6: TCheckBox;
    Label12: TLabel;
    Image1: TImage;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    TMSFMXBarButton1: TTMSFMXBarButton;
    TMSFMXTableView3: TTMSFMXTableView;
    TMSFMXBarButton2: TTMSFMXBarButton;
    TMSFMXPopup1: TTMSFMXPopup;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXTableView1ItemCustomize(Sender: TObject;
      AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
      AItemControlShape: TControl);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure TMSFMXTableView1ApplyStyleLookup(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure TMSFMXTableView1ItemBeforeDetail(Sender: TObject;
      AItem: TTMSFMXTableViewItem);
    procedure TMSFMXTableView1ItemAfterReturnDetail(Sender: TObject;
      AItem: TTMSFMXTableViewItem);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure TMSFMXBarButton1Click(Sender: TObject);
    procedure TMSFMXTableView2ItemSelected(Sender: TObject;
      AItem: TTMSFMXTableViewItem);
    procedure FormDestroy(Sender: TObject);
    procedure TMSFMXTableView2ItemCustomize(Sender: TObject;
      AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
      AItemControlShape: TControl);
    procedure TMSFMXTableView2MarkClick(Sender: TObject);
    procedure TMSFMXTableView2ItemDelete(Sender: TObject;
      AItem: TTMSFMXTableViewItem; var AllowDelete: Boolean);
    procedure TMSFMXBarButton2Click(Sender: TObject);
    procedure TMSFMXTableView3ItemCustomize(Sender: TObject;
      AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
      AItemControlShape: TControl);
    procedure TMSFMXTableView3ItemSelected(Sender: TObject;
      AItem: TTMSFMXTableViewItem);
  private
    { Private declarations }
  public
    { Public declarations }
    function FindItemOnList(AItem: TTMSFMXTableViewItem; ID: Integer): TObject;
  end;

  TItem = class(TPersistent)
  private
    FId: Integer;
  public
    property ID: Integer read FId write FId;
  end;

var
  Form536: TForm536;


implementation

{$R *.fmx}

procedure TForm536.CheckBox1Change(Sender: TObject);
begin
  TMSFMXTableView1.LookupBar := CheckBox1.IsChecked;
end;

procedure TForm536.CheckBox2Change(Sender: TObject);
begin
  TMSFMXTableView1.ShowFilter := CheckBox2.IsChecked;
end;

procedure TForm536.CheckBox3Change(Sender: TObject);
begin
  TMSFMXTableView1.AutoFilter := CheckBox3.IsChecked;
end;

procedure TForm536.CheckBox4Change(Sender: TObject);
begin
  TMSFMXTableView2.EditButton := CheckBox4.IsChecked;
end;

procedure TForm536.CheckBox5Change(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TMSFMXTableView2.Items.Count - 1 do
    TMSFMXTableView2.Items[I].CanEditCaption := CheckBox5.IsChecked;
end;

procedure TForm536.CheckBox6Change(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TMSFMXTableView2.Items.Count - 1 do
    TMSFMXTableView2.Items[I].CanEditDescription := CheckBox6.IsChecked;
end;

procedure TForm536.ComboBox1Change(Sender: TObject);
begin
  TMSFMXTableView1.LayoutMode := TTMSFMXTableViewLayoutMode(ComboBox1.ItemIndex);
end;

procedure TForm536.ComboBox2Change(Sender: TObject);
begin
  case TMSFMXTableView1.CategoryType of
    ctAlphaBetic: TMSFMXTableView1.GetLookupBar.Width := 45;
    ctCustom: TMSFMXTableView1.GetLookupBar.Width := 25;
  end;
  TMSFMXTableView1.CategoryType := TTMSFMXTableViewCategoryType(ComboBox2.ItemIndex);
end;

procedure TForm536.ComboBox3Change(Sender: TObject);
begin
  TMSFMXTableView1.Filtering := TTMSFMXTableViewFiltering(ComboBox3.ItemIndex);
end;

function TForm536.FindItemOnList(AItem: TTMSFMXTableViewItem;
  ID: Integer): TObject;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(AItem.DataObject) then
  begin
    for I := 0 to (AItem.DataObject as TList).Count - 1 do
    begin
      if (TObject((AItem.DataObject as TList).Items[I]) as TItem).ID = ID then
      begin
        Result := TObject((AItem.DataObject as TList).Items[I]);
        Break;
      end;
    end;
  end;
end;

procedure TForm536.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  GroupBox2.Enabled := False;
  TMSFMXTableView1.BeginUpdate;
  TMSFMXTableView1.BitmapContainer := TMSFMXBitmapContainer1;
  TMSFMXTableView1.ItemOptions :=  [ioLeftRectangle, ioCenterRectangle, ioBulbRectangle, ioDetailRectangle, ioCaptionElement];
  TMSFMXTableView1.Items.Clear;
  TMSFMXTableView1.CategoryType := ctCustom;
  TMSFMXTableView1.LookupBar := False;
  with TMSFMXTableView1.Categories.Add do
  begin
    Caption := 'Yellow Fruit';
    LookupText := 'Yellow';
  end;
  with TMSFMXTableView1.Categories.Add do
  begin
    Caption := 'Green Fruit';
    LookupText := 'Green';
  end;
  with TMSFMXTableView1.Categories.Add do
  begin
    Caption := 'Blue Fruit';
    LookupText := 'Blue';
  end;
  with TMSFMXTableView1.Categories.Add do
  begin
    Caption := 'Red Fruit';
    LookupText := 'Red';
  end;
  with TMSFMXTableView1.Categories.Add do
  begin
    Caption := 'Orange Fruit';
    LookupText := 'Orange';
  end;
  TMSFMXTableView1.Categories.Sort;
  for I := 0 to TMSFMXBitmapContainer1.Items.Count - 1 do
  begin
    with TMSFMXTableView1.Items.Add do
    begin
      CategoryID := TMSFMXBitmapContainer1.Items[I].Tag;
      Caption := TMSFMXBitmapContainer1.Items[I].Name;
      BitmapName := Caption;
      if (I = 3) or (I = 2) or (I = 7) then
      begin
        DetailView := TMSFMXTableView2;
        Randomize;
        BulbText := inttostr(Random(10) + 5);
        DataObject := TList.Create;
      end;
    end;
  end;

  TMSFMXTableView1.Items.Sort;
  TMSFMXTableView1.EndUpdate;

  TMSFMXTableView2.ItemOptions := TMSFMXTableView2.ItemOptions + [ioRightRectangle];
end;

procedure TForm536.FormDestroy(Sender: TObject);
var
  i, k: integer;
begin
  for I := 0 to TMSFMXTableView1.Items.Count - 1 do
  begin
    if Assigned(TMSFMXTableView1.Items[I].DataObject) then
    begin
      for K := (TMSFMXTableView1.Items[I].DataObject as TList).Count - 1 downto 0 do
        TObject(((TMSFMXTableView1.Items[I].DataObject as TList).Items[K])).Free;

      TMSFMXTableView1.Items[I].DataObject.Free;
    end;
  end;
end;

procedure TForm536.TMSFMXBarButton1Click(Sender: TObject);
var
  it: TTMSFMXTableViewItem;
  shpr: TRectangle;
  item: TItem;
begin
  it := TMSFMXTableView2.SelectedItem;
  if Assigned(it) then
  begin
    if Assigned((TMSFMXTableView1.SelectedItem.DataObject as TList)) then
    begin
      if (TMSFMXTableView1.SelectedItem.DataObject as TList).IndexOf(it.DataObject) = -1 then
      begin
        item := TItem.Create;
        item.ID := it.Index;
        (TMSFMXTableView1.SelectedItem.DataObject as TList).Add(item);
        it.DataObject := item;
      end;

      shpr := it.ShapeRightRectangle;
      if Assigned(shpr) then
        shpr.Visible := True;
    end;
  end;
end;

procedure TForm536.TMSFMXBarButton2Click(Sender: TObject);
begin
  TMSFMXPopup1.ShowPopup(Sender as TControl, False, TPlacement.plTopCenter);
end;

procedure TForm536.TMSFMXTableView1ApplyStyleLookup(Sender: TObject);
begin
  TMSFMXTableView1.GetLookupBar.Width := 45;
end;

procedure TForm536.TMSFMXTableView1ItemAfterReturnDetail(Sender: TObject;
  AItem: TTMSFMXTableViewItem);
begin
  GroupBox1.Enabled := True;
  GroupBox2.Enabled := False;
  TMSFMXBarButton2.Text := 'Quantity';
end;

procedure TForm536.TMSFMXTableView1ItemBeforeDetail(Sender: TObject;
  AItem: TTMSFMXTableViewItem);
var
  cnt, i, c: Integer;
begin
  GroupBox1.Enabled := False;
  GroupBox2.Enabled := True;
  TMSFMXTableView2.BeginUpdate;
  TMSFMXTableView2.Items.Clear;
  cnt := 0;
  if AItem.BulbText <> '' then
    cnt := strtoint(AItem.BulbText);

  c := Random(10) + 1;
  TMSFMXTableView2.HeaderText := 'Prices of ' + AItem.Caption;
  for I := 0 to cnt - 1 do
  begin
    with TMSFMXTableView2.Items.Add do
    begin
      Caption := inttostr((I + 1) * c) + ' kg';
      Description := 'Price : € ' + floattostr(((I + 1) * c) * 1.25);
      CanEditCaption := CheckBox5.IsChecked;
      CanEditDescription := CheckBox6.IsChecked;
      DataObject := FindItemOnList(AItem, I);
    end;
  end;

  TMSFMXTableView2.EndUpdate;
end;

procedure TForm536.TMSFMXTableView1ItemCustomize(Sender: TObject;
  AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
  AItemControlShape: TControl);
begin
  AItem.ShapeCaption.Align := TAlignLayout.alClient;
  AItem.ShapeLeftRectangle.Visible := True;
end;

procedure TForm536.TMSFMXTableView2ItemCustomize(Sender: TObject;
  AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
  AItemControlShape: TControl);
var
  shpr: TRectangle;
begin
  shpr := AItem.ShapeRightRectangle;
  if Assigned(shpr) then
    shpr.Visible := Assigned(AItem.DataObject);
end;

procedure TForm536.TMSFMXTableView2ItemDelete(Sender: TObject;
  AItem: TTMSFMXTableViewItem; var AllowDelete: Boolean);
var
  shpr: TRectangle;
  i: integer;
  it: TObject;
begin
  if Assigned(AItem.DataObject) then
  begin
    i := (TMSFMXTableView1.SelectedItem.DataObject as TList).IndexOf(AItem.DataObject);
    it := TObject((TMSFMXTableView1.SelectedItem.DataObject as TList).Items[i]);
    (TMSFMXTableView1.SelectedItem.DataObject as TList).Remove(it);
    it.Free;
    AItem.DataObject := nil;
    shpr := AItem.ShapeRightRectangle;
    if Assigned(shpr) then
      shpr.Visible := Assigned(AItem.DataObject);
  end;
end;

procedure TForm536.TMSFMXTableView2ItemSelected(Sender: TObject;
  AItem: TTMSFMXTableViewItem);
begin
  Label16.Text := TMSFMXTableView1.SelectedItem.Caption;
  label17.Text := AItem.Caption;
  Label18.Text := AItem.Description;
  TMSFMXBarButton1.Enabled := True;
  TMSFMXBarButton2.Enabled := True;
end;

procedure TForm536.TMSFMXTableView2MarkClick(Sender: TObject);
var
  it: TTMSFMXTableViewItem;
  shpr: TRectangle;
  item: TItem;
  I: Integer;
begin
  for I := 0 to TMSFMXTableView2.Items.Count - 1 do
  begin
    it := TMSFMXTableView2.Items[I];
    if Assigned(it) and (it.Selected) then
    begin
      if Assigned((TMSFMXTableView1.SelectedItem.DataObject as TList)) then
      begin
        if (TMSFMXTableView1.SelectedItem.DataObject as TList).IndexOf(it.DataObject) = -1 then
        begin
          item := TItem.Create;
          item.ID := it.Index;
          (TMSFMXTableView1.SelectedItem.DataObject as TList).Add(item);
          it.DataObject := item;
        end;

        shpr := it.ShapeRightRectangle;
        if Assigned(shpr) then
          shpr.Visible := True;
      end;
    end;
  end;
end;

procedure TForm536.TMSFMXTableView3ItemCustomize(Sender: TObject;
  AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
  AItemControlShape: TControl);
begin
  if Odd(AItem.Index) then
  begin
    AItemShape.Fill.Color := TAlphaColorRec.Lightgray;
    AItemShape.DescriptionColor := TAlphaColorRec.Slategray;
  end;
end;

procedure TForm536.TMSFMXTableView3ItemSelected(Sender: TObject;
  AItem: TTMSFMXTableViewItem);
begin
  TMSFMXBarButton2.Text := 'Quantity (' + AItem.Caption + ')';
  TMSFMXPopup1.HidePopup;
end;

end.


