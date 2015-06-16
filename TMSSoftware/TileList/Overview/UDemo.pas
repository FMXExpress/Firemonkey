unit UDemo;

interface

uses
  FMX.Forms, FMX.Types, SysUtils, Types, UITypes, Classes, Variants, FMX.Objects, FMX.TMSBaseControl,
  FMX.TMSTileList, FMX.TMSBitmapContainer, FMX.Controls, FMX.ListBox, FMX.TMSBitmap,
  FMX.Layouts, FMX.Dialogs, FMX.Effects, FMX.ExtCtrls, FMX.TabControl, strutils,
  FMX.Edit, FMX.TMSHTMLText, FMX.StdCtrls, FMX.Ani, FMX.Controls.Presentation
  {$if CompilerVersion > 25}
  ,FMX.Graphics
  ,FMX.DateTimeCtrls
  {$ifend}
  {$if CompilerVersion > 27}
  ,FMX.Calendar
  {$ifend}
  ;

type
  TForm633 = class(TForm)
    Panel1: TPanel;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    TMSFMXTileList1: TTMSFMXTileList;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    Button1: TButton;
    Panel2: TPanel;
    ListBox1: TListBox;
    Calendar1: TCalendar;
    Panel3: TPanel;
    ComboBox2: TComboBox;
    Label1: TLabel;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TMSFMXTileList2: TTMSFMXTileList;
    Panel4: TPanel;
    ComboBox3: TComboBox;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ComboBox4: TComboBox;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ComboBox5: TComboBox;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    Label2: TLabel;
    Edit1: TEdit;
    ComboBox6: TComboBox;
    Label3: TLabel;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxItem17: TListBoxItem;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem21: TListBoxItem;
    ListBoxItem22: TListBoxItem;
    ListBoxItem23: TListBoxItem;
    ComboBox7: TComboBox;
    ListBoxItem24: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    ListBoxItem26: TListBoxItem;
    ListBoxItem27: TListBoxItem;
    ListBoxItem28: TListBoxItem;
    ListBoxItem29: TListBoxItem;
    ListBoxItem30: TListBoxItem;
    ListBoxItem31: TListBoxItem;
    ListBoxItem32: TListBoxItem;
    ListBoxItem33: TListBoxItem;
    ListBoxItem34: TListBoxItem;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StyleBook1: TStyleBook;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXTileList1TileHideDetail(Sender: TObject;
      ATile: TTMSFMXTile);
    procedure TMSFMXTileList1LoadTile(Sender: TObject; ATile: TTMSFMXTile;
      ATileShape: TControl);
    procedure TMSFMXTileList1UnLoadTile(Sender: TObject; ATile: TTMSFMXTile;
      ATileShape: TControl);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TMSFMXTileList2Filter(Sender: TObject; var FilterKey: string;
      var Allow: Boolean);
    procedure TMSFMXTileList2TileClick(Sender: TObject; ATile: TTMSFMXTile);
    procedure Edit1ChangeTracking(Sender: TObject);
    procedure TMSFMXTileList2FilterFinished(Sender: TObject;
      OldFilterKey: string; var NewFilterKey: string; var Allow: Boolean);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure TMSFMXTileList1ApplyStyleLookup(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form633: TForm633;
  FUpdateStyle, DisableLookup: Boolean;

implementation

{$R *.fmx}

procedure TForm633.Button1Click(Sender: TObject);
begin
  if Assigned(ListBox1.Selected) then
    ShowMessage(ListBox1.Selected.Text);
end;

procedure TForm633.CheckBox1Change(Sender: TObject);
var
  h: TControl;
begin
  TMSFMXTileList1.BeginUpdate;
  h := TMSFMXTileList1.GetHeader;
  if Assigned(h) then
    h.Visible := CheckBox1.IsChecked;
  TMSFMXTileList1.EndUpdate;
end;

procedure TForm633.CheckBox2Change(Sender: TObject);
var
  h: TControl;
begin
  TMSFMXTileList1.BeginUpdate;
  h := TMSFMXTileList1.GetFooter;
  if Assigned(h) then
    h.Visible := CheckBox2.IsChecked;
  TMSFMXTileList1.EndUpdate;
end;

procedure TForm633.ComboBox1Change(Sender: TObject);
begin
  TMSFMXTileList1.BeginUpdate;
  TMSFMXTileList1.NavigationMode := TTMSFMXTileListNavigationMode(ComboBox1.ItemIndex);
  TMSFMXTileList1.EndUpdate;
end;

procedure AddFilesFromFolder(AFolder: String; ATileList: TTMSFMXTileList);
var
  SR: TSearchRec;
  filext: String;

  procedure AddToList(s: string);
  begin
    with ATileList.Tiles.Add do
    begin
      filext := ExtractFileExt(s);
      filext := Copy(filext, 2, Length(filext));
      BitmapName := filext+'.png';
      Caption := ExtractFileName(s);
      DataString := s;
    end;
  end;

begin
  if FindFirst(AFolder,faAnyFile-faDirectory,SR) = 0 then
  begin
    AddToList(ExtractFilePath(AFolder) + SR.Name);
    while FindNext(SR) = 0 do
      AddToList(ExtractFilePath(AFolder) + SR.Name);
  end;
  FindClose(SR);
end;


procedure TForm633.ComboBox2Change(Sender: TObject);
var
  c: TRectangle;
begin
  TMSFMXTileList1.BeginUpdate;
  c := self.StyleBook1.Style.FindStyleResource('TMSFMXTileList1Style1') as TRectangle;
  if Assigned(c) then
  begin
    {$if CompilerVersion > 26}
    c.Fill.Kind := TBrushKind.Bitmap;
    c.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    {$else}
    c.Fill.Kind := TBrushKind.bkBitmap;
    c.Fill.Bitmap.WrapMode := TWrapMode.wmTileStretch;
    {$ifend}
    case ComboBox2.ItemIndex of
      {$if CompilerVersion > 26}
      0:c.Fill.Kind := TBrushKind.Solid;
      {$else}
      0:c.Fill.Kind := TBrushKind.bkSolid;
      {$ifend}
      1, 2, 3:c.Fill.Bitmap.Bitmap.Assign(TMSFMXBitmapContainer1.FindBitmap('background'+inttostr(ComboBox2.ItemIndex)+'.png'));
    end;
  end;
  TMSFMXTileList1.EndUpdate;
end;

procedure TForm633.ComboBox3Change(Sender: TObject);
begin
  TMSFMXTileList2.KeyBoardMode := TTMSFMXTileListKeyBoardMode(ComboBox3.ItemIndex);
end;

procedure TForm633.ComboBox4Change(Sender: TObject);
begin
  TMSFMXTileList2.FilterMode := TTMSFMXTileListFilterMode(ComboBox4.ItemIndex);
end;

procedure TForm633.ComboBox5Change(Sender: TObject);
begin
  TMSFMXTileList2.Filtering := TTMSFMXTileListFiltering(ComboBox5.ItemIndex);
end;

procedure TForm633.ComboBox6Change(Sender: TObject);
var
  anim: TAnimation;
begin
  anim := TMSFMXTileList1.GetContentListAnim;
  if Assigned(anim) then
    anim.Interpolation := TInterpolationType(ComboBox6.ItemIndex);
end;

procedure TForm633.ComboBox7Change(Sender: TObject);
var
  anim: TAnimation;
begin
  anim := TMSFMXTileList2.GetContentListAnim;
  if Assigned(anim) then
    anim.Interpolation := TInterpolationType(ComboBox7.ItemIndex);
end;

procedure TForm633.Edit1ChangeTracking(Sender: TObject);
begin
  if DisableLookup then
    Exit;

  TMSFMXTileList2.ApplyFilter(Edit1.Text, False);
end;

procedure TForm633.FormCreate(Sender: TObject);
var
  i: integer;
  it: TTMSFMXTile;
begin
  StyleBook1.Resource.LoadFromFile('TileListStyle.style');
  TabControl1.ActiveTab := TabItem1;

  TMSFMXTileList1.BeginUpdate;
  TMSFMXTileList1.Columns := 6;
  TMSFMXTileList1.Rows := 3;
  TMSFMXTileList1.TileOptions := TMSFMXTileList1.TileOptions + [loBitmap] + [loDetail];
  TMSFMXTileList1.BitmapContainer :=  TMSFMXBitmapContainer1;
  TMSFMXTileList1.Padding.Left := 100;
  TMSFMXTileList1.Padding.Top := 100;
  TMSFMXTileList1.Padding.Right := 100;
  TMSFMXTileList1.Padding.Bottom := 100;

  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile1';
  it.BitmapName := 'mail.png';
  it.Caption := 'Mail';
  it.Badge := '10';
  it.DetailSizePercentage := 30;
  it.DetailCaption := 'You have 10 new unread emails';
  it.Tag := 0;

  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile3';
  it.Caption := 'Finance';
  it.BitmapName := 'finance.png';
  it.ColumnSpan := 2;
  it.RowSpan := 2;
  it.Tag := 2;
  it.EnableDetail := False;
  it.Notes := it.Notes + '- Apple CEO in China Mission to Clear up Problems'+#13#10;
  it.Notes := it.Notes + '- Strong Quake Hits Northern Japan, no Tsunami Risk'+#13#10;
  it.Notes := it.Notes + '- Consumer Confidence Expected to Dip in March'+#13#10;
  it.Notes := it.Notes + #13#10;
  it.Notes := it.Notes + 'FTSE 100 5,893.38 -9.32 -0.16%'+#13#10;
  it.Notes := it.Notes + 'CAC 40 3,491.94 -10.04 -0.29%';


  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile2';
  it.Caption := 'Calendar ' + DateToStr(Now);
  it.BitmapName := 'calendar.png';
  it.Notes := 'New appointment'+#13#10+'Meeting with John at 5am';
  it.ColumnSpan := 2;
  it.DetailCaption := 'Select date';
  it.Tag := 6;

  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile4';
  it.BitmapName := 'stack.png';
  it.Caption := 'Photo album';
  it.EnableDetail := False;

  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile5';
  it.Caption := 'Internet Explorer';
  it.BitmapName := 'internet.png';
  it.EnableDetail := False;
  it.Tag := 4;

  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile1';
  it.BitmapName := 'controlpanel.png';
  it.Caption := 'Control Panel';
  it.EnableDetail := False;
  it.Tag := 0;

  it := TMSFMXTileList1.Tiles.Add;
  it.Transparent := True;
  it.ReadOnly := True;
  it.BitmapName := 'wallpaper2.png';
  it.ColumnSpan := 2;

  it := TMSFMXTileList1.Tiles.Add;
  it.Transparent := True;
  it.ReadOnly := True;
  it.BitmapName := 'wallpaper3.png';
  it.ColumnSpan := 2;

  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile2';
  it.BitmapName := 'Search.png';
  it.Caption := 'Search';
  it.EnableDetail := False;
  it.Tag := 0;

  it := TMSFMXTileList1.Tiles.Add;
  it.Transparent := True;
  it.ReadOnly := True;
  it.BitmapName := 'wallpaper4.png';
  it.ColumnSpan := 2;

  it := TMSFMXTileList1.Tiles.Add;
  it.StyleLookup := '[custom]defaulttile1';
  it.Caption := 'Visited sites';
  it.BitmapName := 'news.png';
  it.RowSpan := 2;
  it.DetailSizePercentage := 100;
  it.DetailCaption := 'List of visited sites';
  it.DetailNotes := '';
  it.EnableDetail := True;
  it.Tag := 5;


  for I := 1 to 10 do
  begin
    with TMSFMXTileList1.Tiles.Add do
    begin
      StyleLookup := '[custom]defaulttile1';
      BitmapName := 'model'+inttostr(I)+'.png';
      ColumnSpan := Random(3);
      RowSpan := ColumnSpan;
      DetailCaption := 'Model ' + inttostr(I);
      DetailNotes := 'A model, sometimes called a mannequin, is a person who is employed to display,'+
      ' advertise and promote commercial products (notably fashion clothing) or to serve as a subject of works of art.';
      Tag := 9;
    end;
  end;


  TMSFMXTileList1.EndUpdate;

  TMSFMXTileList2.BeginUpdate;
  TMSFMXTileList2.KeyBoardMode := kmTileNavigation;
  TMSFMXTileList2.Padding.Left := 100;
  TMSFMXTileList2.Padding.Top := 100;
  TMSFMXTileList2.Padding.Right := 100;
  TMSFMXTileList2.Padding.Bottom := 100;
  TMSFMXTileList2.BitmapContainer := TMSFMXBitmapContainer1;
  TMSFMXTileList2.TileOptions := TMSFMXTileList2.TileOptions + [loBitmap];
  {$IFDEF TMSIOS}
  AddFilesFromFolder(ExtractFilePath(ParamStr(0))+'files/*.*', TMSFMXTileList2);
  {$ELSE}
  AddFilesFromFolder(ExtractFilePath(ParamStr(0))+'files\*.*', TMSFMXTileList2);
  {$ENDIF}


  TMSFMXTileList2.EndUpdate;


  {$IFDEF TMSIOS}
  TMSFMXTileList1.GetHeaderBullet.Width := 17;
  TMSFMXTileList1.GetHeaderBullet.Height := 17;

  TMSFMXTileList1.GetFooterBullet.Width := 17;
  TMSFMXTileList1.GetFooterBullet.Height := 17;
  {$ENDIF}




  CheckBox1.IsChecked := False;
  CheckBox2.IsChecked := False;
  TMSFMXTileList1.SetFocus;
end;

procedure TForm633.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab = TabItem1 then
    TMSFMXTileList1.SetFocus
  else
    TMSFMXTileList2.SetFocus;
end;

procedure TForm633.TMSFMXTileList1ApplyStyleLookup(Sender: TObject);
begin
  TMSFMXTileList1.GetHeader.Visible := CheckBox1.IsChecked;
  TMSFMXTileList1.GetFooter.Visible := CheckBox2.IsChecked;
end;

procedure TForm633.TMSFMXTileList1LoadTile(Sender: TObject; ATile: TTMSFMXTile;
  ATileShape: TControl);
var
  ts: TTMSFMXTileShape;
  c: TAlphaColor;
begin
  ATile.CanSelect := False;
  ts := ATileShape as TTMSFMXTileShape;
  case ATile.Tag of
    0, 5:
    begin
      {$if CompilerVersion > 26}
      (ts.ShapeDetailCaption as TText).VertTextAlign := TTextAlign.Leading;
      {$else}
      (ts.ShapeDetailCaption as TText).VertTextAlign := TTextAlign.taLeading;
      {$ifend}
      (ts.ShapeDetailCaption as TText).Font.Size := 10;
      if (ATile.Tag = 5) and Assigned(Panel2) then
      begin
        Panel2.Visible := True;
        ts.ShapeDetailNotes.InsertObject(ts.ShapeDetailNotes.ChildrenCount, Panel2);
        {$if CompilerVersion > 26}
        Panel2.Align := TAlignLayout.Client;
	{$else}
        Panel2.Align := TAlignLayout.alClient;
	{$ifend}
      end;
    end;
    1,6:
    begin
      ts.ShapeNotes.Height := 50;
      ts.ShapeCaption.Height := 20;
      if (ATile.Tag = 6) and Assigned(Panel3) then
      begin
        Panel3.Visible := True;
        ts.ShapeDetailNotes.InsertObject(ts.ShapeDetailNotes.ChildrenCount, Panel3);
        {$if CompilerVersion > 26}
        Panel3.Align := TAlignLayout.Client;
	{$else}
        Panel3.Align := TAlignLayout.alClient;
	{$ifend}
      end;
    end;
    2:
    begin
      {$if CompilerVersion > 26}
      (ts.ShapeCaption as TText).HorzTextAlign := TTextAlign.Leading;
      (ts.ShapeCaption as TText).VertTextAlign := TTextAlign.Leading;
      (ts.ShapeNotes as TText).HorzTextAlign := TTextAlign.Leading;
      {$else}
      (ts.ShapeCaption as TText).HorzTextAlign := TTextAlign.taLeading;
      (ts.ShapeCaption as TText).VertTextAlign := TTextAlign.taLeading;
      (ts.ShapeNotes as TText).HorzTextAlign := TTextAlign.taLeading;
      {$ifend}
    end;
    9:
    begin
      c := TAlphaColorRec.Black;
      ts.Fill.Color := c;
      ts.Stroke.Color := c;
      ts.FillHover.Color := c;
      ts.StrokeHover.Color := c;
      ts.FillDown.Color := c;
      ts.StrokeDown.Color := c;
      ts.ShapeContent.Free;
      {$if CompilerVersion > 26}
      ts.ShapeBitmap.Align := TAlignLayout.Client;
      {$else}
      ts.ShapeBitmap.Align := TAlignLayout.alClient;
      {$ifend}
      (ts.ShapeBitmap as TTMSFMXBitmap).Center := False;
      (ts.ShapeBitmap as TTMSFMXBitmap).Cropping := True;
      (ts.ShapeBitmap as TTMSFMXBitmap).Stretch := False;
      (ts.ShapeBitmap as TTMSFMXBitmap).AspectRatio := False;
    end;
  end;
end;

procedure TForm633.TMSFMXTileList1TileHideDetail(Sender: TObject;
  ATile: TTMSFMXTile);
begin
  if Assigned(Calendar1) then
    TMSFMXTileList1.Tiles[2].Caption := 'Calendar ' + DateToStr(Calendar1.Date);
end;

procedure TForm633.TMSFMXTileList1UnLoadTile(Sender: TObject;
  ATile: TTMSFMXTile; ATileShape: TControl);
begin
  if Assigned(Panel2) and Assigned(Panel3) then
  begin
    Panel2.Visible := False;
    Panel3.Visible := False;
    Panel2.Parent := Self;
    Panel3.Parent := Self;
    {$if CompilerVersion > 26}
    Panel2.Align := TAlignLayout.None;
    Panel3.Align := TAlignLayout.None;
    {$else}
    Panel2.Align := TAlignLayout.alNone;
    Panel3.Align := TAlignLayout.alNone;
    {$ifend}
  end;
end;

procedure TForm633.TMSFMXTileList2Filter(Sender: TObject; var FilterKey: string;
  var Allow: Boolean);
begin
  Edit1.Text := FilterKey;
end;

procedure TForm633.TMSFMXTileList2FilterFinished(Sender: TObject;
  OldFilterKey: string; var NewFilterKey: string; var Allow: Boolean);
begin
  DisableLookup := True;
  Edit1.Text := NewFilterKey;
  DisableLookup := False;
end;

procedure TForm633.TMSFMXTileList2TileClick(Sender: TObject;
  ATile: TTMSFMXTile);
begin
  ShowMessage(ATile.Caption + ' Clicked !');
end;

end.
