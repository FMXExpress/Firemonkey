unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSPageSlider, FMX.Effects, FMX.Objects, FMX.TMSHTMLText, FMX.TMSTableView,
  FMX.TMSBitmapContainer, FMX.TMSBarButton, FMX.TMSBitmap, FMX.StdCtrls;

type
  TCStrings = class(TStrings);

  TForm610 = class(TForm)
    Page1: TTMSFMXPage;
    Page2: TTMSFMXPage;
    TMSFMXPageSlider1: TTMSFMXPageSlider;
    TMSFMXTableView1: TTMSFMXTableView;
    StyleBook1: TStyleBook;
    TMSFMXTableView2: TTMSFMXTableView;
    TMSFMXTableView3: TTMSFMXTableView;
    Label1: TLabel;
    Label2: TLabel;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    TMSFMXBitmap1: TTMSFMXBitmap;
    TMSFMXBitmap2: TTMSFMXBitmap;
    TMSFMXBarButton1: TTMSFMXBarButton;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    TMSFMXBitmap3: TTMSFMXBitmap;
    TMSFMXBarButton2: TTMSFMXBarButton;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXTableView1ItemClick(Sender: TObject;
      AItem: TTMSFMXTableViewItem);
    procedure TMSFMXTableView2ItemClick(Sender: TObject;
      AItem: TTMSFMXTableViewItem);
    procedure TMSFMXBarButton1Click(Sender: TObject);
    procedure TMSFMXTableView3ItemClick(Sender: TObject;
      AItem: TTMSFMXTableViewItem);
    procedure TMSFMXBarButton2Click(Sender: TObject);
    procedure TMSFMXPageSlider1Change(Sender: TObject);
    procedure Page1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowRegionData(ATableViewItem: TTMSFMXTableViewItem);
    procedure ShowCountryData(ATableViewItem: TTMSFMXTableViewItem);
    procedure ShowWineData(ATableViewItem: TTMSFMXTableViewItem);
  end;

var
  Form610: TForm610;

implementation

{$R *.fmx}

function VarPos(su,s:string;var vp: Integer):Integer;
begin
  vp := Pos(su,s);
  Result := vp;
end;


procedure Split(s:string; sl: TStringList; delimiter: string);
var
  vp: integer;
begin
  sl.Clear;
  while VarPos(delimiter, s, vp) > 0 do
  begin
    sl.Add(copy(s, 1, vp - 1));
    delete(s, 1, vp);
  end;

  if s <> '' then
    sl.Add(s)
end;

procedure LoadData(AFileName: String; AElementIndex, ATagIndex: Integer; ATableView: TTMSFMXTableView);
var
  s: TextFile;
  st: TStringList;
  str: String;
  I: Integer;
begin
  ATableView.BeginUpdate;
  ATableView.Items.Clear;
  AssignFile(s, ExtractFilePath(ParamStr(0))+AFileName);
  Reset(s);

  I := 0;
  while not Eof(s) do
  begin
    st := TStringList.Create;
    ReadLn(s, str);
    Split(str, st, ';');
    if (ATagIndex = -1) or (strtoint(st[2]) = ATagIndex) then
    begin
      with ATableView.Items.Add do
      begin
        Caption := st[AElementIndex];
        DataString := str;
        Tag := strtoint(st[0]);
        if UpperCase(AFileName) = 'TBLCOUNTRY.TXT' then
          BitmapName := 'world' + inttostr(I + 1)
        else if UpperCase(AFileName) = 'TBLREGION.TXT' then
          BitmapName := inttostr(I)+'.png'
        else
          BitmapName := st[7];
      end;
    end;

    Inc(I);
  end;

  CloseFile(s);
  ATableView.EndUpdate;
end;

procedure TForm610.FormCreate(Sender: TObject);
begin
  StyleBook1.Resource.LoadFromFile('PageSliderStyle.style');
  {$IFDEF TMSIOS}
  Page1.Effect := False;
  Page2.Effect := False;
  TMSFMXPageSlider1.Effect := False;
  {$ENDIF}
  TMSFMXPageSlider1.ActivePageIndex := -1;
  LoadData('tblCountry.txt', 1, -1, TMSFMXTableView1);
  TMSFMXTableView1.SelectedItemIndex := 0;
  ShowCountryData(TMSFMXTableView1.SelectedItem);
end;

procedure TForm610.Page1Resize(Sender: TObject);
begin
  TMSFMXBarButton2.Position.X := 8;
  TMSFMXBarButton2.Position.Y := Page1.Height - 4 - TMSFMXBarButton2.Height;
end;

procedure TForm610.ShowCountryData(ATableViewItem: TTMSFMXTableViewItem);
begin
  LoadData('tblRegion.txt', 1, ATableViewItem.Tag,TMSFMXTableView2);
  TMSFMXPageSlider1.ActivePageIndex := 0;
  LoadData('tblWines.txt', 1, 1, TMSFMXTableView3);
  TMSFMXTableView2.SelectedItemIndex := -1;
  TMSFMXTableView2.SelectedItemIndex := 0;
  TMSFMXTableView3.SelectedItemIndex := -1;
  TMSFMXTableView3.SelectedItemIndex := 0;
  ShowRegionData(TMSFMXTableView2.SelectedItem);
  ShowWineData(TMSFMXTableView3.SelectedItem);
end;

procedure TForm610.ShowRegionData(ATableViewItem: TTMSFMXTableViewItem);
var
  st: TStringList;
  idx: Integer;
begin
  st := TStringList.Create;
  Split(ATableViewItem.DataString, st, ';');
  idx := strtoint(st[2]) - 1;
  label2.Text := TMSFMXTableView1.Items[idx].Caption;
  TMSFMXBitmap1.BitmapName := TMSFMXTableView1.SelectedItem.BitmapName;
  TMSFMXBitmap2.BitmapName := TMSFMXTableView2.SelectedItem.BitmapName;
  st.Free;
end;

procedure TForm610.ShowWineData(ATableViewItem: TTMSFMXTableViewItem);
var
  st: TStringList;
begin
  if Assigned(ATableViewItem) then
  begin
    st := TStringList.Create;
    Split(ATableViewItem.DataString, st, ';');
    label14.Text := TMSFMXTableView2.SelectedItem.Caption;
    label12.Text := st[4];
    label10.Text := st[6];
    label8.Text := st[3];
    label6.Text := st[5];
    TMSFMXBitmap3.BitmapName := TMSFMXTableView3.SelectedItem.BitmapName;
    st.Free;
  end;
end;

procedure TForm610.TMSFMXBarButton1Click(Sender: TObject);
var
  sel: TTMSFMXTableViewItem;
begin
  sel := TMSFMXTableView2.SelectedItem;
  if Assigned(sel) then
  begin
    LoadData('tblWines.txt', 1, sel.Tag, TMSFMXTableView3);
    ShowWineData(TMSFMXTableView3.SelectedItem);
    TMSFMXPageSlider1.ActivePageIndex := 1;
    TMSFMXTableView3.SelectedItemIndex := -1;
    TMSFMXTableView3.SelectedItemIndex := 0;
  end;
end;

procedure TForm610.TMSFMXBarButton2Click(Sender: TObject);
begin
  TMSFMXPageSlider1.ActivePageIndex := 0;
end;

procedure TForm610.TMSFMXPageSlider1Change(Sender: TObject);
begin
  TMSFMXBarButton2.Visible := TMSFMXPageSlider1.ActivePageIndex = 1;
end;

procedure TForm610.TMSFMXTableView1ItemClick(Sender: TObject;
  AItem: TTMSFMXTableViewItem);
begin
  if Assigned(AItem) then
    ShowCountryData(AItem);
end;

procedure TForm610.TMSFMXTableView2ItemClick(Sender: TObject;
  AItem: TTMSFMXTableViewItem);
begin
  if Assigned(AItem) then
  begin
    LoadData('tblWines.txt', 1, AItem.Tag, TMSFMXTableView3);
    ShowRegionData(AItem);
    TMSFMXTableView3.SelectedItemIndex := -1;
    TMSFMXTableView3.SelectedItemIndex := 0;
    ShowWineData(TMSFMXTableView3.SelectedItem);
  end;
end;

procedure TForm610.TMSFMXTableView3ItemClick(Sender: TObject;
  AItem: TTMSFMXTableViewItem);
begin
  ShowWineData(AItem);
end;

end.
