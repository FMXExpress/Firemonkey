//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit formMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Filter.Effects,
  FMX.Effects, FMX.Layouts, FMX.Objects, FMX.Edit, unitSearchMenuHelper, System.IOUtils,
  FMX.Ani, FMX.Menus, FMX.StdCtrls, FMX.Graphics, FMX.Controls.Presentation;

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    ReflectionEffect1: TReflectionEffect;
    OpenDialog1: TOpenDialog;
    VertScrollBox1: TVertScrollBox;
    Layout2: TLayout;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem1: TMenuItem;
    edtSearch: TClearingEdit;
    MainMenu1: TMainMenu;
    Panel1: TPanel;
    procedure LoadImages(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure ManageClick(Sender: TObject);
    procedure LoadData(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    SearchBandManager : TSearchBandManager;
    function InitializeBandManager : TSearchBand;
    procedure ItemSelected(Sender : TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

uses formData, dataData;

procedure TfrmMain.ManageClick(Sender: TObject);
begin
  if frmManageData = nil then
    frmManageData := TfrmManageData.Create(Self);
  frmManageData.Show;
end;

procedure TfrmMain.LoadData(Sender: TObject);
var
  Band: TSearchBand;
  SearchItem: TSearchItem;
  Image: TImage;
begin
  InitializeBandManager;

  SearchBandManager.Clear;
  //SearchBandManager.
  //Band := TSearchBand.Create(Self,True,Self.Fill.Color,'Images Group '+IntToStr(SearchBandManager.Count+1),100,100);
  //SearchBandManager.Add(Band);

  // Load the image in. (to random sized bands)
  with dtmdlData do begin
    cdsIconData.First;
    while not cdsIcondata.Eof do begin

      Band := SearchBandManager.BandByName(Uppercase(cdsIconDataCategory.AsString));
      if Band = nil then begin
        Band := TSearchBand.Create(Self,True,Self.Fill.Color,UpperCase(cdsIconDataCategory.Text),100,100);
        SearchBandManager.Add(Band);
      end;
      Image := TImage.Create(Self);
      try
        Image.Bitmap.Assign(cdsIconDataIcon);
        SearchItem := TSearchItem.Create(Self,Self.Fill.Color,TAlphaColorRec.White,Image,cdsIconDataDescription.Text);
        SearchItem.SearchText := cdsIconDataSearchTerms.AsString;
        SearchItem.OnDblClick := ItemSelected;
        Band.Add(SearchItem);
      finally
        Image.Free;
      end;
      cdsIconData.Next;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadData(nil);
end;

function TfrmMain.InitializeBandManager : TSearchBand;
begin
  if SearchBandManager = nil then
  begin
    SearchBandManager := TSearchBandManager.Create(Self, True);
    SearchBandManager.Parent := Self.VertScrollBox1;
    SearchBandManager.Align := TAlignLayout.Client;
    SearchBandManager.Visible := True;
    Result := nil;
  end
  else if SearchBandManager.Count > 0 then
    Result := SearchBandManager.Items[SearchBandManager.Count - 1]
  else
    Result := nil;
end;

procedure TfrmMain.ItemSelected(Sender: TObject);
begin
  ShowMessage('Search Text: '+(Sender as TSearchItem).SearchText);
end;

procedure TfrmMain.LoadImages(Sender: TObject);
var
  CurrFile : string;
  FilterPredicate : TDirectory.TFilterPredicate;
  Dir: string;
  Band: TSearchBand;
  SearchItem: TSearchItem;
  Image: TImage;
begin
  // Load pictures as the data currently..
  if not OpenDialog1.Execute then
    Exit;

  // Find Each Image
  Dir := ExtractFilePath(OpenDialog1.FileName);

  FilterPredicate := function(const Path: string; const SearchRec: TSearchRec): Boolean
                     begin
                       Result := (TPath.MatchesPattern(SearchRec.Name, '*.*', False)); // and

                       if Result then // Check if it is a image file
                       begin
                         Result := Pos(LowerCase(ExtractFileExt(SearchRec.Name)),
                                    TBitmapCodecManager.GetFileTypes) > 0;
                       end;
                     end;


  Band := TSearchBand.Create(Self,True,Self.Fill.Color,'Images Group '+IntToStr(SearchBandManager.Count+1),100,100);
  SearchBandManager.Add(Band);

  // Load the image in. (to random sized bands)
  for CurrFile in TDirectory.GetFiles(Dir, FilterPredicate) do begin
    Image := TImage.Create(Self);
    try
      Image.Bitmap.LoadFromFile(CurrFile);
      SearchItem := TSearchItem.Create(Self,Self.Fill.Color,TAlphaColorRec.White,Image,ExtractFileName(CurrFile));
      SearchItem.SearchText := ChangeFileExt(ExtractFileName(CurrFile),'');
      Band.Add(SearchItem);
    finally
      Image.Free;
    end;
  end;
end;

procedure TfrmMain.edtSearchKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Self.SearchBandManager = nil then
    Exit;

  Self.SearchBandManager.TextSearch(Self.edtSearch.Text);
end;

end.
