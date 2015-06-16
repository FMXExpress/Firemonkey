unit UGPlacesPhoneDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.TabControl,
  FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle, FMX.TMSCloudImage,
  FMX.TMSCloudGoogleFMX, FMX.TMSCloudCustomGPlaces, FMX.TMSCloudGPlaces,
  FMX.Objects, System.Actions, FMX.ActnList, System.Sensors, FMX.Sensors;

type
  TForm2 = class(TForm)
    TabControl1: TTabControl;
    tabPlaces: TTabItem;
    tabDetail: TTabItem;
    edSearch: TEdit;
    edType: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    btnSearchNearby: TButton;
    lstPlaces: TListBox;
    TMSFMXCloudGPlaces1: TTMSFMXCloudGPlaces;
    lblAutocomplete: TLabel;
    lblAutocompleteText: TLabel;
    btnAutocomplete: TButton;
    ciIcon: TTMSFMXCloudImage;
    lblNameTitle: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label1: TLabel;
    Label7: TLabel;
    lstTypes: TListBox;
    lblOpen: TLabel;
    lblVicinity: TLabel;
    lblWebsite: TLabel;
    lblRating: TLabel;
    lblLattitude: TLabel;
    lblLongtitude: TLabel;
    lblPhone: TLabel;
    lblAddress: TLabel;
    SpeedButton1: TSpeedButton;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    btnNextPage: TButton;
    LocationSensor1: TLocationSensor;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSearchNearbyClick(Sender: TObject);
    procedure btnAutocompleteClick(Sender: TObject);
    procedure lstPlacesItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnNextPageClick(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
  private
    procedure ShowPlacesList;
    procedure SearchNearby;
    procedure ShowSuggestions;
    procedure ShowPlaceInfo;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

var
  lat, long: double;

{$R *.fmx}

{$I APPIDS.INC}

procedure TForm2.btnAutocompleteClick(Sender: TObject);
begin
  edSearch.Text := lblAutocompleteText.Text;
  edType.Text := '';
  Button1Click(Self);
end;

procedure TForm2.btnDownClick(Sender: TObject);
var
  placeIndex: integer;
begin 
  btnUp.Enabled := true;   

  placeIndex := lstPlaces.ItemIndex;

  if placeIndex + 1 <= lstPlaces.items.Count -1 then
  begin
    lstPlaces.ItemIndex := placeIndex+1;

    lstPlaces.OnItemClick(lstPlaces, TListBoxItem(lstPlaces.Items[placeIndex+1]));      
  end
  else
  begin
    btnDown.enabled := false;
  end;
end;

procedure TForm2.btnNextPageClick(Sender: TObject);
begin
  TMSFMXCloudGPlaces1.GetNextPlacesPage();

  ShowPlacesList();
end;

procedure TForm2.btnSearchNearbyClick(Sender: TObject);
begin
  TMSFMXCloudGPlaces1.App.Key := GAppkey;

  if TMSFMXCloudGPlaces1.App.Key <> '' then
  begin
    SearchNearby();

    if lstPlaces.Items.Count > 0 then
      lstPlaces.ItemIndex := 0;
  end;
end;

procedure TForm2.btnUpClick(Sender: TObject);
var
  placeIndex: integer;
begin
  placeIndex := lstPlaces.ItemIndex;

  btnDown.Enabled := true;

  if placeIndex - 1 >= 0 then
  begin
    lstPlaces.ItemIndex := placeIndex-1;

    if placeIndex - 1 = 0 then
      btnUp.Enabled := false;

    lstPlaces.OnItemClick(lstPlaces, TListBoxItem(lstPlaces.Items[placeIndex-1]));      
  end
  else
    btnUp.Enabled := false;    
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  autocomplete: string;
begin

  TMSFMXCloudGPlaces1.App.Key := GAppkey;

  if (edSearch.Text <> '') and (TMSFMXCloudGPlaces1.App.Key <> '') then
  begin
    // search & show results
    TMSFMXCloudGPlaces1.SearchByText(edSearch.Text, edType.Text);
    ShowPlacesList();

//     look for suggestions
    if lblAutocompleteText.Text <> '' then
      autocomplete := TMSFMXCloudGPlaces1.Autocomplete(edSearch.Text);
      lblAutocompleteText.Text := autocomplete;

    // show suggestion
    ShowSuggestions();
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  TMSFMXCloudGPlaces1.App.Key := GAppkey;
  tabPlaces.Visible := true;
  tabDetail.Visible := false;
  TabControl1.ActiveTab := tabPlaces;
  LocationSensor1.Active  := true;
end;

procedure TForm2.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  LDecSeparator: String;
begin
  LDecSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  // current location
  lat  := StrToFloat(Format('%2.6f', [NewLocation.Latitude]));
  long := StrToFloat(Format('%2.6f', [NewLocation.Longitude]));
end;

procedure TForm2.lstPlacesItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  tabPlaces.Visible := false;
  tabDetail.Visible := true;
  TabControl1.ActiveTab := tabDetail;
  
  ShowPlaceInfo();

  SpeedButton1.Visible := true;

  if lstPlaces.ItemIndex = 0 then
    btnUp.Enabled := false;

  btnUp.Visible := true;
  btnDown.Visible := true;  
end;

procedure TForm2.SearchNearby();
begin
  TMSFMXCloudGPlaces1.SearchNearby(long, lat);

  ShowPlacesList();
end;

procedure TForm2.ShowSuggestions();
begin
  lblAutocomplete.Visible := true;
  lblAutocompleteText.Visible := true;
  btnAutocomplete.Visible := true;
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  tabDetail.Visible := false;
  tabPlaces.Visible := true;
  TabControl1.ActiveTab := tabPlaces; 
  SpeedButton1.Visible := false;
  btnUp.Visible := false;
  btnDown.Visible := false;
end;

procedure TForm2.ShowPlaceInfo();
var
  typ: string;
  counter, heightCounter: integer;  
  Itm: TListBoxItem;
  PlaceItem: TGPlacesItem;
  PlaceIndex: integer;
  CloudImage: TTMSFMXCloudImage;
  Obj: TObject;
  Photo: TPhotoItem;
  I: Integer;
begin
  PlaceIndex := lstPlaces.ItemIndex;

  if PlaceIndex >= 0 then
  begin
    PlaceItem := TGPlacesItem(lstPlaces.Items.Objects[PlaceIndex]);

    // Get more info about this place
    PlaceItem := PlaceItem.GetDetails(PlaceItem);

    ciIcon.URL := PlaceItem.Icon;
    lblWebsite.Text := PlaceItem.Website;
    lblAddress.Text := PlaceItem.Address.FormattedAddress;
    lblPhone.Text := PlaceItem.Phone;
    lblNameTitle.Text := PlaceItem.Title;
    lblLattitude.Text := FloatToStr(PlaceItem.Lat);
    lblLongtitude.Text := FloatToStr(PlaceItem.long);
    lblRating.Text := PlaceItem.Rating;
    lblVicinity.Text := PlaceItem.Vicinity;
    if PlaceItem.Open then
      lblOpen.Text := 'Yes'
    else
      lblOpen.Text := 'No';  

    lstTypes.Items.Clear;
    lstTypes.Items := PlaceItem.Types;

  end;
end;

procedure TForm2.ShowPlacesList();
var
  Obj: TObject;
  Place: TGPlacesItem;
  Itm: TListBoxItem;
begin
  // clear the list
  lstPlaces.Items.Clear;

  if TMSFMXCloudGPlaces1.HasNextPage then
    btnNextPage.Enabled := true
  else
    btnNextPage.Enabled := false;    

  // show the list
  for Obj in TMSFMXCloudGPlaces1.Items do
  begin
    Place := TGPlacesItem(Obj);

    Itm := TListBoxItem.Create(lstPlaces);
    Itm.Text := Place.Title;
    Itm.Data := Place;
    lstPlaces.AddObject(Itm);
  end;
end;

end.
