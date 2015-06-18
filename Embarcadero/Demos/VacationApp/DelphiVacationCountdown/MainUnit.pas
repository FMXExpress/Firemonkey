unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Layouts, FMX.StdCtrls, System.Actions, FMX.ActnList, IPPeerClient,
  FMX.Memo, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, FMX.ListView.Types, FMX.ListView, REST.Response.Adapter,
  Data.DB, Datasnap.DBClient, SimpleDS, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Data.Bind.DBScope, FMX.TabControl, FMX.Edit, FMX.WebBrowser, FMX.DateTimeCtrls,
  System.Sensors;

type
  TForm5 = class(TForm)
    ToolBar1: TToolBar;
    CountdownLabel: TLabel;
    Layout1: TLayout;
    WeatherListBox: TListBox;
    RefreshButton: TSpeedButton;
    ActionList1: TActionList;
    MyAction: TAction;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    BindingsList1: TBindingsList;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    BindSourceDB1: TBindSourceDB;
    LinkFillControlToField1: TLinkFillControlToField;
    ClientDataSet1: TClientDataSet;
    VacationTabControl: TTabControl;
    WeatherTabItem: TTabItem;
    SettingsTabItem: TTabItem;
    ListBox1: TListBox;
    AddressListBoxItem: TListBoxItem;
    LatLongListBoxItem: TListBoxItem;
    AddressEdit: TEdit;
    WebBrowser1: TWebBrowser;
    DateListBoxItem: TListBoxItem;
    CalendarEdit1: TCalendarEdit;
    LatLongLabel: TLabel;
    BrowserTabItem: TTabItem;
    procedure RefreshButtonClick(Sender: TObject);
    procedure MyActionExecute(Sender: TObject);
    procedure AddressEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FoundAddrLatLong : boolean;  // if LatLong found using Address
    FoundLatitude : double;
    FoundLongitude : double;
    lAddress : TCivicAddress;
    APIKeyString : string;
    FGeocoder: TGeocoder;
    procedure OnGeocodeEvent(const Coords: TArray<TLocationCoord2D>);
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}
uses
  System.DateUtils, System.IniFiles,System.iOUTils;

const
  LGoogleMapsURL: String = 'https://maps.google.com/maps?q=%s+%s&output=embed';

procedure TForm5.FormCreate(Sender: TObject);
var
   Ini: TIniFile;
begin
  // set up for Geocode
  FGeocoder := TGeocoder.Current.Create;
  FGeocoder.OnGeocode := OnGeocodeEvent;
  VacationTabControl.ActiveTab := WeatherTabItem;
  FoundLatitude := 0.0;
  FoundLongitude := 0.0;
  FoundAddrLatLong := false;
  RefreshButton.Visible := false;
  // read WeatherBug API Key from INI file
  {$IF DEFINED(IOS) or DEFINED(ANDROID) or DEFINED(MACOS)}
  Ini := TIniFile.Create(TPath.GetDocumentsPath + PathDelim + 'weather.ini');
  {$ELSE}
  Ini := TIniFile.Create('c:\temp\weather.ini');
  {$ENDIF}
  APIKeyString := Ini.ReadString('WeatherBug','APIKey','');
end;

procedure TForm5.AddressEditChange(Sender: TObject);
begin
  // Address changed - get Latitude/Longitude via GeoCoding
  RefreshButton.Visible := false;
  // use address to find Latitude and Longitude
  lAddress := TCivicAddress.Create;
  try
    lAddress.Address := AddressEdit.Text;
    FGeocoder.Geocode(lAddress);
  finally
    lAddress.Free;
  end;
end;

procedure TForm5.MyActionExecute(Sender: TObject);
var
  MyListBoxItem : TListBoxItem;
begin
  // days to vacation
  CountdownLabel.Text :=
    'Days to Vacation: '
    + IntToStr(DaysBetween(
        Now(),
        CalendarEdit1.Date)
      )
  ;

  if FoundAddrLatLong then begin

    // uses WeatherBug REST API
    // http://developer.weatherbug.com/docs/read/WeatherBug_Rest_XML_API
    // update RESTRequest Resource property
    // with latitude, longitude and API key
    RESTRequest1.Resource :=
      'REST/Direct/GetForecast.ashx?'
      + 'la='+FoundLatitude.ToString
      + '&'
      + 'lo='+FoundLongitude.ToString
      + '&ht=t&ht=i&ht=d&'
      + 'api_key='+APIKeyString
    ;

    // get weather temperatures
    RestRequest1.Execute;

    // Populate listbox with temperatures for next 7 days
    WeatherListBox.Items.Clear;
    WeatherListBox.BeginUpdate;
    ClientDataSet1.First;
    while not ClientDataSet1.Eof do begin

      // TODO: get day and night icons for condition codes
      //   For now just display strings

      MyListBoxItem := TListBoxItem.Create(WeatherListBox);
      MyListBoxItem.Text :=
        copy(ClientDataSet1.FieldByName('dayTitle').AsString,1,3)
        + ' Hi: '
        + ClientDataSet1.FieldByName('high').AsString
        + ' Lo: '
        + ClientDataSet1.FieldByName('low').AsString
        + ' '
        + ClientDataSet1.FieldByName('dayDesc').AsString
      ;
      MyListBoxItem.TextAlign := TTextAlign.taCenter;
      WeatherListBox.AddObject(MyListBoxItem);
      ClientDataSet1.Next
    end;
    WeatherListBox.EndUpdate
  end
end;

procedure TForm5.RefreshButtonClick(Sender: TObject);
begin
  MyAction.Execute
end;

procedure TForm5.OnGeocodeEvent(const Coords: TArray<TLocationCoord2D>);
begin
  if Length(Coords) > 0 then begin
    FoundLatitude := Coords[0].Latitude;
    FoundLongitude := Coords[0].Longitude;
    FoundAddrLatLong := true;
    LatLongLabel.Text :=
      Format('%3.5f/%3.5f',[Coords[0].Latitude, Coords[0].Longitude]);
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FormatSettings.DecimalSeparator := '.';
    WebBrowser1.Navigate(Format(LGoogleMapsURL, [FoundLatitude.ToString, FoundLongitude.ToString]));
    {$ENDIF}
    RefreshButton.Visible := true;
  end
  else begin
    FoundAddrLatLong := false;
    LatLongLabel.Text := 'Address not Found!'
  end;
end;

end.
