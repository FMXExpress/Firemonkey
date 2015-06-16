unit UWeatherDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Edit, FMX.TMSCloudBase, FMX.TMSCloudWeather,
  FMX.TMSCloudImage, FMX.TabControl, FMX.Objects;

type
  TForm82 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    TMSFMXCloudWeather1: TTMSFMXCloudWeather;
    TMSFMXCloudWeatherLocationLookupProvider1: TTMSFMXCloudWeatherLocationLookupProvider;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Button2: TButton;
    ComboBox1: TComboBox;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    TMSFMXCloudImage2: TTMSFMXCloudImage;
    Fore2: TLabel;
    TMSFMXCloudImage3: TTMSFMXCloudImage;
    fore3: TLabel;
    Fore1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form82: TForm82;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  Appkey = 'xxxxxxxxx';
//

{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
begin
  if Edit1.Text <> '' then
  begin
    ListBox1.Items.Clear;
    TMSFMXCloudWeatherLocationLookupProvider1.Lookup(Edit1.Text, ListBox1.Items);
  end;
end;

procedure TForm82.Button2Click(Sender: TObject);
var
  s, country, city: string;
  vp: integer;

  function ItemToString(fi: TWeatherForeCastItem): string;
  begin
    Result := DateToStr(fi.Date);
    Result := Result + ' ' + fi.Conditions;
    Result := Result + #13 + 'Temp: ' + FloatTostr(fi.TempLowC)+'°C to '+FloatToStr(fi.TempHighC)+'°C';
    Result := Result + #13 + 'Wind: ' + FloatTostr(fi.MaxWindKmh)+ 'Kmh ' + fi.MaxWindDir;
    Result := Result + #13 + 'Humidity: ' + IntToStr(fi.AveHumidity) + '%';
  end;

begin
  if TMSFMXCloudWeather1.App.Key = '' then
  begin
    ShowMessage('Please request a Wundergroup key and set this to TMSFMXCloudWeather.App.Key');
    Exit;
  end;

  Fore1.Text := '';
  Fore2.Text := '';
  Fore3.Text := '';
  TMSFMXCloudImage1.URL := '';
  TMSFMXCloudImage2.URL := '';
  TMSFMXCloudImage3.URL := '';

  if ListBox1.Selected  = nil then
  begin
    ShowMessage('No location specified.');
    Exit;
  end;

  s := ListBox1.Selected.Text;

  vp := pos(',',s);

  if vp > 0 then
  begin
    city := copy(s,1,vp - 1);
    country := copy(s,vp + 1,length(s));
  end
  else
  begin
    country := '';
    city := ListBox1.Selected.Text;
  end;

  TMSFMXCloudWeather1.GetForecast(country, city);

  if TMSFMXCloudWeather1.ForeCast.Count = 4 then
  begin
    TMSFMXCloudImage1.URL := TMSFMXCloudWeather1.ForeCast.Items[0].IconURL;
    TMSFMXCloudImage2.URL := TMSFMXCloudWeather1.ForeCast.Items[1].IconURL;
    TMSFMXCloudImage3.URL := TMSFMXCloudWeather1.ForeCast.Items[2].IconURL;

    Fore1.Text := ItemToString(TMSFMXCloudWeather1.ForeCast.Items[0]);
    Fore2.Text := ItemToString(TMSFMXCloudWeather1.ForeCast.Items[1]);
    Fore3.Text := ItemToString(TMSFMXCloudWeather1.ForeCast.Items[2]);
  end
  else
    ShowMessage('Weather forecast could not be retrieved');
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  TMSFMXCloudWeather1.App.Key := AppKey;
  TMSFMXCloudWeatherLocationLookupProvider1.App.Key := AppKey;
end;

procedure TForm82.ListBox1Change(Sender: TObject);
begin
  if Assigned(ListBox1.Selected) then
    Button2.Text := 'Weather Forecast for '+ ListBox1.Selected.Text
  else
    Button2.Text := 'Weather Forecast';
end;

end.
