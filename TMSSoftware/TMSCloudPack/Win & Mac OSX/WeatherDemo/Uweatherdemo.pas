unit Uweatherdemo;

interface

uses
  FMX.Forms, FMX.TMSCloudWeather, FMX.Objects, FMX.TMSCloudImage,
  FMX.ListBox, FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Controls,
  System.Classes, FMX.Types, FMX.TMSCloudBase, FMX.Dialogs, SysUtils;

type
  TForm1 = class(TForm)
    TMSFMXCloudWeather1: TTMSFMXCloudWeather;
    TMSFMXCloudWeatherLocationLookupProvider1: TTMSFMXCloudWeatherLocationLookupProvider;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    ListBox1: TListBox;
    GroupBox2: TGroupBox;
    TMSFMXCloudCloudImage1: TTMSFMXCloudImage;
    TMSFMXCloudCloudImage2: TTMSFMXCloudImage;
    TMSFMXCloudCloudImage3: TTMSFMXCloudImage;
    TMSFMXCloudCloudImage4: TTMSFMXCloudImage;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Fore1: TLabel;
    Fore2: TLabel;
    Fore3: TLabel;
    Fore4: TLabel;
    Image1: TImage;
    Label3: TLabel;
    ComboBox1: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.FMX}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  Appkey = 'xxxxxxxxx';
//

{$I APPIDS.INC}

procedure TForm1.Button2Click(Sender: TObject);
var
  s, country, city: string;
  vp: integer;

  function ItemToString(fi: TWeatherForeCastItem): string;
  begin
    Result := DateToStr(fi.Date);
    Result := Result + #13 + fi.Conditions;
    Result := Result + #13 + 'Temp: ' + FloatTostr(fi.TempLowC)+'°C to '+FloatToStr(fi.TempHighC)+'°C';
    Result := Result + #13 + 'Wind: ' + FloatTostr(fi.MaxWindKmh)+ 'Kmh ' + fi.MaxWindDir;
    Result := Result + #13 + 'Humdity: ' + IntToStr(fi.AveHumidity) + '%';
  end;

begin
  if TMSFMXCloudWeather1.App.Key = '' then
  begin
    ShowMessage('Please request a Wundergroup key and set this to TMSFMXCloudWeather.App.Key');
    Exit;
  end;

  Fore1.Text  := '';
  Fore2.Text  := '';
  Fore3.Text  := '';
  Fore4.Text  := '';
  TMSFMXCloudCloudImage1.URL := '';
  TMSFMXCloudCloudImage2.URL := '';
  TMSFMXCloudCloudImage3.URL := '';
  TMSFMXCloudCloudImage4.URL := '';

  if Label1.Text   = '' then
  begin
    ShowMessage('No location specified.');
    Exit;
  end;

  s := Label1.Text;

  vp := pos(',',s);

  if vp > 0 then
  begin
    city := copy(s,1,vp - 1);
    country := copy(s,vp + 1,length(s));
  end
  else
  begin
    country := '';
    city := Label1.Text;
  end;

  TMSFMXCloudWeather1.GetForecast(country, city);

  if TMSFMXCloudWeather1.ForeCast.Count = 4 then
  begin
    TMSFMXCloudCloudImage1.URL := TMSFMXCloudWeather1.ForeCast.Items[0].IconURL;
    TMSFMXCloudCloudImage2.URL := TMSFMXCloudWeather1.ForeCast.Items[1].IconURL;
    TMSFMXCloudCloudImage3.URL := TMSFMXCloudWeather1.ForeCast.Items[2].IconURL;
    TMSFMXCloudCloudImage4.URL := TMSFMXCloudWeather1.ForeCast.Items[3].IconURL;

    Fore1.Text  := ItemToString(TMSFMXCloudWeather1.ForeCast.Items[0]);
    Fore2.Text  := ItemToString(TMSFMXCloudWeather1.ForeCast.Items[1]);
    Fore3.Text  := ItemToString(TMSFMXCloudWeather1.ForeCast.Items[2]);
    Fore4.Text  := ItemToString(TMSFMXCloudWeather1.ForeCast.Items[3]);
  end
  else
    ShowMessage('Weather forecast could not be retrieved');

end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
  0: TMSFMXCloudWeather1.Language := wlEnglish;
  1: TMSFMXCloudWeather1.Language := wlSpanish;
  2: TMSFMXCloudWeather1.Language := wlPortuguese;
  3: TMSFMXCloudWeather1.Language := wlFrench;
  4: TMSFMXCloudWeather1.Language := wlGerman;
  5: TMSFMXCloudWeather1.Language := wlDutch;
  6: TMSFMXCloudWeather1.Language := wlItalian;
  7: TMSFMXCloudWeather1.Language := wlPolish;
  end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  label1.Text  := Edit1.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXCloudWeather1.App.Key := AppKey;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
    Label1.Text  := ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if Edit1.Text <> '' then
  begin
    ListBox1.Items.Clear;
    TMSFMXCloudWeatherLocationLookupProvider1.Lookup(Edit1.Text, ListBox1.Items);
  end;
end;

end.
