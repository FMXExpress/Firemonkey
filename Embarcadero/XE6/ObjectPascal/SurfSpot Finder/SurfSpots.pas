unit SurfSpots;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, FMX.Edit, Data.Bind.Components, Data.Bind.DBScope,
  FMX.Layouts, FMX.ListBox, REST.Response.Adapter, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Client, Data.Bind.ObjectScope,
  FMX.ListView.Types, FMX.ListView, FMX.StdCtrls, FMX.TabControl, FMX.WebBrowser,
  System.Sensors, FMX.Memo, System.Actions, FMX.ActnList;

type
  TSurfSpotFinder = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    FDMemTable1: TFDMemTable;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    AllSurfSpotsListView: TListView;
    TabControl1: TTabControl;
    AllSurfSpotsTab: TTabItem;
    SurfSpotDetailsTab: TTabItem;
    LinkListControlToField1: TLinkListControlToField;
    LinkFillControlToField2: TLinkFillControlToField;
    WebBrowser1: TWebBrowser;
    SurfSpotDetailsToolbar: TToolBar;
    SpotName: TLabel;
    LinkPropertyToFieldText: TLinkPropertyToField;
    SurfSpotInfoList: TListBox;
    LatitudeItem: TListBoxItem;
    LongitudeItem: TListBoxItem;
    LinkPropertyToFieldItemDataText: TLinkPropertyToField;
    LinkPropertyToFieldItemDataText2: TLinkPropertyToField;
    CountyItem: TListBoxItem;
    LinkPropertyToFieldItemDataText3: TLinkPropertyToField;
    AllSurfSpotsToolbar: TToolBar;
    AllSurfSpotsToollabel: TLabel;
    ActionList1: TActionList;
    ChangeTabAction1: TChangeTabAction;
    backbtn: TSpeedButton;
    ChangeTabAction2: TChangeTabAction;
    LinkFillControlToField3: TLinkFillControlToField;
    LinkFillControlToField4: TLinkFillControlToField;
    FavoritesTab: TTabItem;
    FavoritesListBox: TListBox;
    LinkListControlToField2: TLinkListControlToField;
    FavoritesToolbar: TToolBar;
    FavoritesToollabel: TLabel;
    backtoHome: TSpeedButton;
    ChangeTabAction3: TChangeTabAction;
    GotoFavorites: TSpeedButton;
    SaveasFavorite: TSpeedButton;
    ChangeTabAction4: TChangeTabAction;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FavoritesListBoxItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure GotoFavoritesClick(Sender: TObject);
    procedure SaveasFavoriteClick(Sender: TObject);
    procedure AllSurfSpotsListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure backtoHomeClick(Sender: TObject);
    procedure backbtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SurfSpotFinder: TSurfSpotFinder;

implementation

{$R *.fmx}

uses
System.IOUtils;

procedure TSurfSpotFinder.AllSurfSpotsListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
const
  LGoogleMapsURL: String = 'https://maps.google.com/maps?q=%s,%s';

begin
  WebBrowser1.Navigate(Format(LGoogleMapsURL, [LatitudeItem.Text, LongitudeItem.Text]));
  ChangeTabAction1.Tab := SurfSpotDetailsTab;
  ChangeTabAction1.ExecuteTarget(Self);
  ChangeTabAction1.Tab := AllSurfSpotsTab;
end;

procedure TSurfSpotFinder.FormCreate(Sender: TObject);
begin
  RESTRequest1.Execute;
  TabControl1.ActiveTab := AllSurfSpotsTab;
  if FileExists(TPath.GetDocumentsPath + PathDelim + 'myfile.txt') then
    FavoritesListBox.Items.LoadfromFile(TPath.GetDocumentsPath + PathDelim + 'myfile.txt');
end;

procedure TSurfSpotFinder.FormShow(Sender: TObject);
begin
  RESTRequest1.Execute;
end;

procedure TSurfSpotFinder.backbtnClick(Sender: TObject);
begin
  ChangeTabAction1.Tab := AllSurfSpotsTab;
  ChangeTabAction1.ExecuteTarget(Self);
end;

procedure TSurfSpotFinder.backtoHomeClick(Sender: TObject);
begin
  ChangeTabAction1.Tab := AllSurfSpotsTab;
  ChangeTabAction1.ExecuteTarget(Self);
end;

procedure TSurfSpotFinder.FavoritesListBoxItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  LPosLock: IScopeLocks;
begin
  Supports(BindSourceDB1, IScopeLocks, LPosLock);
  LPosLock.PosLockEnter;
  try
    FDMemTable1.Locate('spot_name', Item.Text);
  finally
    LPosLock.PosLockLeave;
  end;
  ChangeTabAction1.Tab := SurfSpotDetailsTab;
  ChangeTabAction1.ExecuteTarget(Self);
end;

procedure TSurfSpotFinder.GotoFavoritesClick(Sender: TObject);
begin
  TabControl1.ActiveTab := FavoritesTab;
end;

procedure TSurfSpotFinder.SaveasFavoriteClick(Sender: TObject);
begin
  if FavoritesListBox.Items.IndexOf(SpotName.Text) < 0 then
  begin
    FavoritesListBox.Items.Add(Spotname.Text);
    ShowMessage('Saved as Favorite');
    FavoritesListBox.Items.SaveToFile(TPath.GetDocumentsPath + PathDelim + 'myfile.txt');
  end
  else
    ShowMessage('Item already saved as Favorite');
end;


end.


