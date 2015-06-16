unit UOverView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUISwitch,
  FMX.TMSNativeUIView, FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIToolBar,
  FMX.TMSNativeUIPopoverController, FMX.TMSNativeUITableView,
  FMX.TMSNativeUILabel, FMX.TMSNativeUIDatePicker, FMX.TMSNativeUIPickerView,
  FMX.TMSNativeUISlider, FMX.TMSNativeUIImageView, iOSApi.UIKit, iOSApi.Foundation,
  FMX.TMSNativeUIButton, FMX.StdCtrls, FMX.TMSNativeUISegmentedControl,
  FMX.TMSNativeUISearchBar, FMX.TMSNativeUIStepper, FMX.Layouts,
  FMX.TMSNativeUITextView, FMX.TMSNativeUICore,
  FMX.TMSNativeUIImagePickerController;

type
  TForm925 = class(TForm)
    TMSFMXNativeUITableView1: TTMSFMXNativeUITableView;
    TMSFMXNativeUIPopoverController1: TTMSFMXNativeUIPopoverController;
    TMSFMXNativeUIToolBar1: TTMSFMXNativeUIToolBar;
    TMSFMXNativeUISegmentedControl1: TTMSFMXNativeUISegmentedControl;
    TMSFMXNativeUISegmentedControl2: TTMSFMXNativeUISegmentedControl;
    TMSFMXNativeUITableView2: TTMSFMXNativeUITableView;
    TMSFMXNativeUIPickerView1: TTMSFMXNativeUIPickerView;
    TMSFMXNativeUITextView2: TTMSFMXNativeUITextView;
    TMSFMXNativeUITextView3: TTMSFMXNativeUITextView;
    TMSFMXNativeUITextView4: TTMSFMXNativeUITextView;
    TMSFMXNativeUIImagePickerController1: TTMSFMXNativeUIImagePickerController;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
      AItem: TTMSFMXNativeUIToolBarItem);
    procedure TMSFMXNativeUITableView1ItemSelect(Sender: TObject; ASection,
      ARow: Integer);
    procedure TMSFMXNativeUISwitch1ValueChanged(ASender: TObject;
      AValue: Boolean);
    procedure TMSFMXNativeUIPickerView1GetNumberOfColumns(Sender: TObject;
      var ANumberOfColumns: Integer);
    procedure TMSFMXNativeUIPickerView1GetNumberOfRowsForColumn(Sender: TObject;
      AColumn: Integer; var ANumberOfRows: Integer);
    procedure TMSFMXNativeUIPickerView1GetTitleForRow(Sender: TObject; AColumn,
      ARow: Integer; var ATitle: string);
    procedure FormDestroy(Sender: TObject);
    procedure TMSFMXNativeUISegmentedControl2ValueChanged(ASender: TObject;
      AItem: TTMSFMXNativeUISegmentedControlItem);
    procedure TMSFMXNativeUITableView2ItemSelect(Sender: TObject; ASection,
      ARow: Integer);
    procedure TMSFMXNativeUIPickerView1ValueChanged(Sender: TObject; AColumn,
      ARow: Integer);
  private
    { Private declarations }
    FFruitList, FCarList: TStringList;
  public
    { Public declarations }
  end;

var
  Form925: TForm925;

implementation

uses
  FMX.Platform.iOS;

{$R *.fmx}

procedure TForm925.FormCreate(Sender: TObject);
var
  s: TTMSFMXNativeUITableViewSection;
  idx: NSIndexPath;
begin
  TMSFMXNativeUITableView2.BeginUpdate;
  s := TMSFMXNativeUITableView2.Sections.Add;
  s.Header := 'Fruits';
  s.Items.Add.Text := 'Apple';
  s.Items.Add.Text := 'Banana';
  s.Items.Add.Text := 'Coconut';
  s.Items.Add.Text := 'Strawberry';
  s := TMSFMXNativeUITableView2.Sections.Add;
  s.Header := 'Cars';
  s.Items.Add.Text := 'Mercedes';
  s.Items.Add.Text := 'Audi';
  s.Items.Add.Text := 'BMW';
  s.Items.Sort;
  s := TMSFMXNativeUITableView2.Sections.Add;
  s.Header := 'Vegetables';
  s.Items.Add.Text := 'Cabbage';
  s.Items.Add.Text := 'Tomato';
  s.Items.Add.Text := 'Carrot';
  s.Items.Sort;
  s := TMSFMXNativeUITableView2.Sections.Add;
  s.Header := 'Clothes';
  s.Items.Add.Text := 'Pants';
  s.Items.Add.Text := 'T-Shirt';
  s.Items.Add.Text := 'Shoes';
  s.Items.Sort;
  s := TMSFMXNativeUITableView2.Sections.Add;
  s.Header := 'Sports';
  s.Items.Add.Text := 'Soccer';
  s.Items.Add.Text := 'Tennis';
  s.Items.Add.Text := 'Basketball';
  s.Items.Add.Text := 'Swimming';
  s.Items.Add.Text := 'Bicycling';
  s.Items.Sort;
  s := TMSFMXNativeUITableView2.Sections.Add;
  s.Header := 'Animals';

  with s.Items.Add do
  begin
    Text := 'Elephant';
    AccessoryType := atTableViewCellAccessoryDisclosureIndicator;
    DataString := 'Elephants are large mammals of the family Elephantidae and the order Proboscidea. Traditionally, two species are recognised, the African elephant '+
    '(Loxodonta africana) and the Asian elephant (Elephas maximus), although some evidence suggests that African bush elephants and African forest elephants are separ'+
    'ate species (L. africana and L. cyclotis respectively). Elephants are scattered throughout sub-Saharan Africa, and South and Southeast Asia. They are the only sur'+
    'viving proboscideans; extinct species include mammoths and mastodons.'+
    'The largest living terrestrial animals, male African elephants can reach a height of 4 m (13 ft) and weigh 7,000 kg (15,000 lb).'+
    ' These animals have several distinctive features, including a long proboscis or trunk used for many purposes, particularly for grasping objects. Their incisors gro'+
    'w into tusks, which serve as tools for moving objects and digging and as weapons for fighting. The elephants large ear flaps help to control the temperature of its '+
    'body. African elephants have larger ears and concave backs while Asian elephants have smaller ears and convex or level backs.';
  end;

  with s.Items.Add do
  begin
    Text := 'Bird';
    AccessoryType := atTableViewCellAccessoryDisclosureIndicator;
    DataString := 'Birds (class Aves) are feathered, winged, bipedal, endothermic (warm-blooded), egg-laying, vertebrate animals. With around 10,000 living species, they'+
    ' are the most speciose class of tetrapod vertebrates. All present species belong to the subclass Neornithes, and inhabit ecosystems across the globe, from the Arcti'+
    'c to the Antarctic. Extant birds range in size from the 5 cm (2 in) Bee Hummingbird to the 2.75 m (9 ft) Ostrich. The fossil record indicates that birds emerged with'+
    'in theropod dinosaurs during the Jurassic period, around 150 million years ago.';
  end;

  with s.Items.Add do
  begin
    Text := 'Tiger';
    AccessoryType := atTableViewCellAccessoryDisclosureIndicator;
    DataString := 'The tiger (Panthera tigris) is the largest cat species, reaching a total body length of up to 3.3 m (11 ft) and weighing up to 306 kg (670 lb). It is the'+
    ' third largest land carnivore (behind only the polar bear and the brown bear). Its most recognizable feature is a pattern of dark vertical stripes on reddish-orange fur'+
    ' with a lighter underside.';
  end;

  with s.Items.Add do
  begin
    Text := 'Fish';
    AccessoryType := atTableViewCellAccessoryDisclosureIndicator;
    DataString := 'A fish is any member of a paraphyletic group of organisms that consist of all gill-bearing aquatic craniate animals that lack limbs with digits. Included in'+
    ' this definition are the living hagfish, lampreys, and cartilaginous and bony fish, as well as various extinct related groups. Most fish are ectothermic ("cold-blooded"),'+
    ' allowing their body temperatures to vary as ambient temperatures change, though some of the large active swimmers like white shark and tuna can hold a higher core temperature.';
  end;

  with s.Items.Add do
  begin
    Text := 'Salamander';
    AccessoryType := atTableViewCellAccessoryDisclosureIndicator;
    DataString := 'Salamanders are any of approximately 550 extant species of amphibians within the order Caudata.[1] They are typically characterized by a superficially lizard-lik'+
    'e appearance, with slender bodies, short noses, and long tails. All known fossil salamanders and all extinct species fall under the order Caudata, while sometimes the extant spe'+
    'cies are grouped together as the Urodela';
  end;
  s.Items.Sort;

  TMSFMXNativeUITableView2.Sections.Sort;

  TMSFMXNativeUITableView2.EndUpdate;

  FFruitList := TStringList.Create;
  FFruitList.Add('Apple');
  FFruitList.Add('Banana');
  FFruitList.Add('Coconut');
  FFruitList.Add('Strawberry');
  FCarList := TStringList.Create;
  FCarList.Add('Audi');
  FCarList.Add('Mercedes');
  FCarList.Add('BMW');
  FCarList.Add('Toyota');
  FCarList.Add('Chrysler');
  FCarList.Add('Land Rover');
  FCarList.Add('Renault');
  FCarList.Add('Peugeot');

  TMSFMXNativeUIToolBar1.BeginUpdate;
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'ToolBar Button';
  with TMSFMXNativeUIToolBar1.Items.Add do
  begin
    Kind := ikSystem;
    SystemItem := siBarButtonSystemItemCamera;
  end;
  with TMSFMXNativeUIToolBar1.Items.Add do
  begin
    Kind := ikSystem;
    SystemItem := siBarButtonSystemItemFixedSpace;
  end;
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Style';
  TMSFMXNativeUIToolBar1.EndUpdate;

  TMSFMXNativeUIToolBar1.Items[2].Item.setWidth(15);

  TMSFMXNativeUISegmentedControl1.SelectedSegmentIndex := 0;
  TMSFMXNativeUISegmentedControl2.SelectedSegmentIndex := 1;
  TMSFMXNativeUITextView2.Text := 'Open Document Selected';
  TMSFMXNativeUITextView2.TextView.setBackgroundColor(TUIColor.Wrap(TUIColor.OCClass.clearColor));

  TMSFMXNativeUIPickerView1.SelectRowInColumn(2, 1, True);
  TMSFMXNativeUIPickerView1.SelectRowInColumn(7, 2, True);

  idx := TNSIndexPath.Wrap(TNSIndexPath.OCClass.indexPathForRow(0, 0));
  TMSFMXNativeUITableView2.TableView.selectRowAtIndexPath(iOSApi.Foundation.NSIndexPath(idx), True, UITableViewScrollPositionNone);
  TMSFMXNativeUITableView2ItemSelect(Self, 0, 0);
  TMSFMXNativeUITextView3.TextView.setBackgroundColor(TUIColor.Wrap(TUIColor.OCClass.clearColor));
  TMSFMXNativeUITextView3.Text := 'Coconut';
  TMSFMXNativeUITextView3.Alignment := taRight;
end;

procedure TForm925.FormDestroy(Sender: TObject);
begin
  FCarList.Free;
  FFruitList.Free;
end;

procedure TForm925.TMSFMXNativeUIPickerView1GetNumberOfColumns(Sender: TObject;
  var ANumberOfColumns: Integer);
begin
  ANumberOfColumns := 3;
end;

procedure TForm925.TMSFMXNativeUIPickerView1GetNumberOfRowsForColumn(
  Sender: TObject; AColumn: Integer; var ANumberOfRows: Integer);
begin
  case AColumn of
  0: ANumberOfRows := 7;
  1: ANumberOfRows := FFruitList.Count;
  2: ANumberOfRows := FCarList.Count;
  end;
end;

procedure TForm925.TMSFMXNativeUIPickerView1GetTitleForRow(Sender: TObject;
  AColumn, ARow: Integer; var ATitle: string);
begin
  case AColumn of
  0:
  begin
    case ARow of
    0: ATitle := 'Default';
    1: ATitle := 'Red';
    2: ATitle := 'Blue';
    3: ATitle := 'Orange';
    4: ATitle := 'Green';
    5: ATitle := 'Seagreen';
    6: ATitle := 'Darkred';
    end;
  end;
  1: ATitle := FFruitList[ARow];
  2: ATitle := FCarList[ARow];
  end;
end;

procedure TForm925.TMSFMXNativeUIPickerView1ValueChanged(Sender: TObject;
  AColumn, ARow: Integer);
begin
  case AColumn of
  0:
  begin
    case ARow of
    0: TMSFMXNativeUIToolBar1.ToolBar.setTintColor(nil);
    1: TMSFMXNativeUIToolBar1.TintColor := TAlphaColorRec.Red;
    2: TMSFMXNativeUIToolBar1.TintColor := TAlphaColorRec.Blue;
    3: TMSFMXNativeUIToolBar1.TintColor := TAlphaColorRec.Orange;
    4: TMSFMXNativeUIToolBar1.TintColor := TAlphaColorRec.Green;
    5: TMSFMXNativeUIToolBar1.TintColor := TAlphaColorRec.Seagreen;
    6: TMSFMXNativeUIToolBar1.TintColor := TAlphaColorRec.Darkred;
    end;
  end;
  1: TMSFMXNativeUITextView3.Text := FFruitList[ARow];
  2: TMSFMXNativeUITextView3.Text := FCarList[ARow];
  end;
end;

procedure TForm925.TMSFMXNativeUISegmentedControl2ValueChanged(ASender: TObject;
  AItem: TTMSFMXNativeUISegmentedControlItem);
begin
  TMSFMXNativeUITextView2.Text := AItem.Text + ' Selected';
end;

procedure TForm925.TMSFMXNativeUISwitch1ValueChanged(ASender: TObject;
  AValue: Boolean);
begin
  TMSFMXNativeUIToolBar1.Translucent := AValue;
end;

procedure TForm925.TMSFMXNativeUITableView1ItemSelect(Sender: TObject; ASection,
  ARow: Integer);
var
  I: Integer;
begin
  TMSFMXNativeUITableView1.BeginUpdate;
  for I := 0 to TMSFMXNativeUITableView1.Sections[ASection].Items.Count - 1 do
    TMSFMXNativeUITableView1.Sections[ASection].Items[I].AccessoryType := atTableViewCellAccessoryNone;

  TMSFMXNativeUITableView1.Sections[ASection].Items[ARow].AccessoryType := atTableViewCellAccessoryCheckmark;
  TMSFMXNativeUITableView1.EndUpdate;

  TMSFMXNativeUIToolBar1.Style := TTMSFMXNativeUIToolBarStyle(ARow);
  TMSFMXNativeUIPopoverController1.Hide;
end;

procedure TForm925.TMSFMXNativeUITableView2ItemSelect(Sender: TObject; ASection,
  ARow: Integer);
begin
  TMSFMXNativeUITextView4.Text := TMSFMXNativeUITableView2.Sections[ASection].Items[ARow].DataString;
end;

procedure TForm925.TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
  AItem: TTMSFMXNativeUIToolBarItem);
begin
  case AItem.Index of
    0: ShowMessage('Hello World !');
    1: TMSFMXNativeUIImagePickerController1.ShowFromButton(AItem.Item);
    3: TMSFMXNativeUIPopoverController1.ShowFromButton(AItem.Item);
  end;
end;

end.
