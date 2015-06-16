unit UCollectionView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.TMSNativeUIImageView, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUICollectionView, Generics.Collections, iOSApi.UIKit, iOSApi.Foundation,
  iOSApi.CocoaTypes, MacApi.ObjectiveC, MacApi.ObjcRuntime, FMX.TMSNativeUICore, FMX.TMSNativeUILabel,
  FMX.TMSNativeUITextView, FMX.TMSNativeUIView, TypInfo, FMX.TMSNativeUIToolBar,
  FMX.TMSNativeUISwitch;

type
  TCity = class(TPersistent)
  private
    FPrice: String;
    FImage: String;
    FRating: String;
    FTitle: String;
    FDescription: String;
    FSelected: Boolean;
  public
    property Title: String read FTitle write FTitle;
    property Description: String read FDescription write FDescription;
    property Rating: String read FRating write FRating;
    property Image: String read FImage write FImage;
    property Price: String read FPrice write FPrice;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TCities = class(TList<TCity>);

  TContinent = class(TPersistent)
  private
    FCities: TCities;
    FTitle: String;
    FID: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property Cities: TCities read FCities write FCities;
  end;

  TContinents = class(TList<TContinent>);

  IUITapGestureTarget = interface(NSObject)
  ['{03CE014E-35C0-4904-A5D0-56BB1E18E174}']
    procedure tapped(Sender: Pointer); cdecl;
  end;

  TUITapGestureTarget = class(TOCLocal)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure tapped(Sender: Pointer); cdecl;
  end;

  TForm1130 = class(TForm)
    CityTrip: TTMSFMXNativeUICollectionView;
    SectionHeader: TTMSFMXNativeUICollectionViewTemplateLabel;
    ItemShadow: TTMSFMXNativeUICollectionViewTemplateImageView;
    ItemTitle: TTMSFMXNativeUICollectionViewTemplateLabel;
    ItemImage: TTMSFMXNativeUICollectionViewTemplateImageView;
    ItemRating: TTMSFMXNativeUICollectionViewTemplateImageView;
    DetailView: TTMSFMXNativeUIView;
    DetailImage: TTMSFMXNativeUIImageView;
    DetailDescription: TTMSFMXNativeUITextView;
    DetailTitle: TTMSFMXNativeUILabel;
    DetailPrice: TTMSFMXNativeUILabel;
    DetailRating: TTMSFMXNativeUIImageView;
    Overlay: TTMSFMXNativeUIView;
    detailshadow: TTMSFMXNativeUIImageView;
    ItemPrice: TTMSFMXNativeUICollectionViewTemplateLabel;
    CityTripLabel: TTMSFMXNativeUILabel;
    BackGroundImage: TTMSFMXNativeUIImageView;
    Wrapper: TTMSFMXNativeUIView;
    ToggleMultiSelect: TTMSFMXNativeUISwitch;
    MultiSelectLabel: TTMSFMXNativeUILabel;
    ItemCheck: TTMSFMXNativeUICollectionViewTemplateCheckBox;
    procedure CityTripGetNumberOfSections(Sender: TObject;
      var ANumberOfSections: Integer);
    procedure CityTripGetNumberOfItemsInSection(
      Sender: TObject; ASection: Integer; var ANumberOfItems: Integer);
    procedure FormCreate(Sender: TObject);
    procedure CityTripApplyItemValue(Sender: TObject;
      AControl: TTMSFMXNativeUICollectionViewTemplateControl; ASection,
      ARow: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure CityTripApplyHeaderValue(Sender: TObject;
      AControl: TTMSFMXNativeUICollectionViewTemplateControl; ASection,
      ARow: Integer);
    procedure CityTripDidselectItem(Sender: TObject;
      ASection, ARow: Integer);
    procedure FormResize(Sender: TObject);
    procedure ToggleMultiSelectValueChanged(ASender: TObject;
      AValue: Boolean);
    procedure CityTripDidDeselectItem(Sender: TObject; ASection, ARow: Integer);
  private
    { Private declarations }
    FContinents: TContinents;
    FTapGesture: UITapGestureRecognizer;
    FTapGestureTarget: TUITapGestureTarget;
    FSaveCellFrame, FSaveDetailFrame: NSRect;
    FSaveSection, FSaveRow: Integer;
  public
    { Public declarations }
    procedure ParseData(AData: TStringList);
    function FindContinentByID(AID: Integer): TContinent;
    procedure CloseDetail(ASection, ARow: Integer);
    procedure OpenDetail(ASection, ARow: Integer);
    procedure UpdateMultiLabel;
    procedure CheckItem(ASection, ARow: Integer; AValue: Boolean);
  end;

var
  Form1130: TForm1130;
const
  Anim: Single = 0.5;

implementation

{$R *.fmx}

function TForm1130.FindContinentByID(AID: Integer): TContinent;
var
  I: Integer;
  f: Boolean;
  c: TContinent;
begin
  f := false;
  for I := 0 to FContinents.Count - 1 do
  begin
    if FContinents[I].ID = AID then
    begin
      f := True;
      Result := FContinents[I];
      Break;
    end;
  end;

  if not f then
  begin
    c := TContinent.Create;
    c.ID := AID;
    case c.ID of
      0: c.Title := 'North America';
      1: c.Title := 'Europe';
      2: c.Title := 'Asia';
      3: c.Title := 'Africa';
    end;
    FContinents.Add(c);
    Result := c;
  end;
end;

procedure TForm1130.FormCreate(Sender: TObject);
var
  d: TStringList;
begin
  FSaveDetailFrame := DetailView.View.frame;
  Overlay.Align := TAlignLayout.alClient;
  Overlay.View.setAlpha(0);
  FTapGestureTarget := TUITapGestureTarget.Create;
  FTapGesture := TUITapGestureRecognizer.Wrap(TUITapGestureRecognizer.Wrap(TUITapGestureRecognizer.OCClass.alloc).
    initWithTarget(FTapGestureTarget.GetObjectID, sel_getUid('tapped:')));
  FTapGesture.setCancelsTouchesInView(False);
  Overlay.View.addGestureRecognizer(FTapGesture);

  DetailView.View.setAlpha(0);

  d := TStringList.Create;
  d.Delimiter := ';';
  d.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Cities.csv');
  FContinents := TContinents.Create;
  ParseData(d);
  d.Free;

  ItemShadow.ImageFile := ExtractFilePath(ParamStr(0)) + 'shadow2.png';
  ItemShadow.X := -13;
  ItemShadow.Y := 25;
  ItemShadow.Height := 245;
  ItemShadow.Width := ItemShadow.Width + 28;
  CityTrip.Options.MinimumLineSpacing := 20;
  CityTrip.Options.MinimumInteritemSpacing := 20;
  CityTrip.Options.ItemSelectedBackgroundColor := TAlphaColorRec.Lightsteelblue;

  detailshadow.Position.X := -10;
  detailshadow.Width := detailshadow.Width + 20;
  detailshadow.BitmapFile := ExtractFilePath(ParamStr(0)) + 'shadow3.png';

  ItemCheck.CheckedImageFile := ExtractFilePath(ParamStr(0)) + 'checked.png';
  ItemCheck.UnCheckedImageFile := ExtractFilePath(ParamStr(0)) + 'unchecked.png';
end;

procedure TForm1130.FormDestroy(Sender: TObject);
begin
  if Assigned(FContinents) then
  begin
    FContinents.Free;
    FContinents := nil;
  end;

  if Assigned(FTapGestureTarget) then
  begin
    FTapGestureTarget.Free;
    FTapGestureTarget := nil;
  end;

  if Assigned(FTapGesture) then
  begin
    FTapGesture.release;
    FTapGesture := nil;
  end;
end;

procedure TForm1130.FormResize(Sender: TObject);
begin
  FSaveDetailFrame := DetailView.View.frame;
end;

procedure TForm1130.OpenDetail(ASection, ARow: Integer);
var
  a, av: NSString;
  c: UIcollectionviewCell;
  idx: NSIndexPath;
  fr: NSRect;
  ct: TCity;
begin
  idx := TNSIndexPath.Wrap(TNSIndexPath.OCClass.indexPathForRow(ARow, ASection));
  c := CityTrip.CollectionView.cellForItemAtIndexPath(idx);
  if Assigned(c) then
  begin
    ct := FContinents[ASection].Cities[ARow];
    DetailImage.BitmapFile := ExtractFilePath(ParamStr(0)) + ct.Image;
    DetailTitle.Text := ct.Title;
    DetailDescription.Text := ct.Description;
    DetailRating.BitmapFile := ExtractFilePath(ParamStr(0)) + ct.Rating;
    DetailPrice.Text := ct.Price;

    FSaveSection := ASection;
    FSaveRow := ARow;
    FSaveCellFrame := c.frame;
    fr := c.frame;
    fr.origin.x := fr.origin.x - CityTrip.CollectionView.contentOffset.x;
    fr.origin.y := fr.origin.y - CityTrip.CollectionView.contentOffset.y;
    DetailView.View.setFrame(fr);

    fr := FSaveDetailFrame;
    fr.origin.x := fr.origin.x + CityTrip.CollectionView.contentOffset.x;
    fr.origin.y := fr.origin.y + CityTrip.CollectionView.contentOffset.y;


    a := NSStr('CellAnimation');
    TUIView.OCClass.beginAnimations((a as ILocalObject).GetObjectID, nil);
    TUIView.OCClass.setAnimationCurve(UIViewAnimationCurveEaseOut);
    TUIView.OCClass.setAnimationTransition(UIViewAnimationTransitionFlipFromLeft, c, True);
    TUIView.OCClass.setAnimationDuration(Anim);
    c.setFrame(fr);
    c.setAlpha(0);
    TUIView.OCClass.commitAnimations;

    av := NSStr('ViewAnimation');
    TUIView.OCClass.beginAnimations((av as ILocalObject).GetObjectID, nil);
    TUIView.OCClass.setAnimationCurve(UIViewAnimationCurveEaseOut);
    TUIView.OCClass.setAnimationTransition(UIViewAnimationTransitionFlipFromLeft, DetailView.View, True);
    TUIView.OCClass.setAnimationDuration(Anim);
    DetailView.View.setAlpha(1);
    DetailView.View.setFrame(FSaveDetailFrame);
    TUIView.OCClass.commitAnimations;

    av := NSStr('OverLayAnimation');
    TUIView.OCClass.beginAnimations((av as ILocalObject).GetObjectID, nil);
    TUIView.OCClass.setAnimationCurve(UIViewAnimationCurveEaseOut);
    TUIView.OCClass.setAnimationDuration(Anim);
    Overlay.View.setAlpha(0.85);
    TUIView.OCClass.commitAnimations;
  end;
end;

procedure TForm1130.ParseData(AData: TStringList);
var
  I: Integer;
  str: String;
  arr: TArray<String>;
  continent: TContinent;
  city: TCity;
begin
  for I := 0 to AData.Count - 1 do
  begin
    str := AData[I];
    arr := str.Split([';']);
    continent := FindContinentByID(strtoint(arr[4]));
    if Assigned(continent) then
    begin
      city := TCity.Create;
      city.Rating := 'star'+arr[0] + '.png';
      city.Price := '€ '+arr[1];
      city.Image := arr[2] + '.jpg';
      city.Title := arr[3];
      city.Description := arr[5];
      continent.Cities.Add(city);
    end;
  end;
end;

procedure TForm1130.ToggleMultiSelectValueChanged(ASender: TObject;
  AValue: Boolean);
var
  c: TContinent;
  ct: TCity;
begin
  CityTrip.DeselectAllItems(false);
  for c in FContinents do
    for ct in c.Cities do
      ct.Selected := False;

  CityTrip.Options.AllowsMultipleSelection := not CityTrip.Options.AllowsMultipleSelection;
  UpdateMultiLabel;
  CityTrip.ReloadData;
end;

procedure TForm1130.UpdateMultiLabel;
var
  it: TCollectionViewItem;
  I: Integer;
  cnt: Integer;
begin
  MultiSelectLabel.Text := 'Multiselect';
  if CityTrip.Options.AllowsMultipleSelection then
  begin
    cnt := Length(CityTrip.SelectedItems);
    if cnt > 0 then
    begin
      MultiSelectLabel.Text := MultiSelectLabel.Text + ' [';
      for I := 0 to cnt - 1 do
      begin
        it := CityTrip.SelectedItems[I];
        MultiSelectLabel.Text := MultiSelectLabel.Text + FContinents[it.section].Cities[it.row].Title;
        if I < cnt - 1 then
          MultiSelectLabel.Text := MultiSelectLabel.Text + ', ';
      end;

      MultiSelectLabel.Text := MultiSelectLabel.Text + ']';
    end;
  end;
end;

procedure TForm1130.CheckItem(ASection, ARow: Integer; AValue: Boolean);
var
  chk: TTMSFMXNativeUICollectionViewTemplateControl;
begin
  FindContinentByID(ASection).Cities[ARow].Selected := AValue;
  chk := CityTrip.GetItemTemplateControl(ASection, ARow, 6);
  if Assigned(chk) and IsCheckBox(chk) then
    AsCheckBox(chk).CheckBox.setSelected(AValue);
end;

procedure TForm1130.CityTripApplyHeaderValue(
  Sender: TObject; AControl: TTMSFMXNativeUICollectionViewTemplateControl;
  ASection, ARow: Integer);
begin
  if IsLabel(AControl) and (AControl.GetViewID = 1) then
    AsLabel(AControl).Text := FindContinentByID(ASection).Title;
end;

procedure TForm1130.CityTripApplyItemValue(Sender: TObject;
  AControl: TTMSFMXNativeUICollectionViewTemplateControl; ASection,
  ARow: Integer);
var
  ct: TCity;
begin
  ct := FindContinentByID(ASection).Cities[ARow];
  if Assigned(ct) then
  begin
    if IsImageView(AControl) and (AControl.GetViewID = 3) then
      AsImageView(AControl).ImageFile := ExtractFilePath(ParamStr(0)) + ct.Image;

    if IsLabel(AControl) and (AControl.GetViewID = 2) then
      AsLabel(AControl).Text := ct.Title;

    if IsImageView(AControl) and (AControl.GetViewID = 4) then
      AsImageView(AControl).ImageFile := ExtractFilePath(ParamStr(0)) + ct.Rating;

    if IsLabel(AControl) and (AControl.GetViewID = 5) then
      AsLabel(AControl).Text := ct.Price;

    if IsCheckBox(AControl) then
    begin
      AsCheckBox(AControl).Visible := CityTrip.Options.AllowsMultipleSelection;
      AsCheckBox(AControl).Checked := ct.Selected;
    end;
  end;
end;

procedure TForm1130.CityTripDidDeselectItem(Sender: TObject; ASection,
  ARow: Integer);
begin
  if CityTrip.Options.AllowsMultipleSelection then
  begin
    CheckItem(ASection, ARow, False);
    UpdateMultiLabel;
  end;
end;

procedure TForm1130.CityTripDidselectItem(Sender: TObject;
  ASection, ARow: Integer);
begin
  if CityTrip.Options.AllowsMultipleSelection then
  begin
    CheckItem(ASection, ARow, True);
    UpdateMultiLabel
  end
  else
    OpenDetail(ASection, ARow);
end;

procedure TForm1130.CityTripGetNumberOfItemsInSection(
  Sender: TObject; ASection: Integer; var ANumberOfItems: Integer);
begin
  ANumberOfItems := FContinents[ASection].Cities.Count;
end;

procedure TForm1130.CityTripGetNumberOfSections(
  Sender: TObject; var ANumberOfSections: Integer);
begin
  ANumberOfSections := FContinents.Count;
end;

procedure TForm1130.CloseDetail(ASection, ARow: Integer);
var
  a, av: NSString;
  c: UIcollectionviewCell;
  idx: NSIndexPath;
  fr: NSRect;
begin
  idx := TNSIndexPath.Wrap(TNSIndexPath.OCClass.indexPathForRow(ARow, ASection));
  c := CityTrip.CollectionView.cellForItemAtIndexPath(idx);
  if Assigned(c) then
  begin

    fr := FSaveCellFrame;
    fr.origin.x := fr.origin.x - CityTrip.CollectionView.contentOffset.x;
    fr.origin.y := fr.origin.y - CityTrip.CollectionView.contentOffset.y;

    a := NSStr('CellAnimation');
    TUIView.OCClass.beginAnimations((a as ILocalObject).GetObjectID, nil);
    TUIView.OCClass.setAnimationCurve(UIViewAnimationCurveEaseOut);
    TUIView.OCClass.setAnimationTransition(UIViewAnimationTransitionFlipFromRight, c, True);
    TUIView.OCClass.setAnimationDuration(Anim);
    c.setFrame(FSaveCellFrame);
    c.setAlpha(1);
    TUIView.OCClass.commitAnimations;

    av := NSStr('ViewAnimation');
    TUIView.OCClass.beginAnimations((av as ILocalObject).GetObjectID, nil);
    TUIView.OCClass.setAnimationCurve(UIViewAnimationCurveEaseOut);
    TUIView.OCClass.setAnimationTransition(UIViewAnimationTransitionFlipFromRight, DetailView.View, True);
    TUIView.OCClass.setAnimationDuration(Anim);
    DetailView.View.setAlpha(0);
    DetailView.View.setFrame(fr);
    TUIView.OCClass.commitAnimations;

    av := NSStr('OverLayAnimation');
    TUIView.OCClass.beginAnimations((av as ILocalObject).GetObjectID, nil);
    TUIView.OCClass.setAnimationCurve(UIViewAnimationCurveEaseOut);
    TUIView.OCClass.setAnimationDuration(Anim);
    Overlay.View.setAlpha(0);
    TUIView.OCClass.commitAnimations;
  end;
end;

{ TContinent }

constructor TContinent.Create;
begin
  FCities := TCities.Create;
end;

destructor TContinent.Destroy;
begin
  FCities.Free;
  inherited;
end;

{ TUITapGestureTarget }

function TUITapGestureTarget.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IUITapGestureTarget)
end;

procedure TUITapGestureTarget.tapped(Sender: Pointer);
begin
  Form1130.CloseDetail(Form1130.FSaveSection, Form1130.FSaveRow);
end;

end.
