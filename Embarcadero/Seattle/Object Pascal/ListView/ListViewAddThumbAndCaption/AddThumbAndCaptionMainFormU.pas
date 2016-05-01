//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

// This projects adds a thumbnail image and caption to all listview items, in event handlers

unit AddThumbAndCaptionMainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ListView.Types,
  FMX.StdCtrls, FMX.ListView, Data.Bind.GenData,
  Fmx.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.ListBox, FMX.Graphics,
  FMX.TabControl, FMX.Objects, RatingsAppearanceU, FMX.MobilePreview;

type
  TForm594 = class(TForm)
    ToolBar1: TToolBar;
    ToggleEditMode: TSpeedButton;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    ListViewBottomDetail: TListView;
    ImageRatings: TImage;
    LinkFillControlToField1: TLinkFillControlToField;
    SpeedButtonLiveBindings: TSpeedButton;
    ToolBar2: TToolBar;
    SpeedButtonFill: TSpeedButton;
    ImageRAD: TImage;
    procedure ToggleEditModeClick(Sender: TObject);
    procedure SpeedButtonFillClick(Sender: TObject);
    procedure SpeedButtonLiveBindingsClick(Sender: TObject);
    procedure ListViewBottomDetailUpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
    procedure LinkFillControlToField1FilledListItem(Sender: TObject;
      const AEditor: IBindListEditorItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form594: TForm594;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses System.Math;

const
  sThumbNailName = 'TI';
  sCaption = 'CA';

procedure TForm594.LinkFillControlToField1FilledListItem(Sender: TObject;
  const AEditor: IBindListEditorItem);
var
  LItem: TListViewItem;
  LThumb: TListItemImage;
  LCaption: TListItemText;
  LField: TBindSourceAdapterField;
  LFieldName: TBindSourceAdapterField;
  LBitmap: TPersistent;
  LName: string;
begin
  // Code to assign to the list item as it is being updated by livebindings
  if AEditor.CurrentIndex >= 0 then
  begin
    LItem := ListViewBottomDetail.Items[AEditor.CurrentIndex];
    LThumb := (LItem.Objects.FindObject(sThumbNailName) as TListItemImage);
    LCaption := (LItem.Objects.FindObject(sCaption) as TListItemText);
    LField := Self.PrototypeBindSource1.InternalAdapter.FindField('Bitmap1');
    LFieldName := Self.PrototypeBindSource1.InternalAdapter.FindField('BitmapName1');
    if (LField <> nil) and (LThumb <> nil) then
    begin
      if (LThumb.Bitmap = nil) or (not LThumb.OwnsBitmap) then
      begin
        LThumb.OwnsBitmap := True;
        LThumb.Bitmap := TBitmap.Create(0, 0);
      end;
      // Get the bitmap from the bind source field
      if LField.GetTValue.TryAsType<TPersistent>(LBitmap) then
        // Copy to the list item
        LThumb.Bitmap.Assign(LBitmap)
      else
        LThumb.Bitmap.Assign(nil);
    end;

    if (LFieldName <> nil) and (LCaption <> nil) then
    begin
      // Get the text
      if LFieldName.GetTValue.TryAsType<string>(LName) then
        LCaption.Text := LName
      else
        LCaption.Text := '';
    end;

  end;

end;

// Code to add rendering items to each listviewitem
procedure TForm594.ListViewBottomDetailUpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
var
  LImage: TListItemImage;
  LCaption: TListItemText;
begin
  LImage := AItem.Objects.FindObject(sThumbNailName) as TListItemImage;
  if LImage = nil then
  begin
    LImage := TListItemImage.Create(AItem);
    LImage.Name := sThumbNailName;
    LImage.Align := TListItemAlign.Trailing;
    LImage.PlaceOffset.Y := 5;
    LImage.PlaceOffset.X := -30;
    LImage.Width := 20;
    LImage.Height := 20;
  end;

  LCaption := AItem.Objects.FindObject(sCaption) as TListItemText;
  if LCaption = nil then
  begin
    LCaption := TListItemText.Create(AItem);
    LCaption.Name := sCaption;
    LCaption.Align := TListItemAlign.Trailing;
    LCaption.VertAlign := TListItemAlign.Trailing;
    LCaption.PlaceOffset.X := -10;
    LCaption.TextAlign := TTextAlign.Center;
    LCaption.Trimming := TTextTrimming.Character;
    LCaption.IsDetailText := True;
    LCaption.Width := 60;
    LCaption.Height := 18;
  end;

end;

// Code to populate list in code
procedure TForm594.SpeedButtonFillClick(Sender: TObject);
var
  I: Integer;
  LItem: TListViewItem;
begin
  LinkFillControlToField1.Active := False;
  // Code to fill TListView
  for I := 1 to 20 do
  begin
    LItem := ListViewBottomDetail.Items.Add;
    LItem.Text := Format('Text %d', [I]);
    LItem.Detail := Format('Detail %d', [I]);
    LItem.BitmapRef := ImageRAD.Bitmap;
    LItem.Data[sCaption] := Format('thumb %d', [I]);
    // LItem.Data[sThumbNailName] := Format('Caption %d', [I]);
    // Do the following instead of  above line.  Above line will copy the image
    (LItem.Objects.FindObject(sThumbNailName) as TListItemImage).OwnsBitmap := False;
    (LItem.Objects.FindObject(sThumbNailName) as TListItemImage).Bitmap := ImageRAD.Bitmap;
  end;

end;

procedure TForm594.SpeedButtonLiveBindingsClick(Sender: TObject);
begin
  LinkFillControlToField1.Active := True;
end;


procedure TForm594.ToggleEditModeClick(Sender: TObject);
begin
  ListViewBottomDetail.EditMode := not ListViewBottomDetail.EditMode;
end;

end.
