//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

// Demonstrates the "MultiDetailListItem" TListView appearance.
// Install the SampleListViewMultiDetailApparancePackage before opening this form.
unit MultiDetailMainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ListView.Types,
  FMX.StdCtrls, FMX.ListView, Data.Bind.GenData,
  Fmx.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.ListBox,
  FMX.TabControl, FMX.Objects, MultiDetailAppearanceU, FMX.MobilePreview;

type
  TForm594 = class(TForm)
    ToolBar1: TToolBar;
    ToggleEditMode: TSpeedButton;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    ListViewMultiDetail: TListView;
    LinkFillControlToField1: TLinkFillControlToField;
    SpeedButtonLiveBindings: TSpeedButton;
    ToolBar2: TToolBar;
    SpeedButtonFill: TSpeedButton;
    ImageRAD: TImage;
    procedure ToggleEditModeClick(Sender: TObject);
    procedure SpeedButtonFillClick(Sender: TObject);
    procedure SpeedButtonLiveBindingsClick(Sender: TObject);
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

procedure TForm594.SpeedButtonFillClick(Sender: TObject);
var
  I: Integer;
  LItem: TListViewItem;
begin
  // Code to fill TListView
  ListViewMultiDetail.BeginUpdate;
  try
    ListViewMultiDetail.Items.Clear;
    for I := 1 to 20 do
    begin
      LItem := ListViewMultiDetail.Items.Add;
      LItem.Text := Format('Text %d', [I]);
      // Update data managed by custom appearance
      LItem.Data[TMultiDetailAppearanceNames.Detail1] := Format('Detail1_%d', [I]);
      LItem.Data[TMultiDetailAppearanceNames.Detail2] := Format('Detail2_%d', [I]);
      LItem.Data[TMultiDetailAppearanceNames.Detail3] := Format('Detail3_%d', [I]);
      LItem.BitmapRef := ImageRAD.Bitmap;
    end;
  finally
    ListViewMultiDetail.EndUpdate;
  end;

end;

procedure TForm594.SpeedButtonLiveBindingsClick(Sender: TObject);
begin
  LinkFillControlToField1.BindList.FillList;
end;

procedure TForm594.ToggleEditModeClick(Sender: TObject);
begin
  ListViewMultiDetail.EditMode := not ListViewMultiDetail.EditMode;
end;

end.
