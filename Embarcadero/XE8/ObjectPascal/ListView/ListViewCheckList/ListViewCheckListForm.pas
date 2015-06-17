//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ListViewCheckListForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ListView.Types,
  Data.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.ListView, System.Generics.Collections, FMX.MobilePreview;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure ListView1UpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
  private
    { Private declarations }
    FChecked: TList<Integer>;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

constructor TForm1.Create(AOwner: TComponent);
begin
  FChecked := TList<Integer>.Create;
  inherited;
end;

destructor TForm1.Destroy;
begin
  FChecked.Free;
  inherited;
end;

procedure TForm1.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  // Toggle visibility of accessory when item is clicked
  // Save checked state of item
  if AItem.Objects.AccessoryObject.Visible then
  begin
    AItem.Objects.AccessoryObject.Visible := False;
    FChecked.Remove(AItem.Index);
  end
  else
  begin
    AItem.Objects.AccessoryObject.Visible := True;
    FChecked.Add(AItem.Index)
  end;
end;

procedure TForm1.ListView1UpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
begin
  // In order for text to be truncated properly, shorten text object
  AItem.Objects.TextObject.Width := AItem.Objects.TextObject.Width -
    (5 + AItem.Objects.AccessoryObject.Width);
  // Restore checked state when device is rotated.
  // When listview is resized because of rotation, accessory properties will be reset to default values
  AItem.Objects.AccessoryObject.Visible := FChecked.Contains(AItem.Index);

end;

end.
