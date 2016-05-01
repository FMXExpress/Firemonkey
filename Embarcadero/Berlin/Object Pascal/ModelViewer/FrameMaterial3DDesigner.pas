//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit FrameMaterial3DDesigner;

interface

uses
  System.TypInfo, FMX.MaterialSources, System.IOUtils,
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Viewport3D, FMX.Controls3D, FMX.Ani, FMX.Objects3D,
  FMX.ListBox, FMX.Layouts, FMX.Colors, FMX.Edit, FMX.Objects, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Graphics, FMX.NumberBox,
  FMX.Text;

type

  TFrameMaterialDesigner = class(TFrame)
    CompListBox: TListBox;
    PanelOfProperties: TPanel;
    itemColor: TListBoxItem;
    itemTexture: TListBoxItem;
    itemLight: TListBoxItem;
    procedure CompListBoxChange(Sender: TObject);
  private
    FMaterialObject : TMaterialSource;
    procedure DoGetProperties(AObject : TObject);
    // setter adders
    procedure DoAddSetterInteger(APropName : string);
    procedure DoAddSetterColor(APropName : string);
    procedure DoAddSetterBitmap(APropName : string);

    // setters
    procedure DidIntegerChange(Sender : TObject);
    procedure DidColorChange(Sender : TObject);
    procedure DidBitmapChange(Sender : TObject);

    // utils
    procedure DragAccept(Sender: TObject; const Data: TDragObject;
                    const Point: TPointF; var Operation: TDragOperation);
    procedure DropBitmap(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure UpdateMaterial;
  public
    TargetObject : TModel3D;
    type
      TMaterialType = (mtColor, mtTexture, mtLight);
  end;

implementation

{$R *.fmx}

procedure TFrameMaterialDesigner.UpdateMaterial;
var
  Mesh : TMesh;
begin
  for Mesh in TargetObject.MeshCollection do
    Mesh.MaterialSource := FMaterialObject;
  for Mesh in TargetObject.MeshCollection do
    Mesh.MaterialSource := FMaterialObject;
end;

procedure TFrameMaterialDesigner.CompListBoxChange(Sender: TObject);
var
  SelectedType : TMaterialType;
begin
  if (Assigned(FMaterialObject)) then
    FMaterialObject.DisposeOf;
  FMaterialObject := nil;

  SelectedType := TMaterialType(CompListBox.ItemIndex);
  case SelectedType of
    TMaterialType.mtColor : FMaterialObject := TColorMaterialSource.Create(TargetObject);
    TMaterialType.mtTexture : FMaterialObject := TTextureMaterialSource.Create(TargetObject);
    TMaterialType.mtLight : FMaterialObject := TLightMaterialSource.Create(TargetObject);
  end;

  DoGetProperties(FMaterialObject);


  FMaterialObject.Parent := TargetObject;
  UpdateMaterial;
end;

procedure TFrameMaterialDesigner.DoGetProperties(AObject: TObject);
var
  APropList:PPropList;
  I,Count:Integer;
  PropTypeName : String;
  PropName : String;
begin

  APropList := nil;
  Count := 0;

  for i := PanelOfProperties.ComponentCount - 1 downto 0 do
  begin
    PanelOfProperties.Components[i].DisposeOf;
  end;

  // look for settable properties

  if AObject <> nil then
  begin
    try
      Count := GetPropList(AObject.ClassInfo,tkProperties,nil);
      GetMem(APropList, Count * Sizeof(PPropInfo));
      GetPropList(AObject.ClassInfo, tkProperties, APropList);
      for I:=0 to Count-1 do
      begin
        PropTypeName := APropList[i]^.PropType^^.NameFld.ToString;
        PropName := APropList[i]^.NameFld.ToString;
        if PropTypeName = 'Integer' then
          DoAddSetterInteger(PropName);
        if PropTypeName = 'TAlphaColor' then
          DoAddSetterColor(PropName);
        if PropTypeName = 'TBitmap' then
          DoAddSetterBitmap(PropName);
      end;
    finally
      FreeMem(APropList, Count * Sizeof(PPropInfo));
    end;
  end;
end;

// setter adders

procedure TFrameMaterialDesigner.DoAddSetterInteger(APropName: string);
var
  NBox : TNumberBox;
  CompLabel: TLabel;
  CompCt: integer;
  val : Integer;
begin
  NBox := nil;
  CompLabel := nil;
  try
    CompCt := PanelOfProperties.ComponentCount;
    CompLabel := TLabel.Create(PanelOfProperties);
    CompLabel.Text := APropName;
    CompLabel.Parent := PanelOfProperties;
    CompLabel.SetBounds(20, 20 * CompCt+20, 100, 30);


    NBox := TNumberBox.Create(PanelOfProperties);
    NBox.Name := APropName;
    NBox.Parent := PanelOfProperties;
    NBox.ValueType := TNumValueType.Integer;
    NBox.TextAlign := TTextAlign.Center;
    val := GetPropValue(FMaterialObject, APropName, False);
    NBox.Min := 0;
    NBox.Max := 100;
    NBox.Value := val;
    NBox.SetBounds(110, 20 * CompCt + 20, 120, 30);
    NBox.OnChange := DidIntegerChange;
  except
    On E : Exception do begin
      NBox.DisposeOf;
      CompLabel.DisposeOf;
    end;
  end;
end;

procedure TFrameMaterialDesigner.DoAddSetterColor(APropName: string);
var
  CBox : TComboColorBox;
  CompLabel: TLabel;
  CompCt: integer;
begin
  CBox := nil;
  CompLabel := nil;
  try
    CompCt := PanelOfProperties.ComponentCount;
    CompLabel := TLabel.Create(PanelOfProperties);
    CompLabel.Text := APropName;
    CompLabel.Parent := PanelOfProperties;
    CompLabel.SetBounds(20, 20 * CompCt+20, 100, 30);


    CBox := TComboColorBox.Create(PanelOfProperties);
    CBox.Name := APropName;
    CBox.Parent := PanelOfProperties;
    CBox.Color := GetOrdProp(FMaterialObject, APropName);
    CBox.SetBounds(110, 20 * CompCt + 20, 120, 30);
    CBox.OnChange := DidColorChange;
  except
    On E : Exception do begin
      CBox.DisposeOf;
      CompLabel.DisposeOf;
    end;
  end;
end;

procedure TFrameMaterialDesigner.DoAddSetterBitmap(APropName: string);
var
  CBox : TComboBox;
  CompLabel: TLabel;
  Btn : TButton;
  CompCt: integer;
begin
  CBox := nil;
  CompLabel := nil;
  try
    CompCt := PanelOfProperties.ComponentCount;
    CompLabel := TLabel.Create(PanelOfProperties);
    CompLabel.Parent := PanelOfProperties;
    CompLabel.Text := APropName;
    CompLabel.SetBounds(20, 20 * CompCt+20, 100, 30);

    Btn := TButton.Create(PanelOfProperties);
    Btn.Parent := PanelOfProperties;
    Btn.Text := 'Edit ' + APropName;
    Btn.Name := APropName;
    Btn.SetBounds(110, 20 * CompCt + 20, 120, 30);
    Btn.OnClick := DidBitmapChange;
    Btn.OnDragOver := DragAccept;
    Btn.OnDragDrop := DropBitmap;
  except
    On E : Exception do begin
      CBox.DisposeOf;
      CompLabel.DisposeOf;
    end;
  end;
end;


// setters
procedure TFrameMaterialDesigner.DidIntegerChange(Sender: TObject);
begin
  SetPropValue(FMaterialObject, TNumberBox(Sender).Name, TNumberBox(Sender).Value);
end;

procedure TFrameMaterialDesigner.DidColorChange(Sender: TObject);
begin
  SetOrdProp(FMaterialObject, TColorComboBox(Sender).Name, TComboColorBox(Sender).Color);
end;

procedure TFrameMaterialDesigner.DidBitmapChange(Sender: TObject);
var
  bmp : TBitmap;
  od : TOpenDialog;
  ctx : TRttiContext;
  typ : TRttiType;
  prop : TRttiProperty;
begin
  od := nil;
  try
    od := TOpenDialog.Create(Owner);
    od.Filter := TBitmapCodecManager.GetFilterString;
    if od.Execute then
    begin
      bmp := TBitmap.Create(100,100);
      bmp.LoadFromFile(od.FileName);
      ctx := TRttiContext.Create;
      typ := ctx.GetType(FMaterialObject.ClassType);
      prop := typ.GetProperty(TControl(Sender).Name);
      prop.SetValue(FMaterialObject,bmp);
    end;
  finally
    od.DisposeOf;
  end;
end;


procedure TFrameMaterialDesigner.DragAccept(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Move;
end;

procedure TFrameMaterialDesigner.DropBitmap(Sender: TObject; const Data: TDragObject; const Point: TPointF);
var
  bmp : TBitmap;
  ctx : TRttiContext;
  typ : TRttiType;
  prop : TRttiProperty;
begin
  bmp := TBitmap.Create(100,100);
  if (Length(Data.Files) > 0) and  TFile.Exists(Data.Files[0]) then
    bmp.LoadFromFile(Data.Files[0]);
    ctx := TRttiContext.Create;
    typ := ctx.GetType(FMaterialObject.ClassType);
    prop := typ.GetProperty(TControl(Sender).Name);
    prop.SetValue(FMaterialObject,bmp);
  if (Data.Source <> nil) and (Data.Source is TImage) then
  begin
    bmp := (Data.Source as TImage).Bitmap;
    ctx := TRttiContext.Create;
    typ := ctx.GetType(FMaterialObject.ClassType);
    prop := typ.GetProperty(TControl(Sender).Name);
    prop.SetValue(FMaterialObject,bmp);
  end;
end;

end.
