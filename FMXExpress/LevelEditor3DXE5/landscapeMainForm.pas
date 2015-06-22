unit landscapeMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Forms3D, FMX.Types3D, FMX.Objects3D,
  FMX.Layers3D, FMX.StdCtrls, FMX.Controls3D, FMX.MaterialSources;

type
  TAddable = (aTree, aHouse);

  TMain = class(TForm3D)
    sun: TLight;
    tree: TCone;
    house: TModel3D;
    Layer3D1: TLayer3D;
    BtnTree: TButton;
    BtnHouse: TButton;
    Label1: TLabel;
    BtnSun: TButton;
    Label2: TLabel;
    BtnUndo: TButton;
    houseMat01: TLightMaterialSource;
    land: TPlane;
    landimage: TTextureMaterialSource;
    procedure landMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure landMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure landMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single;
      RayPos, RayDir: TVector3D);
    procedure Form3DCreate(Sender: TObject);
    procedure BtnTreeClick(Sender: TObject);
    procedure BtnHouseClick(Sender: TObject);
    procedure BtnSunClick(Sender: TObject);
    procedure BtnUndoClick(Sender: TObject);
  private
    FMouseDown: Boolean;
    FAddable : TAddable;
    FLastX, FLastZ : Double;
    FSeparation: Double;
    FHouseAngle: Integer;
    FSunAngle: Integer;
  public
  end;

var
  Main: TMain;

implementation

{$R *.fmx}

procedure TMain.Form3DCreate(Sender: TObject);
begin
  FMouseDown := False;
  FAddable := aTree;
  FHouseAngle := 30;
  FSunAngle := -45;
  FSeparation := 0.3;
end;

procedure TMain.landMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  FMouseDown := True;
  FLastX := -99;
  FLastZ := -99;
end;

procedure TMain.landMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
var
  newObj: TProxyObject;
  HitPos: TVector3D;
begin
  if FMouseDown then begin
    land.RayCastIntersect(RayPos, RayDir, HitPos);
    // if moved far enough
    if (Abs(HitPos.X - FLastX) > FSeparation) or (Abs(HitPos.Z - FLastZ) > FSeparation) then begin
      // add new object as a proxy object
      newObj := TProxyObject.Create(self);
      land.AddObject(newObj);
      case FAddable of
        aTree:  begin
          newObj.SourceObject := tree;
          newObj.Scale.Assign(tree.Scale);
          newObj.RotationAngle.X := 90;//tree.RotationAngle.X;
          newObj.RotationAngle.Y := 0;//tree.RotationAngle.Y;
          newObj.RotationAngle.Z := 0;//tree.RotationAngle.Z;
          FSeparation := 0.6;
        end;
        aHouse: begin
          //newObj.SourceObject := wall; // need to point at material !!!
          // this use to point to the walls in the original but the object got mangled somehow
          newObj.SourceObject := house; // need to point at material !!!
          newObj.Scale.Assign(house.Scale);
          newObj.RotationAngle.X := 90;
          newObj.RotationAngle.Y := 356.8679;//house.RotationAngle.Y;
          newObj.RotationAngle.Z := FHouseAngle;
          newObj.Scale.X := 0.02;
          newObj.Scale.Y := 0.02;
          newObj.Scale.Z := 0.02;
          FSeparation := 1.0;
        end;
      end;

      newObj.Position.Z := newObj.SourceObject.Position.Z;
      //newObj.Position.X := HitPos.X;
      //newObj.Position.Y := -HitPos.Z;  // Z -> Y
      // added +2 offet to get under the mouse cursor on my machine
      newObj.Position.X := HitPos.X+2;
      newObj.Position.Y := -HitPos.Z+2;  // Z -> Y

      newObj.HitTest := False;

      FLastX := HitPos.X;
      FLastZ := HitPos.Z;
    end;
  end;
end;

procedure TMain.landMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  FMouseDown := False;
end;

procedure TMain.BtnHouseClick(Sender: TObject);
begin
  FAddable := aHouse;
  FHouseAngle := (FHouseAngle + 45) mod 360;
  Label1.Text := IntToStr(FHouseAngle);
end;

procedure TMain.BtnTreeClick(Sender: TObject);
begin
  FAddable := aTree;
end;

procedure TMain.BtnUndoClick(Sender: TObject);
begin
  if land.ChildrenCount > 2 then
    land.RemoveObject(land.ChildrenCount-1);
end;

procedure TMain.BtnSunClick(Sender: TObject);
begin
  FSunAngle := (FSunAngle + 10) mod 360;
  sun.RotationAngle.Y := FSunAngle;
  Label2.Text := IntToStr(FSunAngle);
end;

end.
