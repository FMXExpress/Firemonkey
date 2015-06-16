
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Unit7;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Types3D, FMX.Objects3D,
  FMX.Objects, FMX.Layers3D, FMX.Ani, Data.DBXInterBase, Data.FMTBcd,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope, Data.Bind.DBLinks, Fmx.Bind.DBLinks,
  FMX.Layouts, FMX.Grid, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase,
  IdSMTPRelay, Data.DB, Datasnap.Provider, Datasnap.DBClient, Data.SqlExpr,
  FMX.Memo, System.Rtti, System.Bindings.Outputs, FMX.StdCtrls, FMX.Controls3D,
  FMX.MaterialSources, FMX.Forms3D, System.Math.Vectors;

type
  TForm7 = class(TForm3D)
    Light1: TLight;
    Light2: TLight;
    Layer3D1: TLayer3D;
    LeftPlane: TPlane;
    RightPlane: TPlane;
    BackPlane: TPlane;
    BottomPlane: TPlane;
    TopPlane: TPlane;
    ScrollBar1: TScrollBar;
    Camera1: TCamera;
    Light3: TLight;
    Light4: TLight;
    Layout3D1: TLayout3D;
    RotateBoxX: TCheckBox;
    RotateBoxY: TCheckBox;
    RotateBoxZ: TCheckBox;
    Label1: TLabel;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    Layer3D2: TLayer3D;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    BindScopeDB1: TBindScopeDB;
    StringGrid2: TStringGrid;
    Layer3D3: TLayer3D;
    ClientDataSet2: TClientDataSet;
    DataSource2: TDataSource;
    BindScopeDB2: TBindScopeDB;
    DBLinkImageControl1GRAPHIC1: TBindDBImageLink;
    Layer3D4: TLayer3D;
    Layer3D5: TLayer3D;
    Memo1: TMemo;
    DBLinkMemo1NOTES1: TBindDBMemoLink;
    Layer3D6: TLayer3D;
    DBLinkStringGrid11: TBindDBGridLink;
    StringGrid3: TStringGrid;
    DBLinkStringGrid21: TBindDBGridLink;
    ClientDataSet3: TClientDataSet;
    DataSource3: TDataSource;
    BindScopeDB3: TBindScopeDB;
    DBLinkStringGrid31: TBindDBGridLink;
    Timer1: TTimer;
    ImageControl1: TImageControl;
    LinkControlToField2: TLinkControlToField;
    procedure Form3DCreate(Sender: TObject);
    procedure BackPlaneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure BackPlaneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single;
      RayPos, RayDir: TVector3D);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

var
  dX1 : Single = 0.001;
  dY1 : Single = 0.001;
  dZ1 : Single = 0.001;
  dX2 : Single = -0.001;
  dY2 : Single = 0.001;
  dZ2 : Single = -0.001;
  FPS : Single = 0;
  Down : TPointF;

procedure TForm7.Form3DCreate(Sender: TObject);
begin
  Application.OnIdle := AppIdle;
end;

procedure TForm7.Timer1Timer(Sender: TObject);
var
  Done : Boolean;
begin
  AppIdle(Self,Done);  // MacOSX
end;

procedure TForm7.BackPlaneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  Down := PointF(X, Y);
end;

procedure TForm7.BackPlaneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
begin
  if (ssLeft in Shift) then
  begin
    { rotate Z }
    Camera.RotationAngle.X := Camera.RotationAngle.X + ((Y - Down.Y) * 0.2);
    { rotate X }
    Camera.RotationAngle.Y := Camera.RotationAngle.Y + ((X - Down.X) * 0.2);
    Down := PointF(X, Y);
  end;
end;

procedure TForm7.AppIdle(Sender: TObject; var Done: Boolean);
var
  V : Single;
begin
  V := ScrollBar1.Value/5;
  if RotateBoxX.IsChecked then
    Layout3D1.RotationAngle.X := Layout3D1.RotationAngle.X+0.01*V;
  if RotateBoxY.IsChecked then
    Layout3D1.RotationAngle.Y := Layout3D1.RotationAngle.Y+0.01*V;
  if RotateBoxZ.IsChecked then
    Layout3D1.RotationAngle.Z := Layout3D1.RotationAngle.Z+0.01*V;
end;

end.

