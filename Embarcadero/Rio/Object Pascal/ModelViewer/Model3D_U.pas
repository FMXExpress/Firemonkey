//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Model3D_U;

interface

uses
{$IFDEF MSWINDOWS}
  Xml.Win.msxmldom
{$ELSE}
  Xml.omnixmldom
{$ENDIF},
  System.Math, Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layers3D, FMX.Types3D,
  FMX.Objects, FMX.Objects3D, FMX.Ani,
  FMX.ASE.Importer,
  FMX.DAE.Importer,
  FMX.OBJ.Importer,
  FMX.Effects, FMX.Layouts, FMX.StdCtrls, FMX.Viewport3D, FMX.Controls3D,
  FMX.Forms3D, FMX.MaterialSources, FrameMaterial3DDesigner, System.Math.Vectors;

type
  TModel3DTest = class(TForm3D)
    spbLoadFromFile: TSpeedButton;
    Light: TLight;
    theDummy: TDummy;
    MainCamera: TCamera;
    spbClear: TSpeedButton;
    tbDimension: TTrackBar;
    lblScale: TLabel;
    GadgetsCircle: TCircle;
    GadgetsLayout: TLayout;
    ptArrowDown: TPath;
    ptArrowUp: TPath;
    ptArrowRight: TPath;
    ptArrowLeft: TPath;
    DummyY: TDummy;
    DummyX: TDummy;
    StatusBar: TStatusBar;
    lblStatus: TLabel;
    sbArrowDown: TSpeedButton;
    mainStyleBook: TStyleBook;
    sbArrowUp: TSpeedButton;
    sbArrowLeft: TSpeedButton;
    sbArrowRight: TSpeedButton;
    GadgetsLayer3D: TLayer3D;
    MainLayer3D: TLayer3D;
    spbEditMaterial: TSpeedButton;
    MaterialDesignerLayout: TLayout;
    ToolBarDone: TToolBar;
    btnDone: TButton;
    FrameMaterialDesigner1: TFrameMaterialDesigner;
    procedure spbLoadFromFileClick(Sender: TObject);
    procedure spbClearClick(Sender: TObject);
    procedure tbDimensionChange(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);

    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Form3DMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ptArrowDownClick(Sender: TObject);
    procedure ptArrowRightClick(Sender: TObject);
    procedure ptArrowUpClick(Sender: TObject);
    procedure ptArrowLeftClick(Sender: TObject);
    procedure Form3DClose(Sender: TObject; var Action: TCloseAction);
    procedure spbEditMaterialClick(Sender: TObject);
    procedure btnDoneClick(Sender: TObject);

  private
    procedure ClearMeshes(var Model3D : TModel3D );
  public

    { Public declarations }
  end;
const
  MaterialName = 'ModelMaterial';
var
  Model3DTest: TModel3DTest;
  FModel3D : TModel3D;
  lastX, lastY : Single;
  moving : Boolean;
  Mouse : Boolean;
  Down : TPointF;
implementation

{$R *.fmx}

procedure TModel3DTest.Form3DClose(Sender: TObject; var Action: TCloseAction);
begin
  FModel3D.Free();
end;

procedure TModel3DTest.Form3DCreate(Sender: TObject);
begin
  FModel3D := TModel3D.Create(theDummy);
  FModel3D.Parent := theDummy;
  MaterialDesignerLayout.Width := 0;
end;



procedure TModel3DTest.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Mouse := True;
  Down := PointF(X,Y);
end;

procedure TModel3DTest.Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if (ssLeft in Shift) and Mouse then
  begin
    DummyX.RotationAngle.X := DummyX.RotationAngle.X - (Y - Down.Y)* 0.3 ;
    DummyY.RotationAngle.Y := DummyY.RotationAngle.Y + (X - Down.X)* 0.3 ;
    Down.X := X;
    Down.Y := Y;
  end;
end;

procedure TModel3DTest.Form3DMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Mouse := False;
end;

procedure TModel3DTest.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera.Position.Z := Camera.Position.Z + ((WheelDelta / 120) * 0.3);
end;

procedure TModel3DTest.ptArrowDownClick(Sender: TObject);
begin
  TAnimator.AnimateFloat(MainCamera, 'Position.Y',Maincamera.Position.Y - 1,0.2);
end;

procedure TModel3DTest.ptArrowLeftClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWait(MainCamera, 'Position.X',MainCamera.Position.X +1,0.2);
end;

procedure TModel3DTest.ptArrowRightClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWait(MainCamera, 'Position.X',MainCamera.Position.X -1,0.2);
end;

procedure TModel3DTest.ptArrowUpClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWait(MainCamera, 'Position.Y',MainCamera.Position.Y +1,0.2);
end;

procedure TModel3DTest.spbLoadFromFileClick(Sender: TObject);
var
  od : TOpenDialog;
  FileName : String;
begin
  od := nil;
  try
    od := TOpenDialog.Create(self);
    od.Filter :='3D files|*.obj;*.dae;*.ase';
    if od.Execute then
    begin
      lblStatus.Text := 'Loading... please wait';
      Application.ProcessMessages;
      FModel3D.Clear;
      FModel3D.RotationAngle.Vector := Vector3D(0,0,0);

      DummyX.RotationAngle.Vector := Vector3D(0,0,0);
      DummyY.RotationAngle.Vector := Vector3D(0,0,0);

      ClearMeshes(FModel3D);
      FileName := od.FileName;
      FModel3D.LoadFromFile(FileName);

      lblStatus.Text := 'Ready';
    end;
  finally
    od.Free;
  end;
end;

procedure TModel3DTest.ClearMeshes(var Model3D : TModel3D );
var
  I: Integer;
  LMeshes: TList<TMesh>;
  LMesh: TMesh;
begin
  LMeshes := TList<TMesh>.Create;
  try
    for I := 0 to Model3D.ChildrenCount - 1 do
    begin
      if Model3D.Children[I] is TMesh then
      begin
        LMeshes.Add(TMesh(Model3D.Children[I]));
      end;
    end;
    if LMeshes.Count > 0 then begin
      for LMesh in LMeshes do
      begin
        Model3D.RemoveObject(LMesh);
        LMesh.Free;
      end
    end;
  finally
    LMeshes.Free;
  end;
end;


procedure TModel3DTest.spbClearClick(Sender: TObject);
begin
  FModel3D.Clear;

  ClearMeshes(FModel3D);
end;

procedure TModel3DTest.spbEditMaterialClick(Sender: TObject);
begin
  if Assigned(FModel3D.MeshCollection) then
  begin
    FrameMaterialDesigner1.TargetObject := FModel3D;
    TAnimator.AnimateFloat(MaterialDesignerLayout, 'Width',MainLayer3D.Width);
  end
  else
    ShowMessage('Load a model first');
end;

procedure TModel3DTest.tbDimensionChange(Sender: TObject);
var
  FScale : single;
begin
  FScale := Power(10, (Sender as TTrackBar).Value);

  //FScale := (Sender as TTrackBar).Value;

  FModel3D.Scale.X := FScale;
  FModel3D.Scale.Y := FScale;
  FModel3D.Scale.Z := FScale;

  lblScale.Text := 'Scale : ' + FloatToStrF(FScale,TFloatFormat.ffGeneral,5,4);
end;

procedure TModel3DTest.btnDoneClick(Sender: TObject);
begin
  TAnimator.AnimateFloat(MaterialDesignerLayout, 'Width',0);
end;

end.
