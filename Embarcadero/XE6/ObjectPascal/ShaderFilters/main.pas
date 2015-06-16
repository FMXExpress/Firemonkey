
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit main;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.UITypes, System.UIConsts, System.Types,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.Filter, FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Styles,
  FMX.Objects, FMX.Colors, FMX.Ani, FMX.Grid,   FMX.Types3D, FMX.Layers3D, FMX.Filter.Standard,
  FMX.Graphics;

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    listCat: TListBox;
    Splitter4: TSplitter;
    listFX: TListBox;
    Source: TImage;
    Target: TImage;
    Dest: TImage;
    Splitter1: TSplitter;
    panelSettings: TListBox;
    Panel4: TPanel;
    btnBench: TButton;
    labelBench: TLabel;
    panelTransition: TPanel;
    btnPlay: TButton;
    AniIndicator1: TAniIndicator;
    Resources1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure listCat1Click(Sender: TObject);
    procedure listFX1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBench1Click(Sender: TObject);
    procedure btnPlay1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoFloatTrackChange(Sender: TObject);
    procedure DoColorChanged(Sender: TObject);
    procedure DoPointChanged(Sender: TObject; var X, Y: Single);
    procedure UpdatePreview;
  public
    { Public declarations }
    Filter: TFilter;
    Rec: TFilterRec;
    TransitionAni: TFloatAnimation;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  S: TStrings;
begin
  Dest.Bitmap.Assign(Source.Bitmap);

  S := TStringList.Create;
  TFilterManager.FillCategory(S);
  listCat.Assign(S);
  S.Free;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Filter <> nil then
    Filter.Free;
end;

procedure TfrmMain.listCat1Click(Sender: TObject);
var
  S: TStrings;
begin
  if (TransitionAni <> nil) and (TransitionAni.Running) then
    TransitionAni.Stop;

  if listCat.ItemIndex < 0 then Exit;
  listFX.ItemIndex := -1;
  S := TStringList.Create;
  TFilterManager.FillFiltersInCategory(listCat.ListItems[listCat.ItemIndex].Text, S);
  listFX.Assign(S);
  S.Free;
end;

procedure TfrmMain.listFX1Click(Sender: TObject);
var
  i: integer;
  Item: TListboxItem;
  Shape: TSelectionPoint;
  LIsTransition : Boolean;
  LIsTransitionEnable : Boolean;
begin
  if (TransitionAni <> nil) and (TransitionAni.Running) then
    TransitionAni.Stop;

  if listFX.ItemIndex < 0 then Exit;
  // Delete setting controls
  if TransitionAni <> nil then
    FreeAndNil(TransitionAni);
  Dest.DeleteChildren;
  panelSettings.Clear;
  // Create Filter
  if Filter <> nil then
    Filter.Free;
  Filter := TFilterManager.FilterByName(listFX.ListItems[listFX.ItemIndex].Text);
  // Create settings
  Rec := TFilterManager.FilterClassByName(listFX.ListItems[listFX.ItemIndex].Text).FilterAttr;
//  labelName.Text := Rec.Desc;
  for i := 0 to High(Rec.Values) do
  with Rec.Values[i] do
  begin
    case ValueType of
      TFilterValueType.Float:
        begin
          Item := TListboxItem.Create(nil);
          Item.Parent := panelSettings;
          Item.Height := 64;
          Item.StyleLookup := 'trackitem';
          Item.ApplyStyleLookup;
          Item.Text := Name + ' (' + FloatToStrF(Min.AsExtended, ffFixed, 10, 2) + '-' + FloatToStrF(Max.AsExtended, ffFixed, 10, 2) + ')';
          Item.StylesData['desc'] := desc;
          if Assigned(Item.FindStyleResource('track')) then
          begin
            TTrackBar(Item.FindStyleResource('track')).Min := Min.AsExtended;
            TTrackBar(Item.FindStyleResource('track')).Max := Max.AsExtended;
            TTrackBar(Item.FindStyleResource('track')).Value := Default.AsExtended;
            TTrackBar(Item.FindStyleResource('track')).Tag := i;
            Item.StylesData['track'] := TValue.From<TNotifyEvent>(DoFloatTrackChange);
            if Name = 'Progress' then
            begin
              TransitionAni := TFloatAnimation.Create(Self);
              TransitionAni.Parent := TTrackBar(Item.FindStyleResource('track'));
              TransitionAni.StartValue := 0;
              TransitionAni.StopValue := 100;
              TransitionAni.PropertyName := 'Value';
              TransitionAni.Duration := 2;
              TransitionAni.Loop := true;
              TransitionAni.AutoReverse := true;
            end;
          end;
          Item.Opacity := 0;
          Item.AnimateFloatDelay('Opacity', 1, 0.5, i / 10);
        end;
      TFilterValueType.Point:
        begin
          Shape := TSelectionPoint.Create(Self);
          Shape.Parent := Dest;
          Shape.GripSize := 7;
          Shape.Position.X := Default.AsType<TPointF>.x;
          Shape.Position.Y := Default.AsType<TPointF>.y;
          Shape.OnTrack := DoPointChanged;
          Shape.Tag := i;
        end;
      TFilterValueType.Color:
        begin
          Item := TListboxItem.Create(nil);
          Item.Parent := panelSettings;
          Item.Height := 64;
          Item.StyleLookup := 'coloritem';
          Item.ApplyStyleLookup;
          Item.Text := Name;
          Item.StylesData['desc'] := desc;
          Item.StylesData['color'] := Default;
          Item.StylesData['color'] := TValue.From<TNotifyEvent>(DoColorChanged);
          if Assigned(Item.FindStyleResource('color')) then
            TFmxObject(Item.FindStyleResource('color')).Tag := i;
          Item.Opacity := 0;
          Item.AnimateFloatDelay('Opacity', 1, 0.5, i / 10);
        end;
    end;
  end;
  UpdatePreview;
  LIsTransition := (listCat.ListItems[listCat.ItemIndex].Text = 'Transition');
  LIsTransitionEnable := (panelTransition.Visible and Assigned(TransitionAni));
  if (panelTransition.Visible <> LIsTransition) or
    (panelTransition.Visible <> (LIsTransition and LIsTransitionEnable)) then
  begin
    if panelTransition.Visible then
    begin
      btnPlay.AnimateFloatWait('Opacity', 0, 0.3);
      panelTransition.AnimateFloatWait('Height', 0, 0.3);
      panelTransition.Visible := false;
    end
    else
    begin
      panelTransition.Visible := true;
      panelTransition.Height := 0;
      panelTransition.AnimateFloat('Height', 36, 0.3);
      btnPlay.Opacity := 0;
      btnPlay.AnimateFloatDelay('Opacity', 1, 0.3, 0.3);
    end;
  end;
end;

procedure TfrmMain.DoFloatTrackChange(Sender: TObject);
begin
  Filter.ValuesAsFloat[Rec.Values[TFmxObject(Sender).Tag].Name] := TTrackBar(Sender).Value;
  UpdatePreview;
end;

procedure TfrmMain.DoColorChanged(Sender: TObject);
begin
  Filter.ValuesAsColor[Rec.Values[TFmxObject(Sender).Tag].Name] := TFmxObject(Sender).Data.AsType<TAlphaColor>;
  UpdatePreview;
end;

procedure TfrmMain.DoPointChanged(Sender: TObject; var X, Y: Single);
begin
  Filter.ValuesAsPoint[Rec.Values[TFmxObject(Sender).Tag].Name] := TPointF.Create(TControl(Sender).Position.X, TControl(Sender).Position.Y);
  UpdatePreview;
end;

procedure TfrmMain.UpdatePreview;
begin
  if Filter <> nil then
  begin
    // set input
    Filter.ValuesAsBitmap['Input'] := Source.Bitmap;
    // set Target only for transition
    Filter.ValuesAsBitmap['Target'] := Target.Bitmap;
    // apply, get and show result
    Dest.Bitmap := TBitmap(Filter.ValuesAsBitmap['output']);
  end;
end;

function GetTickCount: single;
var
  H, M, S, MS: word;
begin
  DecodeTime(time, H, M, S, MS);
  Result := ((((H * 60 * 60) + (M * 60) + S) * 1000) + MS);
end;

procedure TfrmMain.btnBench1Click(Sender: TObject);
const
  Steps = 200;
var
  i: integer;
  StartTime, NewTime, Time: single;
begin
  btnBench.Enabled := false;
  labelBench.Visible := false;
  AniIndicator1.Visible := true;
  AniIndicator1.Enabled := true;

  Time := 0;
  for i := 1 to Steps do
  begin
    if Filter <> nil then
    begin
      // set input
      Filter.ValuesAsBitmap['input'] := Source.Bitmap;
      // apply (usually apply method called when you order Output }
      StartTime := GetTickCount / 1000;
      Filter.Apply;
      NewTime := GetTickCount / 1000;

      Time := Time + (NewTime - StartTime);

      Application.ProcessMessages;
    end;
  end;

  AniIndicator1.Enabled := false;
  AniIndicator1.Visible := false;

  labelBench.Visible := true;
  labelBench.Opacity := 0;
  labelBench.AnimateFloat('Opacity', 1, 0.3);
  labelBench.Text := 'Average: ' + FloatToStrF((Time) / Steps, ffFixed, 6, 4) + 'ms';
  btnBench.Enabled := true;
end;

procedure TfrmMain.btnPlay1Click(Sender: TObject);
begin
  { Play Transition }
  if btnPlay.Text = 'Play' then
  begin
    TransitionAni.Start;
    btnPlay.Text := 'Stop';
  end
  else
  begin
    TransitionAni.Stop;
    btnPlay.Text := 'Play';
  end;
end;

end.
