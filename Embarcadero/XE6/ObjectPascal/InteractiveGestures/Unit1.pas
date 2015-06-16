unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus, Generics.Collections, FMX.Objects,
  FMX.Layouts, FMX.Memo, System.Math, FMX.StdCtrls, FMX.Graphics;

type
  TForm36 = class(TForm)
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure MenuItem1Click(Sender: TObject);
    procedure Panel1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    FImages: TList<TImage>;
    FLastPosition: TPointF;
    FLastDistance: Integer;
    procedure AddPicture(files: TStrings);
    procedure handlePan(eventInfo: TGestureEventInfo);
    procedure handleZoom(eventInfo: TGestureEventInfo);
    procedure handleRotate(eventInfo: TGestureEventInfo);
    procedure handlePressAndTap(eventInfo: TGestureEventInfo);
  public
  end;

var
  Form36: TForm36;

implementation

{$R *.fmx}

procedure TForm36.AddPicture(files: TStrings);
var
  i: Integer;
  image: TImage;
begin
  FImages := TList<TImage>.Create;
  for i := 0 to files.Count - 1 do
  begin
    image := TImage.Create(Panel1);
    image.Parent := Panel1;
    image.Bitmap := TBitmap.CreateFromFile(files[i]);
    image.Height := 200;
    image.Width := 200;
    image.WrapMode := TImageWrapMode.Stretch;
    image.Position := TPosition.Create(PointF(10, 10));
    FImages.Add(image);
  end;
end;

procedure TForm36.handleRotate(eventInfo: TGestureEventInfo);
var
  LObj: IControl;
  image: TImage;
begin
  LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
  if LObj is TImage then
  begin
    image := TImage(LObj.GetObject);
    image.RotationAngle := RadToDeg(-EventInfo.Angle);
  end;
end;

procedure TForm36.handlePan(EventInfo: TGestureEventInfo);
var
  LObj: IControl;
  image: TImage;
begin
  LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
  if LObj is TImage then
  begin
    if not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then
    begin
      image := TImage(LObj.GetObject);
      //Set the X coordinate.
      image.Position.X := image.Position.X + (EventInfo.Location.X - FLastPosition.X);
      if image.Position.X < 0 then
        image.Position.X := 0;
      if image.Position.X > (Panel1.Width - image.Width) then
        image.Position.X := Panel1.Width - image.Width;

        //Set the Y coordinate.
      image.Position.Y := image.Position.Y + (EventInfo.Location.Y - FLastPosition.Y);
      if image.Position.Y < 0 then
        image.Position.Y := 0;
      if image.Position.Y > (Panel1.Height - image.Height) then
        image.Position.Y := Panel1.Height - image.Height;
    end;

    FLastPosition := EventInfo.Location;
  end;
end;

procedure TForm36.handlePressAndTap(EventInfo: TGestureEventInfo);
var
  LObj: IControl;
  image: TImage;
begin
  LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
  if LObj is TImage then
  begin
    image := TImage(LObj.GetObject);
    FImages.Remove(image);
    FreeAndNil(image);
  end;
end;

procedure TForm36.handleZoom(EventInfo: TGestureEventInfo);
var
  LObj: IControl;
  image: TImage;
begin
  LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
  if LObj is TImage then
  begin
    if not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then
    begin
      image := TImage(LObj.GetObject);
      image.Width := image.Width + (EventInfo.Distance - FLastDIstance)/2;
      image.Height := image.Height + (EventInfo.Distance - FLastDIstance)/2;
      image.Position.X := image.Position.X - (EventInfo.Distance - FLastDIstance)/2;
      image.Position.Y := image.Position.Y - (EventInfo.Distance - FLastDIstance)/2;
    end;
  end;

  FLastDIstance := EventInfo.Distance;
end;

procedure TForm36.MenuItem1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Images|*.jpg;*.jpeg;*.bmp;*.png';
  if OpenDialog1.Execute then
  begin
    AddPicture(OpenDialog1.files);
  end;
end;

procedure TForm36.Panel1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
   if EventInfo.GestureID = igiPan then
    handlePan(EventInfo)
  else if EventInfo.GestureID = igiZoom then
    handleZoom(EventInfo)
  else if EventInfo.GestureID = igiRotate then
    handleRotate(EventInfo)
  else if EventInfo.GestureID = igiPressAndTap then
    handlePressAndTap(EventInfo);
end;

end.
