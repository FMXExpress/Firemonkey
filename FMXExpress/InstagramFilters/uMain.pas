Unit uMain;

Interface

Uses
    System.SysUtils,    System.Types,   System.UITypes, System.Classes, System.Variants,
    FMX.Types,          FMX.Controls,   FMX.Forms,      FMX.Graphics,   FMX.Dialogs, FMX.Filter,
    FMX.Filter.Effects, FMX.Effects,    FMX.Objects,    FMX.StdCtrls,   FMX.Layouts,
    FMX.ExtCtrls,       FMX.TabControl, FMX.Memo,       System.Math,
  FMX.MediaLibrary.Actions, FMX.ActnList, System.Actions, FMX.StdActns;

Type
  TMainForm = class   ( TForm )
    Image1            : TImage;
    Image7            : TImage;
    ContrastEffect7   : TContrastEffect;
    HueAdjustEffect7  : THueAdjustEffect;
    CropRectangle     : TRectangle;
    ImageControl      : TImage;
    Image8            : TImage;
    Image9            : TImage;
    Image10           : TImage;
    HueAdjustEffect10 : THueAdjustEffect;
    HueAdjustEffect8  : THueAdjustEffect;
    ContrastEffect8   : TContrastEffect;
    ContrastEffect9   : TContrastEffect;
    Selection         : TSelection;
    RectTop           : TRectangle;
    RectBottom        : TRectangle;
    RectLeft          : TRectangle;
    RectRight         : TRectangle;
    EditorTabs        : TTabControl;
    CropTab           : TTabItem;
    FilterTab         : TTabItem;
    FilterRectangle   : TRectangle;
    FilterImage       : TImage;
    FilterLayout      : TLayout;
    ShareTab: TTabItem;
    ToolBar1: TToolBar;
    ForwardBTN: TButton;
    BackBTN: TButton;
    StyleBook1: TStyleBook;
    ShareImage: TImage;
    ShareBTN: TButton;
    Layout1: TLayout;
    Label1: TLabel;
    Layout2: TLayout;
    Layout3: TLayout;
    HorzScrollBox1: THorzScrollBox;
    Layout4: TLayout;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Layout5: TLayout;
    Label5: TLabel;
    Image2: TImage;
    ContrastEffect2: TContrastEffect;
    Image3: TImage;
    HueAdjustEffect3: THueAdjustEffect;
    Image4: TImage;
    SepiaEffect4: TSepiaEffect;
    Image5: TImage;
    MonochromeEffect5: TMonochromeEffect;
    Image6: TImage;
    HueAdjustEffect6: THueAdjustEffect;
    SepiaEffect6: TSepiaEffect;
    Layout6: TLayout;
    Layout7: TLayout;
    Layout8: TLayout;
    Layout9: TLayout;
    Layout10: TLayout;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Timer1: TTimer;
    ActionList1: TActionList;
    ShowShareSheetAction1: TShowShareSheetAction;
    Action1: TAction;
    TakePhotoFromCameraAction1: TTakePhotoFromCameraAction;
    Button4: TButton;
    Button5: TButton;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    Procedure Image1Click    ( Sender: TObject );
    Procedure Image5Click    ( Sender: TObject );
    Procedure Image4Click    ( Sender: TObject );
    Procedure Image3Click    ( Sender: TObject );
    Procedure Image2Click    ( Sender: TObject );
    Procedure Image7Click    ( Sender: TObject );
    Procedure Image6Click    ( Sender: TObject );
    Procedure Image10Click   ( Sender: TObject );
    Procedure SelectionChange( Sender: TObject );
    Procedure FormShow       ( Sender: TObject );
    procedure Image9Click(Sender: TObject);
    procedure Image8Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BackBTNClick(Sender: TObject);
    procedure ForwardBTNClick(Sender: TObject);
    procedure TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    procedure ShowShareSheetAction1BeforeExecute(Sender: TObject);
  Private
    { Private declarations }
  Public
    procedure CropImage;
    procedure CropIt;
    { Public declarations }
    Procedure RemoveEffects;
    Procedure Trace          ( S: String );
  End;

Var
   MainForm: TMainForm;

Implementation

{$R *.fmx}

procedure TMainForm.CropIt;
begin
  CropImage;
  EditorTabs.SetActiveTabWithTransition( FilterTab, TTabTransition.ttSlide );
end;

procedure TMainForm.TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
begin
  ImageControl.Bitmap.Assign(Image);
end;

procedure TMainForm.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  ImageControl.Bitmap.Assign(Image);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
Timer1.Enabled := False;

ContrastEffect2.ProcessEffect(Image2.Bitmap.Canvas,Image2.Bitmap,1);

HueAdjustEffect3.ProcessEffect(Image3.Bitmap.Canvas,Image3.Bitmap,1);

SepiaEffect4.ProcessEffect(Image4.Bitmap.Canvas,Image4.Bitmap,1);

MonochromeEffect5.ProcessEffect(Image5.Bitmap.Canvas,Image5.Bitmap,1);

HueAdjustEffect6.ProcessEffect(Image6.Bitmap.Canvas,Image6.Bitmap,1);
SepiaEffect6.ProcessEffect(Image6.Bitmap.Canvas,Image6.Bitmap,1);


ContrastEffect7.ProcessEffect(Image7.Bitmap.Canvas,Image7.Bitmap,1);
HueAdjustEffect7.ProcessEffect(Image7.Bitmap.Canvas,Image7.Bitmap,1);


ContrastEffect8.ProcessEffect(Image8.Bitmap.Canvas,Image8.Bitmap,1);
HueAdjustEffect8.ProcessEffect(Image8.Bitmap.Canvas,Image8.Bitmap,1);

ContrastEffect9.ProcessEffect(Image9.Bitmap.Canvas,Image9.Bitmap,1);

HueAdjustEffect10.ProcessEffect(Image10.Bitmap.Canvas,Image10.Bitmap,1);

end;

Procedure TMainForm.Trace ( S: String );
Begin
     //Memo1.Lines.Append ( S );
End;

procedure TMainForm.CropImage;
Var
   R1,R2            : TRectF;
   XScale, YScale   : Single;
   XBorder, YBorder : Integer;
   ActScale         : Single;

Begin
     XScale    := ImageControl.Bitmap.Width  / ImageControl.Width;
     YScale    := ImageControl.Bitmap.Height / ImageControl.Height;
     ActScale  := Max ( XScale, YScale );

     // Calculation of black area
     // when XY ratio doesnt match

     XBorder := 0;
     YBorder := 0;

     // **************************


     IF XScale > YScale THEN
       Begin
            XBorder := 0;
            YBorder := ROUND ( ( ( ImageControl.Height -  ( ImageControl.Bitmap.Height / ActScale ) ) / 2 ) );
       End
     ELSE
       Begin
            YBorder := 0;
            XBorder := ROUND ( ( ( ImageControl.Width - ( ImageControl.Bitmap.Width / ActScale ) ) / 2 ) );
       End;

     // **************************



     // Calculation of source Rect

     R1        := TRectF.Create ( PointF ( ( TRUNC ( Selection.Position.X ) - XBorder ) * ActScale, ( TRUNC ( Selection.Position.Y ) - YBorder ) * ActScale ) );
     R1.Width  := TRUNC ( Selection.Width  * ActScale );
     R1.Height := TRUNC ( Selection.Height * ActScale );

     // **************************


     // Calculation of destination Rect

     R2.Left   := 0;
     R2.Right  := FilterImage.Bitmap.Canvas.Width;
     R2.Top    := 0;
     R2.Bottom := FilterImage.Bitmap.Canvas.Height;

     // ******************************************

//     FilterImage.Bitmap.SetSize(Trunc(R1.Right),Trunc(R1.Bottom));

     FilterImage.Bitmap.Canvas.BeginScene ();
     FilterImage.Bitmap.Canvas.DrawBitMap ( ImageControl.Bitmap.Canvas.Bitmap, R1, R2, 1 );
     FilterImage.Bitmap.Canvas.EndScene ();

end;

procedure TMainForm.ForwardBTNClick(Sender: TObject);
begin
     if EditorTabs.ActiveTab = FilterTab then
       begin
         ShareImage.Bitmap.Assign(FilterImage.Bitmap);
         EditorTabs.SetActiveTabWithTransition( ShareTab, TTabTransition.ttSlide, TTabTransitionDirection.tdNormal );
         BackBTN.Visible := True;
         ForwardBTN.Visible := False;
       end;

     if EditorTabs.ActiveTab = CropTab then
      begin
        //EditorTabs.SetActiveTabWithTransition( FilterTab, TTabTransition.ttSlide, TTabTransitionDirection.tdNormal );
        BackBTN.Visible := True;
        CropIt;
      end;

     if EditorTabs.ActiveTab = ShareTab then
      begin
        //EditorTabs.SetActiveTabWithTransition( FilterTab, TTabTransition.ttSlide, TTabTransitionDirection.tdNormal );

      end;


end;

procedure TMainForm.BackBTNClick(Sender: TObject);
begin
     if EditorTabs.ActiveTab = FilterTab then
      begin
       EditorTabs.SetActiveTabWithTransition( CropTab, TTabTransition.ttSlide, TTabTransitionDirection.tdReversed );
       BackBTN.Visible := False;
      end;

     if EditorTabs.ActiveTab = ShareTab then
      begin
       EditorTabs.SetActiveTabWithTransition( FilterTab, TTabTransition.ttSlide, TTabTransitionDirection.tdReversed );
       BackBTN.Visible := True;
       ForwardBTN.Visible := True;
      end;

     if EditorTabs.ActiveTab = CropTab then
      begin
      end;
end;

procedure TMainForm.ShowShareSheetAction1BeforeExecute(Sender: TObject);
begin
ShowShareSheetAction1.Bitmap.Assign(ShareImage.Bitmap);
end;

procedure TMainForm.Image2Click(Sender: TObject);
begin
RemoveEffects;

ContrastEffect2.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);
end;

procedure TMainForm.Image3Click(Sender: TObject);
begin
RemoveEffects;
HueAdjustEffect3.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);


end;

procedure TMainForm.Image4Click(Sender: TObject);
begin
RemoveEffects;
SepiaEffect4.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);
end;

procedure TMainForm.Image5Click(Sender: TObject);
begin
RemoveEffects;
MonochromeEffect5.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);
end;

procedure TMainForm.Image6Click(Sender: TObject);
begin
RemoveEffects;

HueAdjustEffect6.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);
SepiaEffect6.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);


end;

procedure TMainForm.Image7Click(Sender: TObject);
begin
RemoveEffects;

ContrastEffect7.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);
HueAdjustEffect7.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);


end;

procedure TMainForm.Image8Click(Sender: TObject);
begin
  RemoveEffects;

ContrastEffect8.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);
HueAdjustEffect8.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);

end;

procedure TMainForm.Image9Click(Sender: TObject);
begin
  RemoveEffects;

ContrastEffect9.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);


end;

procedure TMainForm.RemoveEffects;
begin
     CropImage;
end;

Procedure TMainForm.SelectionChange ( Sender: TObject );
Begin
     IF Selection.Position.X + Selection.Width > ImageControl.Width THEN
       Selection.Width := ImageControl.Width - Selection.Position.X;

     IF Selection.Position.Y + Selection.Height > ImageControl.Height THEN
       Selection.Height := ImageControl.Height - Selection.Position.Y;

     IF Selection.Height <= Selection.Width THEN
       Selection.Height := Selection.Width
     ELSE Selection.Width := Selection.Height;

     RectTop.Height        := Selection.Position.Y;
     RectBottom.Position.Y := Selection.Position.Y+Selection.Height;
     RectBottom.Height     := ImageControl.Height-RectBottom.Position.Y;

     RectLeft.Width        := Selection.Position.X;
     RectRight.Position.X  := Selection.Position.X+Selection.Width;
     RectRight.Width       := ImageControl.Width-RectRight.Position.X;
End;

Procedure TMainForm.FormShow ( Sender: TObject );
Begin
     SelectionChange ( Self );
End;

Procedure TMainForm.Image10Click ( Sender: TObject );
Begin
  RemoveEffects;
HueAdjustEffect10.ProcessEffect(FilterImage.Bitmap.Canvas,FilterImage.Bitmap,1);


End;

Procedure TMainForm.Image1Click ( Sender: TObject );
Begin
     RemoveEffects;
End;



End.
