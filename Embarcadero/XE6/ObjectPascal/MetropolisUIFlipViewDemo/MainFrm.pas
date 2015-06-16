
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
{******************************************************************************}
{                                                                              }
{                         Delphi FireMonkey Platform                           }
{                                                                              }
{                     Flip View Control for Metropolis UI                      }
{                                                                              }
{                Copyright(c) 2012 Embarcadero Technologies, Inc.              }
{                                                                              }
{                                                                              }
{      This sample describes how you can create slider control. It has two     }
{  kind of slider:                                                             }
{                                                                              }
{     1. Slider using firemonkey transition effect (Filters). He has got       }
{        one slide. Any kind of availible transition effect (which have        }
{        'progress' and 'target' properties) applied on it.                    }
{     2. Slider using scrolling animation. It's work at a base is of two slide }
{        They alternately switch.                                              }
{                                                                              }
{      For storage and data access TAbstractDataSource is used. It provides    }
{  access to elements and navigation of slides. FMX.FlipView.Data haves one    }
{  implementation with acces to image content. Current example the slider      }
{  works only with images using TAbstractDataSource. However, it can be        }
{  altered for display of any content type.                                    }
{                                                                              }
{******************************************************************************}

unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.TabControl,
  FMX.Edit, FMX.Objects, FMX.Filter.Effects, FMX.Ani, FMX.ListBox,
  FMX.FlipView.Data, System.Actions, FMX.ActnList, FMX.StdCtrls, FMX.Graphics;

const
  AvailibleSliderTransitionEffects: array [1..21] of TClass = (TBlindTransitionEffect,
    TLineTransitionEffect, TCrumpleTransitionEffect, TFadeTransitionEffect,
    TRippleTransitionEffect, TDissolveTransitionEffect, TCircleTransitionEffect,
    TDropTransitionEffect, TSwirlTransitionEffect, TMagnifyTransitionEffect,
    TWaveTransitionEffect, TBloodTransitionEffect, TBlurTransitionEffect,
    TWaterTransitionEffect, TWiggleTransitionEffect, TShapeTransitionEffect,
    TRotateCrumpleTransitionEffect, TBlindTransitionEffect,
    TBandedSwirlTransitionEffect, TSaturateTransitionEffect,
    TPixelateTransitionEffect);

type

  TSplashImagesLoader = class;

  TFormMain = class(TForm)
    Slider: TRectangle;
    Slide1: TImage;
    ButtonPrev: TButton;
    ButtonNext: TButton;
    Content: TLayout;
    Slide2: TImage;
    LabelTitle: TLabel;
    LayoutRight: TLayout;
    LayoutLeft: TLayout;
    CBTransitionEffect: TCheckBox;
    CBEffects: TComboBox;
    FormLayout: TLayout;
    LightBox: TRectangle;
    LoadingIndicator: TAniIndicator;
    LoadingBox: TLayout;
    LabelLoadingStatus: TLabel;
    CBSlideShow: TCheckBox;
    TimerSlideShow: TTimer;
    ActionList: TActionList;
    ActionNext: TAction;
    ActionPrevious: TAction;
    LabelSliderStatus: TLabel;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure CBTransitionEffectChange(Sender: TObject);
    procedure CBEffectsChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerSlideShowTimer(Sender: TObject);
    procedure ActionPreviousExecute(Sender: TObject);
    procedure ActionNextExecute(Sender: TObject);
    procedure CBSlideShowChange(Sender: TObject);
  private
    FSplashImageLoader: TSplashImagesLoader;
    FImages: TAbstractDataSource;
    // Swipe slider
    FCurrentSlide: TImage;
    FTransitionAni: TFloatAnimation;
    procedure Init1;
    procedure NextSlide1;
    procedure PrevSlide1;
  private
    // Transition Effect slider
    FTransitionEffect: TImageFXEffect;
    FEffectAni: TFloatAnimation;
    procedure Init2;
    procedure NextSlide2;
    procedure PrevSlide2;
    procedure DoStopEffect(Sender: TObject);
  private
    procedure UpdateNavigationButtonVisibility;
    // Reset Progress status from current effect
    procedure ResetProgressEffect;
  public
    destructor Destroy; override;
    procedure ShowSplashLoading;
    procedure HideSplashLoading;
  end;

  /// <summary>
  ///  Thread images loader. Show and hide lightbox with loading state.
  /// </summary>
  TSplashImagesLoader = class (TThread)
  private
    FImages: TAbstractDataSource;
    FForm: TFormMain;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property Form: TFormMain read FForm write FForm;
    property DataSource: TAbstractDataSource read FImages write FImages;
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.Rtti;

{$R *.fmx}

procedure TFormMain.ActionNextExecute(Sender: TObject);
begin
  TimerSlideShow.Enabled := False;
  try
    if CBTransitionEffect.IsChecked then
      NextSlide2
    else
      NextSlide1;
  finally
    if CBSlideShow.IsChecked then
      TimerSlideShow.Enabled := True;
  end;
end;

procedure TFormMain.ActionPreviousExecute(Sender: TObject);
begin
  if CBTransitionEffect.IsChecked then
    PrevSlide2
  else
    PrevSlide1;
end;

procedure TFormMain.CBTransitionEffectChange(Sender: TObject);
begin
  CBEffects.Enabled := CBTransitionEffect.IsChecked;
  if CBTransitionEffect.IsChecked then
    Init2
  else
    Init1;
end;

procedure TFormMain.CBEffectsChange(Sender: TObject);
begin
  Init2;
end;

procedure TFormMain.CBSlideShowChange(Sender: TObject);
begin
  TimerSlideShow.Enabled := CBSlideShow.IsChecked;
end;

destructor TFormMain.Destroy;
begin
  FSplashImageLoader.Terminate;
  FSplashImageLoader.Free;
  FImages.Free;
  inherited Destroy;
end;

procedure TFormMain.DoStopEffect(Sender: TObject);
begin
  ResetProgressEffect;
  if FImages.HasItems then
    Slide1.Bitmap.Assign(FImages.Current as TBitmap);
end;

procedure TFormMain.FormCreate(Sender: TObject);

  procedure LoadAvailibleEffects;
  var
    I: Integer;
  begin
    for I := Low(AvailibleSliderTransitionEffects) to High(AvailibleSliderTransitionEffects) do
      CBEffects.Items.Add(AvailibleSliderTransitionEffects[I].ClassName);
  end;

begin
  FImages := TImageDataSource.Create;
  // Create thread with image loader
  FSplashImageLoader := TSplashImagesLoader.Create;
  FSplashImageLoader.Form := Self;
  FSplashImageLoader.DataSource := FImages;
  // Start loading
  FSplashImageLoader.Start;
  LoadAvailibleEffects;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  Slide1.Width := Slider.Width;
  Slide1.Height := Slider.Height;
  Slide2.Width := Slider.Width;
  Slide2.Height := Slider.Height;
  Content.Width := Slide1.Width + Slide2.Width;
  Content.Height := Slider.Height;
end;

procedure TFormMain.HideSplashLoading;
begin
  LightBox.Align := TAlignLayout.alContents;
  LightBox.Visible := False;
  LoadingIndicator.Enabled := False;
  Init1;
  LabelSliderStatus.Visible := not FImages.HasItems;
end;

procedure TFormMain.Init2;

  procedure RemoveOldSliderElements;
  begin
    FCurrentSlide := nil;
    if Assigned(FTransitionAni) then
      FreeAndNil(FTransitionAni);
    if Assigned(FTransitionEffect) then
    begin
      FreeAndNil(FTransitionEffect);
      FEffectAni := nil;
    end;
  end;

  procedure CreateNewSliderElements;
  var
    EffectClass: TFmxObjectClass;
  begin
    if Assigned(CBEffects.Selected) then
      EffectClass := TFmxObjectClass(FindClass(CBEffects.Selected.Text));
    if Assigned(EffectClass) then
    begin
      FTransitionEffect := TImageFXEffect(EffectClass.Create(Self));
      FTransitionEffect.Parent := Slide1;
    end;

    FEffectAni := TFloatAnimation.Create(Self);
    FEffectAni.Parent := FTransitionEffect;
    FEffectAni.AnimationType := TAnimationType.Out;
    FEffectAni.Interpolation := TInterpolationType.Linear;
    FEffectAni.Duration := 0.5;
    FEffectAni.StartValue := 0;
    FEffectAni.StartValue := 100;
    FEffectAni.PropertyName := 'Progress';
    FEffectAni.StartFromCurrent := False;
    FEffectAni.OnFinish := DoStopEffect;
    ResetProgressEffect;
  end;

  procedure ReallignSliderElements;
  begin
    Content.BoundsRect := Slider.LocalRect;
    Slide1.Align := TAlignLayout.alClient;
    Slide1.Visible := True;
    if FImages.HasItems and (FImages.Current is TBitmap) then
      Slide1.Bitmap.Assign(FImages.Current as TBitmap);
    Slide2.Visible := False;
  end;

begin
  RemoveOldSliderElements;
  CreateNewSliderElements;
  ReallignSliderElements;
  UpdateNavigationButtonVisibility;
end;

procedure TFormMain.NextSlide1;
var
  Current, Next: TImage;
  TargetBitmap: TBitmap;
begin
  if not FImages.HasItems or FImages.IsLast then
    Exit;

  Current := FCurrentSlide;
  if Slide1 = Current then
    Next := Slide2
  else
    Next := Slide1;

  FCurrentSlide := Next;
  FImages.Forward;
  UpdateNavigationButtonVisibility;

  // Make swap slides
  Slider.BeginUpdate;
  try
    Content.Position.Point := Pointf(0, 0);
    Current.Position.Point := PointF(0, 0);
    Next.Position.Point := PointF(Slider.Width, 0);
  finally
    Slider.EndUpdate;
  end;

  if FImages.Current is TBitmap then
    Next.Bitmap.Assign(FImages.Current as TBitmap);

  // Setting animator
  FTransitionAni.StartValue := 0;
  FTransitionAni.StopValue := -Slider.Width;
  FTransitionAni.Start;
end;

procedure TFormMain.NextSlide2;
var
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  TargetBitmapProperty: TRttiProperty;
  TargetBitmap: TBitmap;
begin
  if not FImages.HasItems or FImages.IsLast then
    Exit;

  if FImages.Next is TBitmap then
  begin
    RttiCtx := TRttiContext.Create;
    try
      RttiType := RttiCtx.GetType(FTransitionEffect.ClassInfo);
      TargetBitmapProperty := RttiType.GetProperty('Target');
      TargetBitmap := TBitmap(TargetBitmapProperty.GetValue(FTransitionEffect).AsObject);
      TargetBitmap.Assign(FImages.Next as TBitmap);
    finally
      RttiCtx.Free;
    end;
  end;
  FImages.Forward;
  UpdateNavigationButtonVisibility;
  FEffectAni.StartValue := 0;
  FEffectAni.StopValue := 100;
  FEffectAni.Start;
end;

procedure TFormMain.PrevSlide1;
var
  Current, Next: TImage;
begin
  if not FImages.HasItems or FImages.IsFirst then
    Exit;

  Current := FCurrentSlide;
  if Slide1 = Current then
    Next := Slide2
  else
    Next := Slide1;

  FCurrentSlide := Next;
  FImages.Backward;

  if FImages.Current is TBitmap then
    Next.Bitmap.Assign(FImages.Current as TBitmap);

  // Make swape slide
  Slider.BeginUpdate;
  try
    Content.Position.Point := Pointf(-Slider.Width, 0);
    Current.Position.Point := PointF(Slider.Width, 0);
    Next.Position.Point := PointF(0, 0);
  finally
    Slider.EndUpdate;
  end;

  // setting animator
  FTransitionAni.StartValue := -Slider.Width;
  FTransitionAni.StopValue := 0;
  FTransitionAni.Start;

  UpdateNavigationButtonVisibility;
end;

procedure TFormMain.PrevSlide2;
var
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  TargetBitmapProperty: TRttiProperty;
  TargetBitmap: TBitmap;
begin
  if not FImages.HasItems or FImages.IsFirst then
    Exit;

  if FImages.Previous is TBitmap then
  begin
    RttiCtx := TRttiContext.Create;
    try
      RttiType := RttiCtx.GetType(FTransitionEffect.ClassInfo);
      TargetBitmapProperty := RttiType.GetProperty('Target');
      if Assigned(TargetBitmapProperty) then
      begin
        TargetBitmap := TBitmap(TargetBitmapProperty.GetValue(FTransitionEffect).AsObject);
        if Assigned(TargetBitmap) then
          TargetBitmap.Assign(FImages.Previous as TBitmap);
      end;
    finally
      RttiCtx.Free;
    end;
  end;
  FImages.Backward;
  FEffectAni.StartValue := 0;
  FEffectAni.StopValue := 100;
  FEffectAni.Start;

  UpdateNavigationButtonVisibility;
end;

procedure TFormMain.Init1;

  procedure RemoveOldSliderElements;
  begin
    if Assigned(FEffectAni) then
      FreeAndNil(FEffectAni);
    if Assigned(FTransitionEffect) then
      FreeAndNil(FTransitionEffect);
  end;

  procedure CreateNewSliderElements;
  begin
    FTransitionAni := TFloatAnimation.Create(Self);
    FTransitionAni.Parent := Content;
    FTransitionAni.AnimationType := TAnimationType.Out;
    FTransitionAni.Interpolation := TInterpolationType.Linear;
    FTransitionAni.Duration := 0.5;
    FTransitionAni.PropertyName := 'Position.X';
    FTransitionAni.StartFromCurrent := True;
  end;

  procedure ReallignSliderElements;
  begin
    Content.Align := TAlignLayout.alNone;
    Slide1.Align := TAlignLayout.alVertical;
    Slide1.Position.Point := PointF(0, 0);
    Slide1.Width := Slider.Width;
    Slide1.Height := Slider.Height;
    Slide1.Visible := True;
    Slide2.Align := TAlignLayout.alVertical;
    Slide2.Width := Slider.Width;
    Slide2.Height := Slider.Height;
    Slide2.Visible := True;

    Content.Width := Slide1.Width + Slide2.Width;
    Content.Position.Point := PointF(0, 0);
  end;

  procedure LoadFirstImage;
  begin
    FCurrentSlide := Slide1;
    FImages.GoFirst;
    if FImages.HasItems and (FImages.Current is TBitmap) then
      Slide1.Bitmap.Assign(FImages.Current as TBitmap);
  end;

begin
  RemoveOldSliderElements;
  CreateNewSliderElements;
  ReallignSliderElements;
  UpdateNavigationButtonVisibility;
  LoadFirstImage;
end;

procedure TFormMain.ResetProgressEffect;
var
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  TargetProgressProperty: TRttiProperty;
begin
  RttiCtx := TRttiContext.Create;
  try
    RttiType := RttiCtx.GetType(FTransitionEffect.ClassInfo);
    TargetProgressProperty := RttiType.GetProperty('Progress');
    if Assigned(TargetProgressProperty) then
      TargetProgressProperty.SetValue(FTransitionEffect, 0);
  finally
    RttiCtx.Free;
  end;
end;

procedure TFormMain.ShowSplashLoading;
begin
  LightBox.Align := TAlignLayout.alContents;
  LoadingIndicator.Enabled := True;
  LightBox.Visible := True;
end;

procedure TFormMain.TimerSlideShowTimer(Sender: TObject);
begin
  if FImages.IsLast then
  begin
    FImages.GoFirst;
    ActionNext.Execute;
  end
  else
    ActionNext.Execute;
end;

procedure TFormMain.UpdateNavigationButtonVisibility;
begin
  LayoutLeft.Visible := not FImages.IsFirst and FImages.HasItems;
  LayoutRight.Visible := not FImages.IsLast and FImages.HasItems;
end;

{ TSplashImagesLoader }

constructor TSplashImagesLoader.Create;
begin
  inherited Create(True);
end;

procedure TSplashImagesLoader.Execute;
begin
  if not Assigned(Form) or not Assigned(FImages) then
    Exit;

  Synchronize(FForm.ShowSplashLoading);
  try
    FImages.Load;
  finally
    Synchronize(FForm.HideSplashLoading);
  end;
end;

end.
