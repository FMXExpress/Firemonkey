unit MainFrm_Tablet;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, MainFrm, FMX.ActnList, System.Actions, FMX.StdActns, FMX.Objects, FMX.Layouts, FMX.MediaLibrary.Actions,
  FMX.Effects, FMX.Filter.Effects, FMX.Ani;

type
  TTabletMainForm = class(TBaseMainForm)
    Image1: TImage;
    TopHelp: TLayout;
    Text1: TText;
    Image2: TImage;
    Text2: TText;
    ActionSwirlEffect: TAction;
    SpeedButton11: TSpeedButton;
    ActionWaveEffect: TAction;
    ActionEmbossEffect: TAction;
    SpeedButton7: TSpeedButton;
    ActionContrastEffect: TAction;
    SpeedButton8: TSpeedButton;
    ActionPaperSketchEffect: TAction;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ActionSwirlEffectExecute(Sender: TObject);
    procedure ActionWaveEffectExecute(Sender: TObject);
    procedure ActionEmbossEffectExecute(Sender: TObject);
    procedure ActionContrastEffectExecute(Sender: TObject);
    procedure ActionPaperSketchEffectExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TabletMainForm: TTabletMainForm;

implementation

{$R *.fmx}

procedure TTabletMainForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  inherited ActionListUpdate(Action, Handled);
  TopHelp.Visible := FRawBitmap.IsEmpty;
  ActionSwirlEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionWaveEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionEmbossEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionContrastEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionPaperSketchEffect.Enabled := not FRawBitmap.IsEmpty;
end;

procedure TTabletMainForm.ActionContrastEffectExecute(Sender: TObject);
begin
  SetEffect('Contrast');
end;

procedure TTabletMainForm.ActionEmbossEffectExecute(Sender: TObject);
begin
  SetEffect('Emboss');
end;

procedure TTabletMainForm.ActionPaperSketchEffectExecute(Sender: TObject);
begin
  SetEffect('PaperSketch');
end;

procedure TTabletMainForm.ActionSwirlEffectExecute(Sender: TObject);
begin
  SetEffect('Swirl');
end;

procedure TTabletMainForm.ActionWaveEffectExecute(Sender: TObject);
begin
  SetEffect('Wave');
end;

end.
