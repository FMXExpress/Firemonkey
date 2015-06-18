unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  EDK.Core, EDK.EmoState, EDK.ErrorCodes, Vcl.ExtCtrls, Vcl.Samples.Spin;

type
  TForm2 = class(TForm)
    gbEDK: TGroupBox;
    gbDeviceCount: TGroupBox;
    btnDeviceCount: TButton;
    edDeviceCount: TEdit;
    Timer1: TTimer;
    sbMouse: TGroupBox;
    seSensitivity: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbAutoCenter: TCheckBox;
    cbActive: TCheckBox;
    seMinX: TSpinEdit;
    Label3: TLabel;
    seMinY: TSpinEdit;
    Label4: TLabel;
    Panel1: TPanel;
    lblUpperFace: TLabel;
    lblEye: TLabel;
    lblLowerFace: TLabel;
    lblContacts: TLabel;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnDeviceCountClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure seSensitivityChange(Sender: TObject);
  private
    FEmoEvent: EmoEngineEventHandle;
    FEmoState: EmoStateHandle;
    FErrCode: Integer;
    FNumUsers: Integer;
    FInitialized: Boolean;
    FUserID: Word;
    procedure Gyro;
    procedure Expressiv;
    procedure ContactQuality;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}



procedure PosOrNeg(var AValue: Integer; const AModifier: Integer);
begin
  if AModifier <> 0 then
  begin
    if AValue < 0 then
      AValue := AValue div AModifier;
    if AValue > 0 then
      AValue := AValue div AModifier;
  end;
end;

function ExpressiveString(expressive: TExpressivAlgo): string;
begin
  case expressive of
    EXP_NEUTRAL: Result := 'Neutral';
    EXP_BLINK: Result := 'Blink';
    EXP_WINK_LEFT: Result := 'Wink Left';
    EXP_WINK_RIGHT: Result := 'Wink Right';
    EXP_HORIEYE: Result := 'Horizontal Eyes';
    EXP_EYEBROW: Result := 'Eye Brows';
    EXP_FURROW: Result := 'Furrow';
    EXP_SMILE: Result := 'Smile';
    EXP_CLENCH: Result := 'Clench';
    EXP_LAUGH: Result := 'Laugh';
    EXP_SMIRK_LEFT: Result := 'Left Smirk';
    EXP_SMIRK_RIGHT: Result := 'Right Smirk';
  else
    Result := IntToStr(Integer(expressive));
  end;
end;


procedure TForm2.ContactQuality;
var
  contacts: array[0..15] of TEEGContactQuality;
  s: string;
  I: Integer;
begin
  s := '';
  for I := 0 to 15 do
  begin
    contacts[i] := ES_GetContactQuality(FEmoState, I);
    case contacts[i] of
      EEG_CQ_NO_SIGNAL: s := s + '0';
      EEG_CQ_VERY_BAD: s := s + '1';
      EEG_CQ_POOR: s := s + '2';
      EEG_CQ_FAIR: s := s + '3';
      EEG_CQ_GOOD: s := s + '4';
    end;
  end;
  lblContacts.Caption := s;
end;

procedure TForm2.Expressiv;
var
  upperFaceType: TExpressivAlgo;
  upperFacePower: Single;
  s: string;
begin
  upperFaceType := ES_ExpressivGetUpperFaceAction(FEmoState);
  upperFacePower := ES_ExpressivGetUpperFaceActionPower(FEmoState);
  lblUpperFace.Caption := ExpressiveString(upperFaceType);

  if ES_ExpressivIsBlink(FEmoState) = 1 then lblEye.Caption := 'Blink' else
  if ES_ExpressivIsLeftWink(FEmoState) = 1 then lblEye.Caption := 'Left Wink' else
  if ES_ExpressivIsRightWink(FEmoState) = 1 then lblEye.Caption := 'Right Wink' else
  if ES_ExpressivIsEyesOpen(FEmoState) = 1 then lblEye.Caption := 'Open' else
  if ES_ExpressivIsLookingUp(FEmoState) = 1 then lblEye.Caption := 'Look Up' else
  if ES_ExpressivIsLookingLeft(FEmoState) = 1 then lblEye.Caption := 'Look Left' else
  if ES_ExpressivIsLookingRight(FEmoState) = 1 then lblEye.Caption := 'Look Right' else
  if ES_ExpressivIsLookingDown(FEmoState) = 1 then lblEye.Caption := 'Look Down';
  //lblEye.Caption := 'Undefined';

  lblLowerFace.Caption := ExpressiveString(ES_ExpressivGetLowerFaceAction(FEmoState));
end;

procedure TForm2.Gyro;
var
  LY: Integer;
  LMousePos: TPoint;
  LX: Integer;
  status: integer;
begin
  status := EE_HeadsetGetGyroDelta(0, @LX, @LY);
  if status <> EDK_OK then
    lblStatus.Caption := EDK_ErrorToString(status)
  else begin
    lblStatus.Caption := Format('X: %d, Y: %d', [LX, LY]);
    LMousePos := Mouse.CursorPos;
    PosOrNeg(LX, seSensitivity.Value);
    PosOrNeg(LY, seSensitivity.Value);
    if (LX < -seMinX.Value) or (LX > seMinX.Value) then
      LMousePos.X := LMousePos.X + LX
    else if cbAutoCenter.Checked then
      LMousePos.X := Screen.Width div 2;
    if (LY < -seMinY.Value) or (LY > seMinY.Value) then
      LMousePos.Y := LMousePos.Y - LY
    else if cbAutoCenter.Checked then
      LMousePos.Y := Screen.Height div 2;
    if (cbActive.Checked) then
      Mouse.CursorPos := LMousePos;
  end;
end;

procedure TForm2.btnDeviceCountClick(Sender: TObject);
begin
  RaiseEdkError(EE_EngineGetNumUser(@FNumUsers));

  edDeviceCount.Text := IntToStr(FNumUsers);
  if FNumUsers > 0 then
    Timer1.Enabled := FInitialized;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FInitialized := EE_EngineConnect(PAnsiChar(AnsiString('Emotiv Systems-5'))) = EDK_OK;
  if FInitialized then
  begin
    RaiseEdkError(EE_EnableDiagnostics(PAnsiChar(AnsiString(ChangeFileExt(Application.ExeName, '.log'))), 1, 0));
    //FEmoEvent := EE_EmoEngineEventCreate;
    FEmoState := EE_EmoStateCreate;
    //RaiseEdkError(EE_EmoEngineEventGetEmoState(FEmoEvent, FEmoState));
    //RaiseEdkError(EE_EmoEngineEventGetUserId(FEmoEvent, @FUserId));
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  if FInitialized then
  begin
    EE_EmoStateFree(FEmoState);
    EE_EmoEngineEventFree(FEmoEvent);
    EE_EngineDisconnect;
  end;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  gbEDK.Enabled := FInitialized;
  if FInitialized then
    btnDeviceCount.Click;
end;

procedure TForm2.seSensitivityChange(Sender: TObject);
begin
  if seSensitivity.Value < 0 then
    seSensitivity.Value := 0;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if FNumUsers > 0 then
  begin
    Gyro;
    Expressiv;
    ContactQuality;
  end;
end;

end.
