unit FullScreenService;

interface

uses FMX.Forms;

type
  /// FullScreenWindowService that is created.
  TFullScreenServiceiOS = class(TInterfacedObject, IFMXFullScreenWindowService)
  private
    FOriginalBoarderStyle : TFmxFormBorderStyle;
  public
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetFullScreen(const AForm: TCommonCustomForm;
      const AValue: Boolean);
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm;
      const AValue: Boolean);
  end;

var
  FullScreenServiceiOS : TFullScreenServiceiOS;

implementation

uses
  FMX.Platform;

{ TFullScreenServiceiOS }

function TFullScreenServiceiOS.GetFullScreen(
  const AForm: TCommonCustomForm): Boolean;
begin
  if AForm = nil then
    Exit(False);

  Result := AForm.BorderStyle = TFmxFormBorderStyle.None;
end;

procedure TFullScreenServiceiOS.SetFullScreen(const AForm: TCommonCustomForm;
  const AValue: Boolean);
begin
  if AForm = nil then
    Exit;

  if AValue then begin
    if AForm.BorderStyle <> TFmxFormBorderStyle.None then
      FOriginalBoarderStyle := AForm.BorderStyle;

    AForm.BorderStyle := TFmxFormBorderStyle.None
  end else begin
    if (FOriginalBoarderStyle = TFmxFormBorderStyle.None) and (AValue = False) then
      FOriginalBoarderStyle := TFmxFormBorderStyle.Sizeable;

    AForm.BorderStyle := FOriginalBoarderStyle;
  end;
end;

procedure TFullScreenServiceiOS.SetShowFullScreenIcon(
  const AForm: TCommonCustomForm; const AValue: Boolean);
begin
end;

procedure RegisterFullScreenServiceiOS;
begin
  FullScreenServiceiOS := TFullScreenServiceiOS.Create;
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, FullScreenServiceiOS);
end;

{$IFDEF IOS}
initialization
  RegisterFullScreenServiceiOS;
{$ENDIF}

end.
