unit uXCadencer;

interface

uses
  FMX.Types,
  System.Diagnostics;

type
  TProgressEvent = procedure(const deltaTime, newTime: Double) of object;

  TXCadencer = class
  private
    FStopwatch: TStopwatch;
    FTimer: TTimer;
    FEnabled: boolean;
    FOnProgress: TProgressEvent;
    FStartTime,
    FLastTime: double;
    procedure SetEnabled(const Value: boolean);
    function GetEnabled: boolean;
    procedure DoOnTimer(Sender: TObject);
  protected
    function GetRawReferenceTime: double;
    procedure DoOnProgress; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TXCadencer }

constructor TXCadencer.Create;
begin
  FStopwatch := TStopwatch.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 1;
  FTimer.OnTimer := DoOnTimer;

  Reset;
end;

destructor TXCadencer.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TXCadencer.DoOnProgress;
var deltaTime, newTime, refTime: Double;
begin
  refTime := GetRawReferenceTime;
  newTime := refTime - FStartTime;
  deltaTime := refTime - FLastTime;
  FLastTime := refTime;

  if Assigned(FOnProgress) then
    FOnProgress(deltaTime, newTime);
end;

procedure TXCadencer.DoOnTimer(Sender: TObject);
begin
  DoOnProgress;
end;

function TXCadencer.GetEnabled: boolean;
begin
  Result := FTimer.Enabled;
end;

function TXCadencer.GetRawReferenceTime: double;
begin
  Result := TStopwatch.GetTimeStamp / TStopwatch.Frequency;
end;

procedure TXCadencer.Reset;
begin
  FStartTime := GetRawReferenceTime;
  FLastTime := GetRawReferenceTime;
end;

procedure TXCadencer.SetEnabled(const Value: boolean);
begin
  if FTimer.Enabled <> Value then
  begin
    FTimer.Enabled := Value;
    if Value then
      Reset;
  end;
end;

end.
