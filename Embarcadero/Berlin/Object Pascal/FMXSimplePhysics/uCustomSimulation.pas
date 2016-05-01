unit uCustomSimulation;

interface

uses
  System.Diagnostics;

type
  TCustomSimulation = class
  protected
    FStopwatch: TStopwatch;
    FStartTime,
    FLastTime: Double;
    procedure Initialize; virtual; abstract;
    procedure Step(deltaTime: Double = 1/60); virtual; abstract;
    procedure Finalize; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoStep;
    function GetRawReferenceTime: Double;
  end;

implementation

{ TCustomSimulation }

constructor TCustomSimulation.Create;
begin
  FStopwatch := TStopwatch.Create;
  FStartTime := GetRawReferenceTime;
  FLastTime := GetRawReferenceTime;
  Initialize;
end;

destructor TCustomSimulation.Destroy;
begin
  Finalize;
  inherited;
end;

procedure TCustomSimulation.DoStep;
var deltaTime, refTime: Double;
begin
  refTime := GetRawReferenceTime;
  deltaTime := refTime - FLastTime;
  FLastTime := refTime;

  Step(deltaTime);
end;

function TCustomSimulation.GetRawReferenceTime: Double;
begin
  Result := FStopwatch.GetTimeStamp / FStopwatch.Frequency;
end;

end.
