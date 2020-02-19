unit uFmxControlHelper;

interface

uses
  FMX.Controls;

type
  TFmxControlHelper = class helper for TControl
  public
    procedure SetRotation(const Value: Single);
  end;

implementation

{ TFmxControlHelper }

procedure TFmxControlHelper.SetRotation(const Value: Single);
begin
  RotationAngle := Value;
end;

end.
