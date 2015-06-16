unit uFrame2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, DPF.iOS.BaseControl, DPF.iOS.UIDatePicker;

type
  TFrame2 = class( TFrame )
    DPFDatePicker1: TDPFDatePicker;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
