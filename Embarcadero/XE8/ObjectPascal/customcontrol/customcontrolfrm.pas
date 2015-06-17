
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit customcontrolfrm;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,  
  fmx.customcontrol;

type
  TForm6 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TForm6.FormCreate(Sender: TObject);
var
  C: TMyControl;
begin
  C := TMyControl.Create(Self);
  C.Parent := Self;
end;

end.
