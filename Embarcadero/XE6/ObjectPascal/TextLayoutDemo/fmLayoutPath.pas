
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit fmLayoutPath;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo, FMX.StdCtrls,
  FMX.Objects, FMX.Graphics;

type
  TLayoutPathFrom = class(TForm)
    pthView: TPath;
    mPathData: TMemo;
  public
    procedure SetPath(APath: TPathData);
  end;


implementation

{$R *.fmx}

{ TLayoutPathFrom }

procedure TLayoutPathFrom.SetPath(APath: TPathData);
begin
  pthView.Data.Assign(APath);
  mPathData.Text := APath.Data;
end;

end.
