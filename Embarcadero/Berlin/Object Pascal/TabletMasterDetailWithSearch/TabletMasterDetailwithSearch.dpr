//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program TabletMasterDetailwithSearch;

uses
  System.StartUpCopy,
  FMX.Forms,
  MasterDetailTablet_Search in 'MasterDetailTablet_Search.pas' {TabletSearchForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabletSearchForm, TabletSearchForm);
  Application.Run;
end.
