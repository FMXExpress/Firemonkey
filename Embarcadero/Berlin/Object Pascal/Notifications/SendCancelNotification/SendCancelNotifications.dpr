//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program SendCancelNotifications;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {NotificationsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNotificationsForm, NotificationsForm);
  Application.Run;
end.
