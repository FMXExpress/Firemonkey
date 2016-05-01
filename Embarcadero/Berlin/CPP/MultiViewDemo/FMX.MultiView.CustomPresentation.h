//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef Fmx_MultiView_CustomPresentationH
#define Fmx_MultiView_CustomPresentationH

//--------------------------------------------------------------------------



//--------------------------------------------------------------------------
/*

  TMultiViewAlertPresentation = class(TMultiViewPresentation)
  private
	FDetailOverlay: TShadowedOverlayLayout;
	FFrame: TPanel;
	{ Messaging }
	procedure DoFormReleased(const Sender: TObject; const M: TMessage);
  protected
	function GetDisplayName: string; override;
	procedure DoOpen(const ASpeed: Single); override;
	procedure DoClose(const ASpeed: Single); override;
	procedure DoInstall; override;
	procedure DoUninstall; override;
	{ Mouse events }
	procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); virtual;
  public
	constructor Create(AMultiView: TCustomMultiView); override;
	destructor Destroy; override;
	procedure UpdateSettings; override;
	procedure Realign; override;
  end;
*/

class TMultiViewAlertPresentation : public TMultiViewPresentation
{

};


#endif
