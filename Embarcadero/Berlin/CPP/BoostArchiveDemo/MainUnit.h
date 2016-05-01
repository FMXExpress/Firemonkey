//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainUnitH
#define MainUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Types.hpp>
#include <FMX.Edit.hpp>

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TButton *ArchiveButton;
	TButton *RestoreButton;
	TEdit *OrigDegreesEdit;
	TLabel *Label1;
	TEdit *OrigMinutesEdit;
	TLabel *Label2;
	TEdit *OrigSecondsEdit;
	TLabel *Label3;
	TEdit *RestDegreesEdit;
	TEdit *RestMinutesEdit;
	TEdit *RestSecondsEdit;
	TLabel *Label4;
	TLabel *Label5;
	void __fastcall ArchiveButtonClick(TObject *Sender);
	void __fastcall RestoreButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};

class gps_position
{
private:
	friend class boost::serialization::access;
	// When the class Archive corresponds to an output archive, the
	// & operator is defined similar to <<.  Likewise, when the class Archive
	// is a type of input archive the & operator is defined similar to >>.
	template<class Archive>
	void serialize(Archive & ar, const unsigned int version)
	{
		ar & degrees;
		ar & minutes;
		ar & seconds;
	}
	int degrees;
	int minutes;
	float seconds;
public:
	gps_position(){};
	gps_position(int d, int m, float s) :
		degrees(d), minutes(m), seconds(s)
	{}
};


//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
