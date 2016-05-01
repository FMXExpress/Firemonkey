//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef __TODOITEM_TYPES_H
#define __TODOITEM_TYPES_H

#include <System.Classes.hpp>

#pragma explicit_rtti methods (__published, public) properties (__published, public) fields(__published, public)
class TToDo: public TObject {
private:
	System::String FTitle;
	System::String FContent;
public:
	__property System::String Title = {read=FTitle, write=FTitle};
	__property System::String Content = {read=FContent, write=FContent};
};

struct TToDoNames {
public:
	static const String TitleProperty;
	static const String ContentProperty;
	static const String BackendClassname;
	static const String TitleElement;
	static const String ContentElement;
};

#endif
