//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef NotesStorageUH
#define NotesStorageUH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.IniFiles.hpp>
#include "NoteTypesU.h"
#include <vector>

class TNotesStorage {
private:
	TIniFile * FIniFile;
	String FUserID;

	String __fastcall DecodeMultilineText(const String & AText);
	String __fastcall EncodeMultilineText(const String & AText);
	void __fastcall ReadNote(const String & AID, TNote *ANote);
	void __fastcall ReadIDs(std::vector<String> * AList);
	std::vector<String> * __fastcall ReadIDs(void);
	String __fastcall NextId(void);

public:
	__fastcall TNotesStorage(const String & ADirectory, const String & AUserID);
	__fastcall ~TNotesStorage(void);
	std::vector<TNote*> * __fastcall GetNotes(void);
	bool __fastcall GetNote(const String & AID, TNote * ANote);
	void __fastcall UpdateNote(const String & AID, const TNote * ANote);
	void __fastcall AddNote(const TNote &ANote,  String &AID);
	bool __fastcall DeleteNote(const String & AID);
	bool __fastcall NoteTitleExists(const String ATitle);
	bool __fastcall FindNote(const String &ATitle, TNote * ANote);
	bool __fastcall NoteIDExists(const String &AID);
};

class ENoteError : public Exception
{
public:
	__fastcall ENoteError(const System::UnicodeString Msg) : Exception(Msg){}
};

class ENoteNotFound : public ENoteError
{
public:
	__fastcall ENoteNotFound(const System::UnicodeString Msg) :
		ENoteError(Msg){}
};

class ENoteDuplicate : public ENoteError
{
public:
	__fastcall ENoteDuplicate(const System::UnicodeString Msg) :
		ENoteError(Msg){}
};

class ENoteMissingTitle : public ENoteError
{
public:
	__fastcall ENoteMissingTitle(const System::UnicodeString Msg) :
		ENoteError(Msg){}
};

#endif
