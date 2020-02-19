//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop
#pragma package(smart_init)

#include "NotesStorageU.h"
#include <memory>

// ---------------------------------------------------------------------------
String __fastcall TNotesStorage::DecodeMultilineText(const String & AText) {
	std::auto_ptr<TStringBuilder> lBuilder(new TStringBuilder());
	String _return = "";
	int i = 1;
	while(i <= AText.Length()) {
		WideChar S = AText[i];
		if((S == '\\') && (i+1 <= AText.Length())) {
			switch(AText[i+1]) {
				case 'n' :
					i++;
					S = static_cast<WideChar>(10);
					break;
				case 'r' :
					i++;
					S = static_cast<WideChar>(13);
					break;
				case '\\' :
					i++;
					S = '\\';
					break;
			}
		}
		lBuilder->Append(S);
		i++;
	}
	_return = lBuilder->ToString();
	return _return;
}
// ---------------------------------------------------------------------------
String __fastcall TNotesStorage::EncodeMultilineText(const String & AText) {
	std::auto_ptr<TStringBuilder> lBuilder(new TStringBuilder());
	String _return = "";
	for(int i = 1; i <= AText.Length(); i++)
	{
		WideChar S = AText[i];
		switch(S) {
			case 10 :
				lBuilder->Append("\\n");
				break;
			case 13 :
				lBuilder->Append("\\r");
				break;
			case '\\' :
				lBuilder->Append("\\\\");
				break;
			default:
				lBuilder->Append(S);
				break;
		}
	}
	_return = lBuilder->ToString();
	return _return;
}
// ---------------------------------------------------------------------------
void __fastcall TNotesStorage::ReadNote(const String & AID, TNote *ANote) {
	ANote->Title = FIniFile->ReadString(FUserID, AID + ".title", "");
	ANote->Content = DecodeMultilineText(FIniFile->ReadString(FUserID, AID + ".text", ""));
	ANote->ID = AID;
}
// ---------------------------------------------------------------------------
void __fastcall TNotesStorage::ReadIDs(std::vector<String> * AList) {
	TStrings * lSections = new TStringList();
	FIniFile->ReadSection(FUserID, lSections);
	for(int i = 0; i < lSections->Count; i++)
	{
		String lKey = lSections->operator [](i);
		if(EndsStr(".title", lKey)) {
			String lId = lKey.SubString(0, lKey.Length() - String(".title").Length());
			AList->push_back(lId);
        }
	}
}
// ---------------------------------------------------------------------------
std::vector<String> * __fastcall TNotesStorage::ReadIDs(void) {
	std::vector<String> * _return = new std::vector<String>();
	ReadIDs(_return);
	return _return;
}
// ---------------------------------------------------------------------------
__fastcall TNotesStorage::TNotesStorage(const String & ADirectory,
	const String & AUserID) {
	FUserID = AUserID;
	String lPath = IncludeTrailingPathDelimiter(ExpandFileName(ADirectory)) + "notes.ini";
	FIniFile = new TIniFile(lPath);
}
// ---------------------------------------------------------------------------
__fastcall TNotesStorage::~TNotesStorage(void) {
	FreeAndNil(FIniFile);
}
// ---------------------------------------------------------------------------
std::vector<TNote*> * __fastcall TNotesStorage::GetNotes(void) {
	std::vector<TNote*> * notes = new std::vector<TNote*>();

	std::vector<String> *ids = ReadIDs();
	for(std::vector<String>::iterator it = ids->begin(); it != ids->end(); it++)
	{
		TNote * note = new TNote();
		ReadNote(*it, note);
		notes->push_back(note);
	}
	 return notes;
}
// ---------------------------------------------------------------------------
bool __fastcall TNotesStorage::GetNote(const String & AID, TNote * ANote) {
	bool _return = NoteIDExists(AID);
	if(_return) {
		ReadNote(AID, ANote);
	}
	return _return;
}
// ---------------------------------------------------------------------------
void __fastcall TNotesStorage::UpdateNote(const String & AID,
	const TNote * ANote) {
	if(!NoteIDExists(AID)) {
		throw ENoteNotFound(String().printf(L"%s", AID.c_str()));
	}
	FIniFile->WriteString(FUserID, AID + ".title", EncodeMultilineText(ANote->Title));
	FIniFile->WriteString(FUserID, AID + ".text", EncodeMultilineText(ANote->Content));
}

String __fastcall TNotesStorage::NextId(void)
{
	std::vector<String> * lList = new std::vector<String>();
	ReadIDs(lList);
	int i = lList->size();
	for(std::vector<String>::iterator it = lList->begin(); it != lList->end(); it++)
	{
		if(ContainsStr(*it, IntToStr(i))) {
			break;
		}
		i++;
	}
	delete lList;
	return IntToStr(i);
}

// ---------------------------------------------------------------------------
void __fastcall TNotesStorage::AddNote(const TNote & ANote, String &AID) {
	if(ANote.Title.IsEmpty()) {
		throw new ENoteMissingTitle("Note title required");
	}
	if(NoteTitleExists(ANote.Title)) {
		throw ENoteDuplicate(String().sprintf(L"%s already exists", ANote.Title.c_str()));
    }
	AID = this->NextId();
	FIniFile->WriteString(FUserID, AID + ".title", ANote.Title);
	FIniFile->WriteString(FUserID, AID + ".text", EncodeMultilineText(ANote.Content));
}
// ---------------------------------------------------------------------------
bool __fastcall TNotesStorage::DeleteNote(const String & AID) {
	bool _return = NoteIDExists(AID);
	if(_return) {
        FIniFile->DeleteKey(FUserID, AID + ".title");
	}
	return _return;
}
// ---------------------------------------------------------------------------
bool __fastcall TNotesStorage::NoteTitleExists(const String ATitle) {
	bool _return = false;
	std::vector<TNote*> * notes = this->GetNotes();
	for(std::vector<TNote*>::iterator it = notes->begin(); it != notes->end(); it++)
	{
		if((*it)->Title == ATitle) {
			_return = true;
			break;
		}
	}
	return _return;
}
// ---------------------------------------------------------------------------
bool __fastcall TNotesStorage::FindNote(const String &ATitle, TNote * ANote) {
	bool _return = false;
	std::vector<TNote*> * notes = this->GetNotes();
	for(std::vector<TNote*>::iterator it = notes->begin(); it != notes->end(); it++)
	{
		if((*it)->Title == ATitle) {
			ReadNote((*it)->ID, ANote);
			_return = true;
			break;
		}
	}
	return _return;
}
// ---------------------------------------------------------------------------
bool __fastcall TNotesStorage::NoteIDExists(const String &AID) {
	return FIniFile->ValueExists(FUserID, AID + ".title");
}
// ---------------------------------------------------------------------------
