//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop

#include "NotesResourceU.h"
#include <memory>
#include <vector>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "System.Classes.TPersistent"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TNotesResource1::TNotesResource1(TComponent* Owner)
	: TDataModule(Owner)
{
}

__fastcall TNotesResource1::~TNotesResource1(void)
{
    FreeAndNil(&FNotesStorage);
}

void TNotesResource1::HandleException(void)
{
	String LMessage = "";
	TObject * lException = ExceptObject();
	if(lException != NULL) {
		if(dynamic_cast<Exception*>(lException)) {
			LMessage = dynamic_cast<Exception*>(lException)->Message;
			if(dynamic_cast<ENoteDuplicate*>(lException)) {
				EEMSHTTPError::RaiseDuplicate(LMessage);
			}
			else if(dynamic_cast<ENoteNotFound*>(lException)) {
				EEMSHTTPError::RaiseNotFound(LMessage);
			}
			else if(dynamic_cast<ENoteMissingTitle*>(lException)) {
                EEMSHTTPError::RaiseBadRequest(LMessage);
			}
			else {
				lException = (TObject*)AcquireExceptionObject();
				if(lException != NULL) {
					throw lException;
				}
			}
        }
    }
}

void TNotesResource1::Get(TEndpointContext* AContext, TEndpointRequest* ARequest,
	TEndpointResponse* AResponse)
{
	String LTitle = "";
	std::auto_ptr<TNote> LNote(new TNote());
	std::vector<TNote*> * lNotes = NULL;
	TJSONArray * lJson = NULL;
	try {
		this->CheckNotesManager(AContext);
		if(ARequest->Params->TryGetValue("title", LTitle)) {
			// Find a note with a particular title
			if(FNotesStorage->FindNote(LTitle, LNote.get())) {
				lNotes = new std::vector<TNote*>();
				lNotes->push_back(LNote.get());
			}
			else {
				lNotes = NULL;
			}
		}
		else {
			lNotes = FNotesStorage->GetNotes();
		}
		lJson = TNoteJSON::NotesToJSON(lNotes);
		AResponse->Body->SetValue(lJson, true);
	}
	catch(...) {
		FreeAndNil(&lJson);
		HandleException();
	}
}

void TNotesResource1::GetItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try {
		String lItem = ARequest->Params->Values["item"];
		std::auto_ptr<TNote> lNote(new TNote());
		CheckNotesManager(AContext);
		if(FNotesStorage->GetNote(lItem, lNote.get())) {
			TJSONObject * jsonObj = TNoteJSON::NoteToJSON(lNote.get());
			AResponse->Body->SetValue(jsonObj, true);
		} else {
			AResponse->RaiseNotFound(String().sprintf(L"\"%s\" not found", lItem.c_str()));
		}
	} catch (...) {
		HandleException();
	}
}

void TNotesResource1::Post(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try {
		TJSONObject * lJson = NULL;
		if(ARequest->Body->TryGetObject(lJson)) {
			CheckNotesManager(AContext);
			TNote * lNote = TNoteJSON::JSONToNote(lJson);
			String lId = "";
			FNotesStorage->AddNote(*lNote, lId);
			lJson = new TJSONObject();
			lJson->AddPair(TNoteJSON::TNames::Id, lId);
			AResponse->Body->SetValue(lJson, true);
		}
		else {
			AResponse->RaiseBadRequest("JSON expected");
		}
	} catch (...) {
		HandleException();
	}
}

void TNotesResource1::PutItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try {
		String lItem = ARequest->Params->Values["item"];
		TJSONObject * lJson = NULL;
		if(ARequest->Body->TryGetObject(lJson)) {
			CheckNotesManager(AContext);
			TNote * lNote = TNoteJSON::JSONToNote(lJson);
			FNotesStorage->UpdateNote(lItem, lNote);
		}
		else {
		   AResponse->RaiseBadRequest("JSON expected");
        }
	} catch (...) {
		HandleException();
	}
}

String GetModuleDirectory(void)
{
	return ExtractFilePath(StringReplace(GetModuleName((unsigned int)HInstance), "\\\\?\\",
		"", TReplaceFlags() << System::Sysutils::rfReplaceAll));
}

void TNotesResource1::CheckNotesManager(const TEndpointContext * AContext)
{
	if(const_cast<TEndpointContext*>(AContext)->User == NULL) {
		const_cast<TEndpointContext*>(AContext)->Response->RaiseUnauthorized("The operation is only permitted for logged in users");
	}
	if(FNotesStorage == NULL) {
		FNotesStorage = new TNotesStorage(GetModuleDirectory(), const_cast<TEndpointContext*>(AContext)->User->UserID);
    }
}

void TNotesResource1::DeleteItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try{
		String lItem = ARequest->Params->Values["item"];
		CheckNotesManager(AContext);
		FNotesStorage->DeleteNote(lItem);
	}catch(...) {
		HandleException();
    }
}

System::String initYamlDefinitions()
{
	return "# "  sLineBreak
  " ItemObject:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      RowID:"   sLineBreak
  "        type: integer"   sLineBreak
  "      Original:"   sLineBreak
  "        type: object"   sLineBreak
  "        properties:"   sLineBreak
  "          EMP_NO:"   sLineBreak
  "            type: integer"   sLineBreak
  "          FIRST_NAME:"   sLineBreak
  "            type: string"   sLineBreak
  "          LAST_NAME:"   sLineBreak
  "            type: string"   sLineBreak
  "          PHONE_EXT:"   sLineBreak
  "            type: string"   sLineBreak
  "          HIRE_DATE:"   sLineBreak
  "            type: string"   sLineBreak
  "          DEPT_NO:"   sLineBreak
  "            type: string"   sLineBreak
  "          JOB_CODE:"   sLineBreak
  "            type: string"   sLineBreak
  "          JOB_GRADE:"   sLineBreak
  "            type: integer"   sLineBreak
  "          JOB_COUNTRY:"   sLineBreak
  "            type: string"   sLineBreak
  "          SALARY:"   sLineBreak
  "            type: integer"   sLineBreak
  "          FULL_NAME:"   sLineBreak
  "            type: string"   sLineBreak
  "# "   sLineBreak
  " TableObject:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      class:"   sLineBreak
  "        type: string"   sLineBreak
  "      Name:"   sLineBreak
  "        type: string"   sLineBreak
  "      SourceName:"   sLineBreak
  "        type: string"   sLineBreak
  "      SourceID:"   sLineBreak
  "        type: integer"   sLineBreak
  "      RowList:"   sLineBreak
  "        type: array"   sLineBreak
  "        items: "   sLineBreak
  "          $ref: \"#/definitions/ItemObject\""   sLineBreak
  "# "   sLineBreak
  " EmployeeTable:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      FBDS:"   sLineBreak
  "        type: object"   sLineBreak
  "        properties:"   sLineBreak
  "          Version:"   sLineBreak
  "            type: string"   sLineBreak
  "          Manager:"   sLineBreak
  "            type: object"   sLineBreak
  "            properties:"   sLineBreak
  "              TableList:"   sLineBreak
  "                type: array"   sLineBreak
  "                items:"   sLineBreak
  "                  $ref: \"#/definitions/TableObject\""   sLineBreak
  "# "   sLineBreak
  " PostObject:"   sLineBreak
  "    properties:"   sLineBreak
  "      EMP_NO:"   sLineBreak
  "        type: integer"   sLineBreak
  "      FIRST_NAME:"   sLineBreak
  "        type: string"   sLineBreak
  "      LAST_NAME:"   sLineBreak
  "        type: string"   sLineBreak
  "      PHONE_EXT:"   sLineBreak
  "        type: string"   sLineBreak
  "      HIRE_DATE:"   sLineBreak
  "        type: string"   sLineBreak
  "        format: date-time"   sLineBreak
  "      DEPT_NO:"   sLineBreak
  "        type: string"   sLineBreak
  "      JOB_CODE:"   sLineBreak
  "        type: string"   sLineBreak
  "      JOB_GRADE:"   sLineBreak
  "        type: integer"   sLineBreak
  "      JOB_COUNTRY:"   sLineBreak
  "        type: string"   sLineBreak
  "      SALARY:"   sLineBreak
  "        type: integer"   sLineBreak
  "      FULL_NAME:"   sLineBreak
  "        type: string"   sLineBreak
  "# "   sLineBreak
  " PutObject:"   sLineBreak
  "    type: object" sLineBreak
  "    properties:"   sLineBreak
  "      EMP_NO:"   sLineBreak
  "        type: integer"   sLineBreak
  "      FIRST_NAME:"   sLineBreak
  "        type: string"   sLineBreak
  "      LAST_NAME:"   sLineBreak
  "        type: string"   sLineBreak
  "      PHONE_EXT:"   sLineBreak
  "        type: string"   sLineBreak
  "      HIRE_DATE:"   sLineBreak
  "        type: string"   sLineBreak
  "        format: date-time"   sLineBreak
  "      DEPT_NO:"   sLineBreak
  "        type: string"   sLineBreak
  "      JOB_CODE:"   sLineBreak
  "        type: string"   sLineBreak
  "      JOB_GRADE:"   sLineBreak
  "        type: integer"   sLineBreak
  "      JOB_COUNTRY:"   sLineBreak
  "        type: string"   sLineBreak
  "      SALARY:"   sLineBreak
  "        type: integer"   sLineBreak
  "      FULL_NAME:"   sLineBreak
  "        type: string"   sLineBreak
  "# "   sLineBreak
  " ItemPostedResponseObject:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      PostedData:"   sLineBreak
  "        type: array"   sLineBreak
  "        items:"   sLineBreak
  "          type: string"   sLineBreak
  "# "   sLineBreak
  " ItemPutResponseObject:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      PathItem:"   sLineBreak
  "        type: string"   sLineBreak
  "      PostedData:"   sLineBreak
  "        type: array"   sLineBreak
  "        items:"   sLineBreak
  "          type: string"   sLineBreak
  "# "   sLineBreak
  " NoteObject:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      title:"   sLineBreak
  "        type: string"   sLineBreak
  "      content:"   sLineBreak
  "        type: string"   sLineBreak
  "      id:"   sLineBreak
  "        type: string"   sLineBreak
  "# "   sLineBreak
  " NotesListObject:"   sLineBreak
  "    type: array"   sLineBreak
  "    items:"    sLineBreak
  "      $ref: \"#/definitions/NoteObject\""   sLineBreak
  "# "   sLineBreak
  " IdObject:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      id:"   sLineBreak
  "        type: string"   sLineBreak
  "# "   sLineBreak
  " ErrorObject:"   sLineBreak
  "    type: object"   sLineBreak
  "    properties:"   sLineBreak
  "      error:"   sLineBreak
  "        type: string"   sLineBreak
  "      description:"   sLineBreak
  "        type: string"   sLineBreak
  "";
}

System::String initJSONDefinitions()
{
	return  "{"   sLineBreak
  "    \"ItemObject\": {"   sLineBreak
  "        \"type\": \"object\","   sLineBreak
  "        \"properties\": {"   sLineBreak
  "            \"RowID\": {"   sLineBreak
  "                \"type\": \"integer\""   sLineBreak
  "            },"   sLineBreak
  "            \"Original\": {"   sLineBreak
  "                \"type\": \"object\","   sLineBreak
  "                \"properties\": {"   sLineBreak
  "                    \"EMP_NO\": {"   sLineBreak
  "                        \"type\": \"integer\""   sLineBreak
  "                    },"   sLineBreak
  "                    \"FIRST_NAME\": {"   sLineBreak
  "                        \"type\": \"string\""   sLineBreak
  "                    },"   sLineBreak
  "                    \"LAST_NAME\": {"   sLineBreak
  "                       \"type\": \"string\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"PHONE_EXT\": {"   sLineBreak
  "                       \"type\": \"string\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"HIRE_DATE\": {"   sLineBreak
  "                       \"type\": \"string\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"DEPT_NO\": {"   sLineBreak
  "                       \"type\": \"string\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"JOB_CODE\": {"   sLineBreak
  "                       \"type\": \"string\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"JOB_GRADE\": {"   sLineBreak
  "                       \"type\": \"integer\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"JOB_COUNTRY\": {"   sLineBreak
  "                       \"type\": \"string\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"SALARY\": {"   sLineBreak
  "                       \"type\": \"integer\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"FULL_NAME\": {"   sLineBreak
  "                      \"type\": \"string\""   sLineBreak
  "                   }"   sLineBreak
  "               }"   sLineBreak
  "          }"   sLineBreak
  "       }"   sLineBreak
  "   },"   sLineBreak
  "   \"TableObject\": {"   sLineBreak
  "       \"type\": \"object\","   sLineBreak
  "       \"properties\": {"   sLineBreak
  "           \"class\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"Name\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"SourceName\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"SourceID\": {"   sLineBreak
  "               \"type\": \"integer\""   sLineBreak
  "           },"   sLineBreak
  "           \"RowList\": {"   sLineBreak
  "               \"type\": \"array\","   sLineBreak
  "               \"items\": {"   sLineBreak
  "                   \"$ref\": \"#/definitions/ItemObject\""   sLineBreak
  "               }"   sLineBreak
  "           }"   sLineBreak
  "      }"   sLineBreak
  "   },"   sLineBreak
  "   \"EmployeeTable\": {"   sLineBreak
  "       \"type\": \"object\","   sLineBreak
  "       \"properties\": {"   sLineBreak
  "          \"FBDS\": {"   sLineBreak
  "               \"type\": \"object\","   sLineBreak
  "               \"properties\": {"   sLineBreak
  "                   \"Version\": {"   sLineBreak
  "                       \"type\": \"string\""   sLineBreak
  "                   },"   sLineBreak
  "                   \"Manager\": {"   sLineBreak
  "                       \"type\": \"object\","   sLineBreak
  "                       \"properties\": {"   sLineBreak
  "                           \"TableList\": {"   sLineBreak
  "                               \"type\": \"array\","   sLineBreak
  "                               \"items\": {"   sLineBreak
  "                                   \"$ref\": \"#/definitions/TableObject\""   sLineBreak
  "                               }"   sLineBreak
  "                           }"   sLineBreak
  "                       }"   sLineBreak
  "                   }"   sLineBreak
  "               }"   sLineBreak
  "           }"   sLineBreak
  "       }"   sLineBreak
  "   },"   sLineBreak
  "   \"PostObject\": {"   sLineBreak
  "       \"properties\": {"   sLineBreak
  "           \"EMP_NO\": {"   sLineBreak
  "               \"type\": \"integer\""   sLineBreak
  "           },"   sLineBreak
  "           \"FIRST_NAME\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"LAST_NAME\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"PHONE_EXT\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"HIRE_DATE\": {"   sLineBreak
  "               \"type\": \"string\","   sLineBreak
  "               \"format\": \"date-time\""   sLineBreak
  "           },"   sLineBreak
  "           \"DEPT_NO\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"JOB_CODE\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"JOB_GRADE\": {"   sLineBreak
  "               \"type\": \"integer\""   sLineBreak
  "           },"   sLineBreak
  "            \"JOB_COUNTRY\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            },"   sLineBreak
  "            \"SALARY\": {"   sLineBreak
  "                \"type\": \"integer\""   sLineBreak
  "            },"   sLineBreak
  "            \"FULL_NAME\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            }"   sLineBreak
  "        }"   sLineBreak
  "    },"   sLineBreak
  "   \"PutObject\": {"   sLineBreak
  "       \"properties\": {"   sLineBreak
  "           \"EMP_NO\": {"   sLineBreak
  "               \"type\": \"integer\""   sLineBreak
  "           },"   sLineBreak
  "           \"FIRST_NAME\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"LAST_NAME\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"PHONE_EXT\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"HIRE_DATE\": {"   sLineBreak
  "               \"type\": \"string\","   sLineBreak
  "               \"format\": \"date-time\""   sLineBreak
  "           },"   sLineBreak
  "           \"DEPT_NO\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"JOB_CODE\": {"   sLineBreak
  "               \"type\": \"string\""   sLineBreak
  "           },"   sLineBreak
  "           \"JOB_GRADE\": {"   sLineBreak
  "               \"type\": \"integer\""   sLineBreak
  "           },"   sLineBreak
  "            \"JOB_COUNTRY\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            },"   sLineBreak
  "            \"SALARY\": {"   sLineBreak
  "                \"type\": \"integer\""   sLineBreak
  "            },"   sLineBreak
  "            \"FULL_NAME\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            }"   sLineBreak
  "        }"   sLineBreak
  "    },"   sLineBreak
  "    \"ItemPostedResponseObject\": {"   sLineBreak
  "        \"type\": \"object\","   sLineBreak
  "        \"properties\": {"   sLineBreak
  "            \"PostedData\": {"   sLineBreak
  "                \"type\": \"array\","   sLineBreak
  "                \"items\": {"   sLineBreak
  "                    \"type\": \"string\""   sLineBreak
  "                }"   sLineBreak
  "            }"   sLineBreak
  "        }"   sLineBreak
  "    },"   sLineBreak
  "    \"ItemPutResponseObject\": {"   sLineBreak
  "        \"type\": \"object\","   sLineBreak
  "        \"properties\": {"   sLineBreak
  "            \"PathItem\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            },"   sLineBreak
  "            \"PostedData\": {"   sLineBreak
  "                \"type\": \"array\","   sLineBreak
  "                \"items\": {"   sLineBreak
  "                    \"type\": \"string\""   sLineBreak
  "                }"   sLineBreak
  "            }"   sLineBreak
  "        }"   sLineBreak
  "    },"   sLineBreak
  "    \"NoteObject\": {"  sLineBreak
  "        \"type\": \"object\","   sLineBreak
  "        \"properties\": {"   sLineBreak
  "            \"title\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            },"   sLineBreak
  "            \"content\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            },"   sLineBreak
  "            \"id\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            }"   sLineBreak
  "        }"   sLineBreak
  "    },"   sLineBreak
  "    \"NotesListObject\": {"   sLineBreak
  "        \"type\": \"array\","   sLineBreak
  "        \"items\": {"   sLineBreak
  "            \"$ref\": \"#/definitions/NoteObject\""   sLineBreak
  "        }"   sLineBreak
  "    },"   sLineBreak
  "    \"IdObject\": {"   sLineBreak
  "        \"type\": \"object\","   sLineBreak
  "        \"properties\": {"   sLineBreak
  "            \"id\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            }"  sLineBreak
  "        }"   sLineBreak
  "    },"  sLineBreak
  "    \"ErrorObject\": {"   sLineBreak
  "        \"type\": \"object\","   sLineBreak
  "        \"properties\": {"   sLineBreak
  "            \"error\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            },"   sLineBreak
  "            \"description\": {"   sLineBreak
  "                \"type\": \"string\""   sLineBreak
  "            }"   sLineBreak
  "        }"   sLineBreak
  "    }"   sLineBreak
  "}"
  "";
}

namespace Notesresourceu
{
	void __fastcall PACKAGE Register() {
		std::auto_ptr<TEMSResourceAttributes>attributes
			(new TEMSResourceAttributes());
		attributes->ResourceName = "Notes";

		//YAML definitions
		attributes->YAMLDefinitions["Notes"] = initYamlDefinitions();

		//JSON definitions
		attributes->JSONDefinitions["Notes"] = initJSONDefinitions();

		// SET SUMMARY
		std::unique_ptr<EndPointRequestSummaryAttribute>RequestSummary
			(new EndPointRequestSummaryAttribute("Notes", "Get",
			"Gets a list of notes", "application/json", ""));
		attributes->RequestSummary["Get"] = RequestSummary.get();

		attributes->ResourceSuffix["GetItem"] = "{item}";
		RequestSummary.reset(new EndPointRequestSummaryAttribute
			("Notes", "GetItem", "Accepts a parameter to return a note.",
			"application/json", ""));
		attributes->RequestSummary["GetItem"] = RequestSummary.get();

		RequestSummary.reset(new EndPointRequestSummaryAttribute
			("Notes", "Post", "Adds a note.", "application/json",
			"application/json"));
		attributes->RequestSummary["Post"] = RequestSummary.get();

		attributes->ResourceSuffix["PutItem"] = "{item}";
		RequestSummary.reset(new EndPointRequestSummaryAttribute
			("Notes", "PutItem", "Updates a note.", "application/json",
			"application/json"));
		attributes->RequestSummary["PutItem"] = RequestSummary.get();

		attributes->ResourceSuffix["DeleteItem"] = "{item}";
		RequestSummary.reset(new EndPointRequestSummaryAttribute
			("Notes", "DeleteItem", "Deletes a note.",
			"application/json", "application/json"));
		attributes->RequestSummary["DeleteItem"] = RequestSummary.get();
		// END SET SUMMARY

		// ADD PARAMETERS
		//Get

		//GET /{item}
		std::unique_ptr<EndPointRequestParameterAttribute>ResponseParameter
			(new EndPointRequestParameterAttribute
			(TAPIDocParameter::TParameterIn::Path, "item",
			"Id of item to locate", true,
			TAPIDoc::TPrimitiveType::spString, TAPIDoc::TPrimitiveFormat::None,
			TAPIDoc::TPrimitiveType::spString, "", ""));
		attributes->AddRequestParameter("GetItem", ResponseParameter.get());

		//POST
		ResponseParameter.reset(new EndPointRequestParameterAttribute
			(TAPIDocParameter::TParameterIn::Body, "body",
			"Item to be created", true, TAPIDoc::TPrimitiveType::spObject,
			TAPIDoc::TPrimitiveFormat::None, TAPIDoc::TPrimitiveType::spObject,
			"", "#/definitions/NoteObject"));
		attributes->AddRequestParameter("Post", ResponseParameter.get());

		//PUT
		ResponseParameter.reset(new EndPointRequestParameterAttribute
			(TAPIDocParameter::TParameterIn::Path, "item",
			"Id of item to be updated", true,
			TAPIDoc::TPrimitiveType::spInteger, TAPIDoc::TPrimitiveFormat::None,
			TAPIDoc::TPrimitiveType::spInteger, "", ""));
		attributes->AddRequestParameter("PutItem", ResponseParameter.get());
		ResponseParameter.reset(new EndPointRequestParameterAttribute
			(TAPIDocParameter::TParameterIn::Body, "body",
			"Contents of item", true,
			TAPIDoc::TPrimitiveType::spObject, TAPIDoc::TPrimitiveFormat::None,
			TAPIDoc::TPrimitiveType::spObject, "", ""));
		attributes->AddRequestParameter("PutItem", ResponseParameter.get());

		//DELETE
		ResponseParameter.reset(new EndPointRequestParameterAttribute
			(TAPIDocParameter::TParameterIn::Path, "item",
			"Id of item to be deleted", true, TAPIDoc::TPrimitiveType::spString,
			TAPIDoc::TPrimitiveFormat::None, TAPIDoc::TPrimitiveType::spString,
			"", ""));
		attributes->AddRequestParameter("DeleteItem", ResponseParameter.get());

		// END ADD PARAMETERS

		// SET RESPONSES
		// GET /notes
		std::unique_ptr<EndPointResponseDetailsAttribute>ResponseDetail
			(new EndPointResponseDetailsAttribute(200, "OK",
			TAPIDoc::TPrimitiveType::spObject, TAPIDoc::TPrimitiveFormat::None,
			"", "#/definitions/NotesListObject"));
		attributes->AddResponseDetail("Get", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(401,
			"Unauthorized", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", "#/definitions/ErrorObject"));
		attributes->AddResponseDetail("Get", ResponseDetail.get());

		// GET /notes/{item}
		ResponseDetail.reset(new EndPointResponseDetailsAttribute(200, "OK",
			TAPIDoc::TPrimitiveType::spObject, TAPIDoc::TPrimitiveFormat::None,
			"", "#/definitions/NoteObject"));
		attributes->AddResponseDetail("GetItem", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(401,
			"Unauthorized", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", "#/definitions/ErrorObject"));
		attributes->AddResponseDetail("GetItem", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(404,
			"Not Found", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", "#/definitions/ErrorObject"));
		attributes->AddResponseDetail("GetItem", ResponseDetail.get());

		// POST /notes
		ResponseDetail.reset(new EndPointResponseDetailsAttribute(200, "OK",
			TAPIDoc::TPrimitiveType::spString, TAPIDoc::TPrimitiveFormat::None,
			"", "#/definitions/IdObject"));
		attributes->AddResponseDetail("Post", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(401,
			"Unauthorized", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", "#/definitions/ErrorObject"));
		attributes->AddResponseDetail("Post", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(409,
			"Item Exist", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", ""));
		attributes->AddResponseDetail("Post", ResponseDetail.get());

		// PUT /notes/{item}
		ResponseDetail.reset(new EndPointResponseDetailsAttribute(200, "OK",
			TAPIDoc::TPrimitiveType::spNull, TAPIDoc::TPrimitiveFormat::None,
			"", ""));
		attributes->AddResponseDetail("PutItem", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(401,
			"Unauthorized", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", "#/definitions/ErrorObject"));
		attributes->AddResponseDetail("PutItem", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(404,
			"Not Found", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", ""));
		attributes->AddResponseDetail("PutItem", ResponseDetail.get());

		// DELETE /notes/{item}
		ResponseDetail.reset(new EndPointResponseDetailsAttribute(200, "OK",
			TAPIDoc::TPrimitiveType::spNull, TAPIDoc::TPrimitiveFormat::None,
			"", ""));
		attributes->AddResponseDetail("DeleteItem", ResponseDetail.get());

		ResponseDetail.reset(new EndPointResponseDetailsAttribute(401,
			"Unauthorized", TAPIDoc::TPrimitiveType::spNull,
			TAPIDoc::TPrimitiveFormat::None, "", "#/definitions/ErrorObject"));
		attributes->AddResponseDetail("DeleteItem", ResponseDetail.get());

		// END SET RESPONSES
		RegisterResource(__typeinfo(TNotesResource1), attributes.release());
	}
}

