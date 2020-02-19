//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit APIDocDelphiAttributesUnitConsts;

interface

resourcestring

  sError = 'Error';
  sErrorMessage = 'errormessage';
  sDescription = 'description';
  sNotFound = 'Not Found';
  sUnExpectedToken ='Unexpected Token, expected string';
  sPathItem = 'PathItem';
  sPostedData = 'PostedData';

const
  cYamlDefinitions =
  '# ' +  sLineBreak +
  ' ItemObject:' +  sLineBreak +
  '    type: object' +  sLineBreak +
  '    properties:' +  sLineBreak +
  '      RowID:' +  sLineBreak +
  '        type: integer' +  sLineBreak +
  '      Original:' +  sLineBreak +
  '        type: object' +  sLineBreak+
  '        properties:' +  sLineBreak +
  '         EMP_NO:' +  sLineBreak +
  '          type: integer' +  sLineBreak +
  '         FIRST_NAME:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '         LAST_NAME:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '         PHONE_EXT:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '         HIRE_DATE:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '         DEPT_NO:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '         JOB_CODE:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '         JOB_GRADE:' +  sLineBreak +
  '          type: integer' +  sLineBreak +
  '         JOB_COUNTRY:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '         SALARY:' +  sLineBreak +
  '          type: integer' +  sLineBreak +
  '         FULL_NAME:' +  sLineBreak +
  '          type: string' +  sLineBreak +
  '# ' +  sLineBreak +
  ' TableObject:' +  sLineBreak +
  '    type: object' +  sLineBreak +
  '    properties:' +  sLineBreak +
  '      class:' +  sLineBreak +
  '        type: string' +  sLineBreak +
  '      Name:' +  sLineBreak +
  '        type: string' +  sLineBreak +
  '      SourceName:' +  sLineBreak +
  '        type: string' +  sLineBreak +
  '      SourceID:' +  sLineBreak +
  '        type: integer' +  sLineBreak +
  '      RowList:' +  sLineBreak +
  '        type: array' +  sLineBreak +
  '        items: ' +  sLineBreak +
  '         $ref: "#/definitions/ItemObject"' +  sLineBreak +
  '# ' +  sLineBreak +
  ' EmployeeTable:' +  sLineBreak +
  '    type: object' +  sLineBreak +
  '    properties:' +  sLineBreak +
  '      FBDS:' +  sLineBreak +
  '       type: object' +  sLineBreak +
  '       properties:' +  sLineBreak +
  '         Version:' +  sLineBreak +
  '           type: string' +  sLineBreak +
  '         Manager:' +  sLineBreak +
  '           type: object' +  sLineBreak+
  '           properties:' +  sLineBreak +
  '             TableList:' +  sLineBreak +
  '              type: array' +  sLineBreak +
  '              items:' +  sLineBreak +
  '               $ref: "#/definitions/TableObject"' +  sLineBreak +
  '# ' +  sLineBreak +
  ' PostObject:' +  sLineBreak +
  '   properties:' +  sLineBreak +
  '    EMP_NO:' +  sLineBreak +
  '     type: integer' +  sLineBreak +
  '    FIRST_NAME:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    LAST_NAME:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    PHONE_EXT:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    HIRE_DATE:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '     format: date-time' +  sLineBreak +
  '    DEPT_NO:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    JOB_CODE:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    JOB_GRADE:' +  sLineBreak +
  '     type: integer' +  sLineBreak +
  '    JOB_COUNTRY:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    SALARY:' +  sLineBreak +
  '     type: integer' +  sLineBreak +
  '    FULL_NAME:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '# ' +  sLineBreak +
  ' PutObject:' +  sLineBreak +
  '   properties:' +  sLineBreak +
  '    EMP_NO:' +  sLineBreak +
  '     type: integer' +  sLineBreak +
  '    FIRST_NAME:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    LAST_NAME:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    PHONE_EXT:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    HIRE_DATE:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '     format: date-time' +  sLineBreak +
  '    DEPT_NO:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    JOB_CODE:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    JOB_GRADE:' +  sLineBreak +
  '     type: integer' +  sLineBreak +
  '    JOB_COUNTRY:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '    SALARY:' +  sLineBreak +
  '     type: integer' +  sLineBreak +
  '    FULL_NAME:' +  sLineBreak +
  '     type: string' +  sLineBreak +
  '# ' +  sLineBreak +
  ' ItemPostedResponseObject:' +  sLineBreak +
  '    type: object' +  sLineBreak +
  '    properties:' +  sLineBreak +
  '      PostedData:' +  sLineBreak +
  '        type: array' +  sLineBreak +
  '        items:' +  sLineBreak +
  '         type: string' +  sLineBreak+
  '# ' +  sLineBreak +
  ' ItemPutResponseObject:' +  sLineBreak +
  '    type: object' +  sLineBreak +
  '    properties:' +  sLineBreak +
  '      PathItem:' +  sLineBreak +
  '        type: string' +  sLineBreak+
  '      PostedData:' +  sLineBreak +
  '        type: array' +  sLineBreak +
  '        items:' +  sLineBreak +
  '         type: string' +  sLineBreak+
  '';

cJSONDefinitions =
  '{' +  sLineBreak +
  '    "ItemObject": {' +  sLineBreak +
  '        "type": "object",' +  sLineBreak +
  '        "properties": {' +  sLineBreak +
  '            "RowID": {' +  sLineBreak +
  '                "type": "integer"' +  sLineBreak +
  '            },' +  sLineBreak +
  '            "Original": {' +  sLineBreak +
  '                "type": "object",' +  sLineBreak +
  '                "properties": {' +  sLineBreak +
  '                    "EMP_NO": {' +  sLineBreak +
  '                        "type": "integer"' +  sLineBreak +
  '                    },' +  sLineBreak +
  '                    "FIRST_NAME": {' +  sLineBreak +
  '                        "type": "string"' +  sLineBreak +
  '                    },' +  sLineBreak +
  '                    "LAST_NAME": {' +  sLineBreak +
  '                       "type": "string"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "PHONE_EXT": {' +  sLineBreak +
  '                       "type": "string"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "HIRE_DATE": {' +  sLineBreak +
  '                       "type": "string"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "DEPT_NO": {' +  sLineBreak +
  '                       "type": "string"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "JOB_CODE": {' +  sLineBreak +
  '                       "type": "string"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "JOB_GRADE": {' +  sLineBreak +
  '                       "type": "integer"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "JOB_COUNTRY": {' +  sLineBreak +
  '                       "type": "string"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "SALARY": {' +  sLineBreak +
  '                       "type": "integer"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "FULL_NAME": {' +  sLineBreak +
  '                      "type": "string"' +  sLineBreak +
  '                   }' +  sLineBreak +
  '               }' +  sLineBreak +
  '          }' +  sLineBreak +
  '       }' +  sLineBreak +
  '   },' +  sLineBreak +
  '   "TableObject": {' +  sLineBreak +
  '       "type": "object",' +  sLineBreak +
  '       "properties": {' +  sLineBreak +
  '           "class": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "Name": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "SourceName": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "SourceID": {' +  sLineBreak +
  '               "type": "integer"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "RowList": {' +  sLineBreak +
  '               "type": "array",' +  sLineBreak +
  '               "items": {' +  sLineBreak +
  '                   "$ref": "#/definitions/ItemObject"' +  sLineBreak +
  '               }' +  sLineBreak +
  '           }' +  sLineBreak +
  '      }' +  sLineBreak +
  '   },' +  sLineBreak +
  '   "EmployeeTable": {' +  sLineBreak +
  '       "type": "object",' +  sLineBreak +
  '       "properties": {' +  sLineBreak +
  '          "FBDS": {' +  sLineBreak +
  '               "type": "object",' +  sLineBreak +
  '               "properties": {' +  sLineBreak +
  '                   "Version": {' +  sLineBreak +
  '                       "type": "string"' +  sLineBreak +
  '                   },' +  sLineBreak +
  '                   "Manager": {' +  sLineBreak +
  '                       "type": "object",' +  sLineBreak +
  '                       "properties": {' +  sLineBreak +
  '                           "TableList": {' +  sLineBreak +
  '                               "type": "array",' +  sLineBreak +
  '                               "items": {' +  sLineBreak +
  '                                   "$ref": "#/definitions/TableObject"' +  sLineBreak +
  '                               }' +  sLineBreak +
  '                           }' +  sLineBreak +
  '                       }' +  sLineBreak +
  '                   }' +  sLineBreak +
  '               }' +  sLineBreak +
  '           }' +  sLineBreak +
  '       }' +  sLineBreak +
  '   },' +  sLineBreak +
  '   "PostObject": {' +  sLineBreak +
  '       "properties": {' +  sLineBreak +
  '           "EMP_NO": {' +  sLineBreak +
  '               "type": "integer"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "FIRST_NAME": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "LAST_NAME": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "PHONE_EXT": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "HIRE_DATE": {' +  sLineBreak +
  '               "type": "string",' +  sLineBreak +
  '               "format": "date-time"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "DEPT_NO": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "JOB_CODE": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "JOB_GRADE": {' +  sLineBreak +
  '               "type": "integer"' +  sLineBreak +
  '           },' +  sLineBreak +
  '            "JOB_COUNTRY": {' +  sLineBreak +
  '                "type": "string"' +  sLineBreak +
  '            },' +  sLineBreak +
  '            "SALARY": {' +  sLineBreak +
  '                "type": "integer"' +  sLineBreak +
  '            },' +  sLineBreak +
  '            "FULL_NAME": {' +  sLineBreak +
  '                "type": "string"' +  sLineBreak +
  '            }' +  sLineBreak +
  '        }' +  sLineBreak +
  '    },' +  sLineBreak +
  '   "PutObject": {' +  sLineBreak +
  '       "properties": {' +  sLineBreak +
  '           "EMP_NO": {' +  sLineBreak +
  '               "type": "integer"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "FIRST_NAME": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "LAST_NAME": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "PHONE_EXT": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "HIRE_DATE": {' +  sLineBreak +
  '               "type": "string",' +  sLineBreak +
  '               "format": "date-time"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "DEPT_NO": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "JOB_CODE": {' +  sLineBreak +
  '               "type": "string"' +  sLineBreak +
  '           },' +  sLineBreak +
  '           "JOB_GRADE": {' +  sLineBreak +
  '               "type": "integer"' +  sLineBreak +
  '           },' +  sLineBreak +
  '            "JOB_COUNTRY": {' +  sLineBreak +
  '                "type": "string"' +  sLineBreak +
  '            },' +  sLineBreak +
  '            "SALARY": {' +  sLineBreak +
  '                "type": "integer"' +  sLineBreak +
  '            },' +  sLineBreak +
  '            "FULL_NAME": {' +  sLineBreak +
  '                "type": "string"' +  sLineBreak +
  '            }' +  sLineBreak +
  '        }' +  sLineBreak +
  '    },' +  sLineBreak +
  '    "ItemPostedResponseObject": {' +  sLineBreak +
  '        "type": "object",' +  sLineBreak +
  '        "properties": {' +  sLineBreak +
  '            "PostedData": {' +  sLineBreak +
  '                "type": "array",' +  sLineBreak +
  '                "items": {' +  sLineBreak +
  '                    "type": "string"' +  sLineBreak +
  '                }' +  sLineBreak +
  '            }' +  sLineBreak +
  '        }' +  sLineBreak +
  '    },' +  sLineBreak +
  '    "ItemPutResponseObject": {' +  sLineBreak +
  '        "type": "object",' +  sLineBreak +
  '        "properties": {' +  sLineBreak +
  '            "PathItem": {' +  sLineBreak +
  '                "type": "string"' +  sLineBreak +
  '            },' +  sLineBreak +
  '            "PostedData": {' +  sLineBreak +
  '                "type": "array",' +  sLineBreak +
  '                "items": {' +  sLineBreak +
  '                    "type": "string"' +  sLineBreak +
  '                }' +  sLineBreak +
  '            }' +  sLineBreak +
  '        }' +  sLineBreak +
  '    }' +  sLineBreak +
  '}' +
  '';

implementation


end.
