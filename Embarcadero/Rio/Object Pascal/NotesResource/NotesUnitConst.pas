//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit NotesUnitConst;

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
  '         type: string' +  sLineBreak +
  '# ' + sLineBreak +
  ' NoteObject:' + sLineBreak +
  '    type: object' + sLineBreak +
  '    properties:' + sLineBreak +
  '      title:' + sLineBreak +
  '        type: string' + sLineBreak +
  '      content:' + sLineBreak +
  '        type: string' + sLineBreak +
  '      id:' + sLineBreak +
  '        type: string' + sLineBreak +
  '# ' + sLineBreak +
  ' NotesListObject:' + sLineBreak +
  '    type: array' + sLineBreak +
  '    items:' + sLineBreak +
  '      $ref: "#/definitions/NoteObject"' +  sLineBreak +
  '# ' + sLineBreak +
  ' IdObject:' + sLineBreak +
  '    type: object' + sLineBreak +
  '    properties:' + sLineBreak +
  '      id:' + sLineBreak +
  '        type: string' + sLineBreak +
  '# ' + sLineBreak +
  ' ErrorObject:' + sLineBreak +
  '    type: object' + sLineBreak +
  '    properties:' + sLineBreak +
  '      error:' + sLineBreak +
  '        type: string' + sLineBreak +
  '      description:' + sLineBreak +
  '        type: string' + sLineBreak +
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
  '    },' +  sLineBreak +
  '    "NoteObject": {' + sLineBreak +
  '        "type": "object",' + sLineBreak +
  '        "properties": {' + sLineBreak +
  '            "title": {' + sLineBreak +
  '                "type": "string"' + sLineBreak +
  '            },' + sLineBreak +
  '            "content": {' + sLineBreak +
  '                "type": "string"' + sLineBreak +
  '            },' + sLineBreak +
  '            "id": {' + sLineBreak +
  '                "type": "string"' + sLineBreak +
  '            }' + sLineBreak +
  '        }' + sLineBreak +
  '    },' + sLineBreak +
  '    "NotesListObject": {' + sLineBreak +
  '        "type": "array",' + sLineBreak +
  '        "items": {' + sLineBreak +
  '            "$ref": "#/definitions/NoteObject"' +  sLineBreak +
  '        }' + sLineBreak +
  '    },' + sLineBreak +
  '    "IdObject": {' + sLineBreak +
  '        "type": "object",' + sLineBreak +
  '        "properties": {' + sLineBreak +
  '            "id": {' + sLineBreak +
  '                "type": "string"' + sLineBreak +
  '            }' + sLineBreak +
  '        }' + sLineBreak +
  '    },' + sLineBreak +
  '    "ErrorObject": {' + sLineBreak +
  '        "type": "object",' + sLineBreak +
  '        "properties": {' + sLineBreak +
  '            "error": {' + sLineBreak +
  '                "type": "string"' + sLineBreak +
  '            },' + sLineBreak +
  '            "description": {' + sLineBreak +
  '                "type": "string"' + sLineBreak +
  '            }' + sLineBreak +
  '        }' + sLIneBreak +
  '    }' + sLineBreak +
  '}' +
  '';

implementation


end.
