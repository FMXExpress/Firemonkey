object ServerMethods1: TServerMethods1
  OldCreateOrder = False
  Height = 337
  Width = 497
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=IB'
      
        'Database=C:\Users\Public\Documents\Embarcadero\Studio\15.0\Sampl' +
        'es\Data\EMPLOYEE.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'Port=3050')
    LoginPrompt = False
    Left = 56
    Top = 16
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 56
    Top = 72
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 56
    Top = 128
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 56
    Top = 184
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 56
    Top = 240
  end
  object FDQueryDepartmentNames: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select dept_no, department from department')
    Left = 232
    Top = 16
  end
  object FDQueryDepartment: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from department where DEPT_NO =:DEPT')
    Left = 232
    Top = 72
    ParamData = <
      item
        Name = 'DEPT'
        DataType = ftString
        ParamType = ptInput
      end>
  end
  object FDQueryDepartmentEmployees: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from employee where dept_no = :DEPT')
    Left = 232
    Top = 128
    ParamData = <
      item
        Name = 'DEPT'
        DataType = ftString
        ParamType = ptInput
      end>
  end
end
