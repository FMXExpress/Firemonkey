object DM: TDM
  OldCreateOrder = False
  Height = 150
  Width = 215
  object DataSnapConn: TSQLConnection
    DriverName = 'DATASNAP'
    LoginPrompt = False
    Params.Strings = (
      'drivername=DATASNAP'
      'port=211'
      'CommunicationProtocol=tcp/ip'
      'HostName=localhost'
      'DSAuthenticationUser=admin'
      'DSAuthenticationPassword=admin')
    Left = 72
    Top = 40
  end
end
