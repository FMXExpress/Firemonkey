object BeaconServiceDM: TBeaconServiceDM
  OldCreateOrder = False
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
  object Beacon1: TBeacon
    ModeExtended = [iBeacons, AltBeacons]
    MonitorizedRegions = <
      item
        UUID = '{87654321-CF6D-4A0F-ADF2-F4911BA9FFA6}'
        EddyNamespace = '00000000000000000000'
        EddyInstance = '000000000000'
        IDManufacturer = '-1'
      end>
    BeaconDeathTime = 30
    SPC = 0.500000000000000000
    ScanningTime = 10000
    ScanningSleepingTime = 0
    OnBeaconEnter = Beacon1BeaconEnter
    OnBeaconProximity = Beacon1BeaconProximity
    Left = 88
    Top = 40
  end
  object NotificationCenter1: TNotificationCenter
    Left = 168
    Top = 40
  end
end
