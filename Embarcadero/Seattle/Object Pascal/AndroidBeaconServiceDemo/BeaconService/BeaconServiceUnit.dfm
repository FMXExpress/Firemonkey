object BeaconServiceDM: TBeaconServiceDM
  OldCreateOrder = False
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
  object Beacon1: TBeacon
    MonitorizedRegions = <
      item
        UUID = '{699EBC80-E1F3-11E3-9A0F-0CF3EE3BC012}'
      end>
    BeaconDeathTime = 10
    SPC = 0.500000000000000000
    ScanningSleepingTime = 2000
    OnBeaconProximity = Beacon1BeaconProximity
    Left = 88
    Top = 40
  end
  object NotificationCenter1: TNotificationCenter
    Left = 168
    Top = 40
  end
end
