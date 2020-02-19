object DownloadServiceDM: TDownloadServiceDM
  OldCreateOrder = False
  OnCreate = AndroidServiceCreate
  OnDestroy = AndroidServiceDestroy
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
  object NotificationCenter1: TNotificationCenter
    Left = 64
    Top = 40
  end
end
