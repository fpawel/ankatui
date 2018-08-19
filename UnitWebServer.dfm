object WebServer: TWebServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 371
  Width = 592
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 56
    Top = 40
  end
end
