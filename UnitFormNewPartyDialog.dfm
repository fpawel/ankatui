object FormNewPartyDialog: TFormNewPartyDialog
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'FormNewPartyDialog'
  ClientHeight = 424
  ClientWidth = 953
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 19
  inline TFrameSettings1: TFrameSettings
    Left = 0
    Top = 0
    Width = 721
    Height = 424
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 731
    ExplicitHeight = 434
    inherited Panel1: TPanel
      Width = 721
      ExplicitWidth = 731
    end
    inherited Panel2: TPanel
      Top = 419
      Width = 721
      ExplicitTop = 429
      ExplicitWidth = 731
    end
    inherited Panel4: TPanel
      Left = 716
      Height = 414
      ExplicitLeft = 726
      ExplicitHeight = 424
    end
    inherited Panel7: TPanel
      Height = 414
      ExplicitHeight = 424
    end
    inherited CategoryPanelGroup1: TCategoryPanelGroup
      Width = 711
      Height = 414
      ExplicitTop = 5
      ExplicitWidth = 721
      ExplicitHeight = 424
    end
  end
  object Panel2: TPanel
    Left = 721
    Top = 0
    Width = 232
    Height = 424
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 731
    ExplicitHeight = 434
    object Label1: TLabel
      Left = 6
      Top = 202
      Width = 211
      Height = 71
      AutoSize = False
      Caption = #1044#1091#1073#1083#1080#1088#1086#1074#1072#1085#1080#1077' '#1089#1077#1088#1080#1081#1085#1086#1075#1086' '#1085#1086#1084#1077#1088#1072
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Button1: TButton
      Left = 6
      Top = 13
      Width = 213
      Height = 44
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1087#1072#1088#1090#1080#1102
      Enabled = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 6
      Top = 102
      Width = 211
      Height = 35
      Caption = '+1 '#1087#1088#1080#1073#1086#1088
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 6
      Top = 143
      Width = 211
      Height = 34
      Caption = '-1 '#1087#1088#1080#1073#1086#1088
      TabOrder = 2
      OnClick = Button3Click
    end
  end
end
