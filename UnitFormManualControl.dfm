object FormManualControl: TFormManualControl
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1056#1091#1095#1085#1086#1077' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1077
  ClientHeight = 395
  ClientWidth = 535
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 19
  object GroupBox1: TGroupBox
    Left = 17
    Top = 16
    Width = 481
    Height = 97
    Caption = #1054#1090#1087#1088#1072#1074#1082#1072' '#1082#1086#1084#1072#1085#1076#1099
    TabOrder = 0
    object Edit1: TEdit
      Left = 17
      Top = 56
      Width = 238
      Height = 27
      TabOrder = 0
      Text = '0'
      OnChange = Edit1Change
    end
    object ComboBox2: TComboBox
      Left = 17
      Top = 23
      Width = 238
      Height = 27
      TabOrder = 1
      OnChange = ComboBox2Change
    end
    object Button1: TButton
      Left = 261
      Top = 23
      Width = 97
      Height = 27
      Caption = #1050#1086#1084#1072#1085#1076#1072
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button6: TButton
      Left = 261
      Top = 56
      Width = 97
      Height = 27
      Caption = #1056#1077#1078#1080#1084
      TabOrder = 3
      OnClick = Button6Click
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 17
    Top = 119
    Width = 481
    Height = 122
    Caption = #1043#1072#1079#1086#1074#1099#1081' '#1073#1083#1086#1082
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      #1042#1099#1082#1083'.'
      #1050#1083#1072#1087#1072#1085' 1'
      #1050#1083#1072#1087#1072#1085' 2'
      #1050#1083#1072#1087#1072#1085' 3'
      #1050#1083#1072#1087#1072#1085' 4'
      #1050#1083#1072#1087#1072#1085' 5'
      #1050#1083#1072#1087#1072#1085' 6')
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 17
    Top = 255
    Width = 481
    Height = 112
    Caption = #1058#1077#1088#1084#1086#1082#1072#1084#1077#1088#1072
    TabOrder = 2
    object Label2: TLabel
      Left = 17
      Top = 67
      Width = 8
      Height = 19
      Caption = '?'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Button2: TButton
      Left = 201
      Top = 67
      Width = 115
      Height = 27
      Caption = #1057#1090#1072#1088#1090
      TabOrder = 0
    end
    object Button3: TButton
      Left = 201
      Top = 34
      Width = 115
      Height = 27
      Caption = #1057#1090#1086#1087
      TabOrder = 1
    end
    object Button4: TButton
      Left = 17
      Top = 34
      Width = 176
      Height = 27
      Caption = #1057#1095#1080#1090#1072#1090#1100' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1091
      TabOrder = 2
    end
    object Button5: TButton
      Left = 322
      Top = 34
      Width = 117
      Height = 27
      Caption = #1059#1089#1090#1072#1074#1082#1072
      TabOrder = 3
    end
    object Edit2: TEdit
      Left = 322
      Top = 67
      Width = 117
      Height = 27
      TabOrder = 4
      Text = '0'
    end
  end
end
