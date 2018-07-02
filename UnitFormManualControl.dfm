object FormManualControl: TFormManualControl
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1056#1091#1095#1085#1086#1077' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1077
  ClientHeight = 505
  ClientWidth = 291
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 19
  object Label2: TLabel
    Left = 124
    Top = 32
    Width = 38
    Height = 19
    Caption = '20"'#1057
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 291
    Height = 97
    Align = alTop
    Caption = #1054#1090#1087#1088#1072#1074#1082#1072' '#1082#1086#1084#1072#1085#1076#1099
    TabOrder = 0
    object Edit1: TEdit
      Left = 17
      Top = 56
      Width = 135
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
      Left = 158
      Top = 56
      Width = 97
      Height = 27
      Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100
      TabOrder = 2
      OnClick = Button1Click
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 0
    Top = 161
    Width = 291
    Height = 138
    Align = alTop
    Caption = #1043#1072#1079#1086#1074#1099#1081' '#1073#1083#1086#1082
    Columns = 2
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
    ExplicitTop = 97
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 299
    Width = 291
    Height = 206
    Align = alClient
    Caption = #1058#1077#1088#1084#1086#1082#1072#1084#1077#1088#1072
    TabOrder = 2
    ExplicitTop = 235
    ExplicitHeight = 278
    object Label1: TLabel
      Left = 17
      Top = 32
      Width = 101
      Height = 19
      Caption = #1058#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072':'
    end
    object Label3: TLabel
      Left = 17
      Top = 160
      Width = 62
      Height = 19
      Caption = #1059#1089#1090#1072#1074#1082#1072':'
    end
    object Button2: TButton
      Left = 138
      Top = 58
      Width = 115
      Height = 27
      Caption = #1057#1090#1072#1088#1090
      TabOrder = 0
    end
    object Button3: TButton
      Left = 17
      Top = 58
      Width = 115
      Height = 27
      Caption = #1057#1090#1086#1087
      TabOrder = 1
    end
    object Button4: TButton
      Left = 17
      Top = 91
      Width = 236
      Height = 27
      Caption = #1057#1095#1080#1090#1072#1090#1100' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1091
      TabOrder = 2
    end
    object Button5: TButton
      Left = 17
      Top = 124
      Width = 236
      Height = 27
      Caption = #1047#1072#1076#1072#1090#1100' '#1091#1089#1090#1072#1074#1082#1091
      TabOrder = 3
    end
    object Edit2: TEdit
      Left = 85
      Top = 157
      Width = 168
      Height = 27
      TabOrder = 4
      Text = '0'
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 97
    Width = 291
    Height = 64
    Align = alTop
    Caption = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1088#1077#1078#1080#1084#1072' '#1088#1072#1073#1086#1090#1099' '#1040#1053#1050#1040#1058
    TabOrder = 3
    object Edit3: TEdit
      Left = 17
      Top = 24
      Width = 135
      Height = 27
      TabOrder = 0
      Text = '0'
      OnChange = Edit3Change
    end
    object Button6: TButton
      Left = 158
      Top = 23
      Width = 97
      Height = 27
      Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100
      TabOrder = 1
      OnClick = Button6Click
    end
  end
end
