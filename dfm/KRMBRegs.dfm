object KRMBRegsForm: TKRMBRegsForm
  Left = 0
  Top = 0
  Caption = 'MBRegs'
  ClientHeight = 299
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 258
    Width = 673
    Height = 41
    Align = alBottom
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      673
      41)
    object cbTimerInterval: TKRBLComboBox
      Left = 587
      Top = 8
      Width = 73
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemIndex = 3
      TabOrder = 0
      Text = '1000'
      Items.Strings = (
        '100'
        '250'
        '500'
        '1000'
        '2000'
        '5000'
        '')
      BLabel.Width = 134
      BLabel.Height = 13
      BLabel.AutoSize = True
      BLabel.Spacing = 6
      BLabel.Caption = #1055#1077#1088#1080#1086#1076' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' '#1087#1086#1083#1077#1081
    end
    object cbRegs: TKRComboBox
      Left = 8
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = -1
      TabOrder = 1
    end
    object btnAdd: TButton
      Left = 159
      Top = 6
      Width = 75
      Height = 25
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      TabOrder = 2
      OnClick = btnAddClick
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 673
    Height = 258
    Align = alClient
    TabOrder = 1
  end
  object KRTimer1: TKRTimer
    OnTimer = KRTimer1Timer
    Left = 48
    Top = 40
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2
    OnTimer = Timer1Timer
    Left = 192
    Top = 40
  end
end
