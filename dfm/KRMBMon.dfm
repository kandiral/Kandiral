object KRMBMonForm: TKRMBMonForm
  Left = 0
  Top = 0
  Caption = 'KRMBMonForm'
  ClientHeight = 291
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 253
    Width = 464
    Height = 38
    Align = alBottom
    Caption = 'Panel3'
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      464
      38)
    object btnSaveLog: TButton
      Left = 377
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save log'
      TabOrder = 0
      OnClick = btnSaveLogClick
    end
    object Button1: TButton
      Left = 296
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 61
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object cbAutoScrolling: TCheckBox
      Left = 17
      Top = 31
      Width = 97
      Height = 17
      Caption = 'AutoScrolling'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbAutoScrollingClick
    end
    object chOnOff: TCheckBox
      Left = 17
      Top = 8
      Width = 97
      Height = 17
      Caption = 'On/Off'
      TabOrder = 1
      OnClick = chOnOffClick
    end
  end
  object lbLogs: TListBox
    Left = 0
    Top = 61
    Width = 464
    Height = 192
    Align = alClient
    ItemHeight = 13
    TabOrder = 2
  end
  object SaveDialog1: TSaveDialog
    Left = 32
    Top = 104
  end
end
