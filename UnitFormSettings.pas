unit UnitFormSettings;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PropertiesFormUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
    TFormSettings = class(TForm)
    procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
        FPropertiesForm : TPropertiesForm;

    public
        { Public declarations }
        procedure SetConfig;
        procedure CancelEditNode;
    end;

var
    FormSettings: TFormSettings;

implementation

{$R *.dfm}

uses UnitData, config;


procedure TFormSettings.FormCreate(Sender: TObject);
begin

    FPropertiesForm := TPropertiesForm.Create(self);
    with FPropertiesForm do
    begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
        SetPropertyValueChanged(procedure(p:TConfigProperty)
        begin
            if p.FError = '' then
                DataModule1.UpdateConfig(p);
        end);
    end;
    SetConfig;

end;

procedure TFormSettings.SetConfig;
begin
    FPropertiesForm.VST3.CancelEditNode;
    FPropertiesForm.SetConfig(DataModule1.GetConfig);
end;

procedure TFormSettings.CancelEditNode;
begin
    FPropertiesForm.VST3.CancelEditNode;
end;

end.
